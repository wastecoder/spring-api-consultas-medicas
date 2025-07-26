package com.consultas.api_consultas.configs;

import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.handlers.CustomAccessDeniedHandler;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.source.ImmutableJWKSet;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.JwtEncoder;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.oauth2.jwt.NimbusJwtEncoder;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationConverter;
import org.springframework.security.oauth2.server.resource.authentication.JwtGrantedAuthoritiesConverter;
import org.springframework.security.web.SecurityFilterChain;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyFactory;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http, CustomAccessDeniedHandler accessDeniedHandler) throws Exception {
        http
                .csrf(AbstractHttpConfigurer::disable) // Usado em cookies
                .authorizeHttpRequests(auth -> auth
                        // TODOS: Swagger
                        .requestMatchers(
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-ui.html",
                                "/webjars/**",
                                "/auth/login"
                        ).permitAll()

                        // MEDICO:
                        .requestMatchers("/medicos/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // PACIENTE:
                        .requestMatchers(HttpMethod.GET, "/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers(HttpMethod.PUT, "/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers("/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // CONSULTA:
                        .requestMatchers(HttpMethod.GET, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "MEDICO", "PACIENTE")
                        .requestMatchers(HttpMethod.POST, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers(HttpMethod.PUT, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers(HttpMethod.DELETE, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // RELATÓRIOS: médico, consulta, financeiro
                        .requestMatchers("/relatorios/medico/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers("/relatorios/consulta/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers("/relatorios/financeiro/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // FALLBACK: ADMIN e/ou RECEPCIONISTA
                        .anyRequest().hasAnyRole("ADMIN", "RECEPCIONISTA")
                )
                .exceptionHandling(ex -> ex
                        .accessDeniedHandler(accessDeniedHandler)
                )
                .oauth2ResourceServer(conf -> conf
                        .jwt(jwt -> jwt
                                .jwtAuthenticationConverter(jwtAuthenticationConverter())
                        )
                );

        return http.build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
        return config.getAuthenticationManager();
    }

    @Bean
    public JwtDecoder jwtDecoder() throws Exception {
        RSAPublicKey publicKey = readPublicKey("jwt-public.key");
        return NimbusJwtDecoder.withPublicKey(publicKey).build();
    }

    @Bean
    public JwtEncoder jwtEncoder() throws Exception {
        RSAPublicKey publicKey = readPublicKey("jwt-public.key");
        RSAPrivateKey privateKey = readPrivateKey("jwt-private.key");

        var jwk = new RSAKey.Builder(publicKey)
                .privateKey(privateKey)
                .build();
        var jwks = new ImmutableJWKSet<>(new JWKSet(jwk));

        return new NimbusJwtEncoder(jwks);
    }

    @Bean
    public JwtAuthenticationConverter jwtAuthenticationConverter() {
        JwtGrantedAuthoritiesConverter grantedAuthoritiesConverter = new JwtGrantedAuthoritiesConverter();
        grantedAuthoritiesConverter.setAuthorityPrefix("");
        grantedAuthoritiesConverter.setAuthoritiesClaimName("scope");

        JwtAuthenticationConverter jwtAuthenticationConverter = new JwtAuthenticationConverter();
        jwtAuthenticationConverter.setJwtGrantedAuthoritiesConverter(grantedAuthoritiesConverter);

        return jwtAuthenticationConverter;
    }

    @Bean
    public CommandLineRunner criarUsuarioInicial(UsuarioRepository repo, PasswordEncoder encoder) {
        return args -> repo
                .findByUsername("admin")
                .orElseGet(() -> repo.save(new Usuario(
                        null,
                        "admin",
                        encoder.encode("123456"),
                        Funcao.ADMIN,
                        true
                )));
    }

    private RSAPublicKey readPublicKey(String path) throws Exception {
        try (InputStream is = new ClassPathResource(path).getInputStream()) {
            String key = new String(is.readAllBytes(), StandardCharsets.UTF_8);
            String cleaned = key
                    .replace("-----BEGIN PUBLIC KEY-----", "")
                    .replace("-----END PUBLIC KEY-----", "")
                    .replaceAll("\\s", "");

            byte[] decoded = java.util.Base64.getDecoder().decode(cleaned);
            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(decoded);
            return (RSAPublicKey) KeyFactory.getInstance("RSA").generatePublic(keySpec);
        }
    }

    private RSAPrivateKey readPrivateKey(String path) throws Exception {
        try (InputStream is = new ClassPathResource(path).getInputStream()) {
            String key = new String(is.readAllBytes(), StandardCharsets.UTF_8);
            String cleaned = key
                    .replace("-----BEGIN PRIVATE KEY-----", "")
                    .replace("-----END PRIVATE KEY-----", "")
                    .replaceAll("\\s", "");

            byte[] decoded = java.util.Base64.getDecoder().decode(cleaned);
            PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(decoded);
            return (RSAPrivateKey) KeyFactory.getInstance("RSA").generatePrivate(keySpec);
        }
    }

}
