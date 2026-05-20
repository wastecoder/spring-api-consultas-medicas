package com.consultas.api_consultas.configs;

import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.handlers.CustomAccessDeniedHandler;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.security.BlacklistedTokenFilter;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.source.ImmutableJWKSet;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.Customizer;
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
import org.springframework.security.oauth2.server.resource.web.authentication.BearerTokenAuthenticationFilter;
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

    @Value("${jwt.private-key-path}")
    private Resource privateKeyResource;

    @Value("${jwt.public-key-path}")
    private Resource publicKeyResource;

    @Value("${admin.username}")
    private String adminUsername;

    @Value("${admin.password}")
    private String adminPassword;

    @Value("${admin.email:admin@consultas-medicas.local}")
    private String adminEmail;

    @Bean
    public SecurityFilterChain securityFilterChain(
            HttpSecurity http,
            CustomAccessDeniedHandler accessDeniedHandler,
            BlacklistedTokenFilter blacklistedTokenFilter
    ) throws Exception {
        http
                .cors(Customizer.withDefaults())
                .csrf(AbstractHttpConfigurer::disable) // Usado em cookies
                .authorizeHttpRequests(auth -> auth
                        // ACTUATOR: health e prometheus publicos; resto restrito a ADMIN
                        .requestMatchers(
                                "/actuator/health",
                                "/actuator/health/**",
                                "/actuator/prometheus"
                        ).permitAll()
                        .requestMatchers("/actuator/**").hasRole("ADMIN")

                        // TODOS: Swagger
                        .requestMatchers(
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-ui.html",
                                "/webjars/**",
                                "/auth/login",
                                "/auth/refresh",
                                "/auth/forgot-password",
                                "/auth/reset-password",
                                "/auth/signup"
                        ).permitAll()

                        // AUTH: logout precisa apenas de autenticação válida
                        .requestMatchers("/auth/logout").authenticated()

                        // MEDICO: o próprio médico pode consultar seu cadastro (GET /medicos/{id});
                        // a verificação de "é o próprio" fica no controller. Demais operações: ADMIN/RECEPCIONISTA.
                        .requestMatchers(HttpMethod.GET, "/medicos/*").hasAnyRole("ADMIN", "RECEPCIONISTA", "MEDICO")
                        .requestMatchers("/medicos/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // PACIENTE:
                        .requestMatchers(HttpMethod.GET, "/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers(HttpMethod.PUT, "/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers("/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // CONSULTA:
                        .requestMatchers(HttpMethod.GET, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "MEDICO", "PACIENTE")
                        .requestMatchers(HttpMethod.POST, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers(HttpMethod.PUT, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers(HttpMethod.PATCH, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "MEDICO")
                        .requestMatchers(HttpMethod.DELETE, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA")

                        // RELATÓRIO DE PACIENTE: paciente pode acessar consultas pessoais
                        .requestMatchers(HttpMethod.GET, "/relatorios/paciente/historico/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")

                        // RELATÓRIOS: ADMIN e/ou RECEPCIONISTA
                        .requestMatchers("/relatorios/medico/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers("/relatorios/paciente/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers("/relatorios/consulta/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers("/relatorios/operacional/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers("/relatorios/produtividade/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
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
                )
                .addFilterAfter(blacklistedTokenFilter, BearerTokenAuthenticationFilter.class);

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
        RSAPublicKey publicKey = readPublicKey(publicKeyResource);
        return NimbusJwtDecoder.withPublicKey(publicKey).build();
    }

    @Bean
    public JwtEncoder jwtEncoder() throws Exception {
        RSAPublicKey publicKey = readPublicKey(publicKeyResource);
        RSAPrivateKey privateKey = readPrivateKey(privateKeyResource);

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
        // O 'sub' carrega o ID do usuário; o nome do principal vem do claim 'username',
        // mantendo Authentication.getName() compatível com o resto do sistema.
        jwtAuthenticationConverter.setPrincipalClaimName("username");

        return jwtAuthenticationConverter;
    }

    @Bean
    public CommandLineRunner criarUsuarioInicial(UsuarioRepository repo, PasswordEncoder encoder) {
        return args -> repo
                .findByUsername(adminUsername)
                .orElseGet(() -> repo.save(Usuario.builder()
                        .username(adminUsername)
                        .email(adminEmail)
                        .senha(encoder.encode(adminPassword))
                        .funcao(Funcao.ADMIN)
                        .ativo(true)
                        .build()));
    }

    private RSAPublicKey readPublicKey(Resource resource) throws Exception {
        try (InputStream is = resource.getInputStream()) {
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

    private RSAPrivateKey readPrivateKey(Resource resource) throws Exception {
        try (InputStream is = resource.getInputStream()) {
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
