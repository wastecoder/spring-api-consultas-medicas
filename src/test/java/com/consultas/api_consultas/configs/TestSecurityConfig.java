package com.consultas.api_consultas.configs;

import com.consultas.api_consultas.handlers.CustomAccessDeniedHandler;
import com.consultas.api_consultas.security.TokenBlacklistService;
import org.mockito.Mockito;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.SecurityFilterChain;

// Espelha as regras de SecurityConfig#securityFilterChain mas sem JwtDecoder/JwtEncoder/RSA keys.
// Usa httpBasic apenas para que requisições não autenticadas devolvam 401 nos slices @WebMvcTest.
// A autenticação real nos testes vem de @WithMockUser, que injeta direto no SecurityContext.
@TestConfiguration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class TestSecurityConfig {

    @Bean
    public CustomAccessDeniedHandler customAccessDeniedHandler() {
        return new CustomAccessDeniedHandler();
    }

    // O BlacklistedTokenFilter (registrado automaticamente como filtro Servlet)
    // exige TokenBlacklistService. Em @WebMvcTest os @Service não são scaneados,
    // então fornecemos um stub Mockito para satisfazer a injeção.
    @Bean
    public TokenBlacklistService tokenBlacklistService() {
        return Mockito.mock(TokenBlacklistService.class);
    }

    @Bean
    public SecurityFilterChain testSecurityFilterChain(
            HttpSecurity http,
            CustomAccessDeniedHandler accessDeniedHandler
    ) throws Exception {
        http
                .csrf(AbstractHttpConfigurer::disable)
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-ui.html",
                                "/webjars/**",
                                "/auth/login"
                        ).permitAll()
                        .requestMatchers("/medicos/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers(HttpMethod.GET, "/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers(HttpMethod.PUT, "/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers("/pacientes/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers(HttpMethod.GET, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "MEDICO", "PACIENTE")
                        .requestMatchers(HttpMethod.POST, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers(HttpMethod.PUT, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers(HttpMethod.DELETE, "/consultas/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .requestMatchers(HttpMethod.GET, "/relatorios/paciente/historico/**").hasAnyRole("ADMIN", "RECEPCIONISTA", "PACIENTE")
                        .requestMatchers("/relatorios/**").hasAnyRole("ADMIN", "RECEPCIONISTA")
                        .anyRequest().hasAnyRole("ADMIN", "RECEPCIONISTA")
                )
                .exceptionHandling(ex -> ex.accessDeniedHandler(accessDeniedHandler))
                .httpBasic(Customizer.withDefaults());

        return http.build();
    }
}
