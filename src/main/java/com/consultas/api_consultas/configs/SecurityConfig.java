package com.consultas.api_consultas.configs;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .csrf(AbstractHttpConfigurer::disable) //Deixa vulnerável, em produção use token
                .authorizeHttpRequests(auth -> auth
                        // TODOS: Swagger
                        .requestMatchers(
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-ui.html",
                                "/webjars/**"
                        ).permitAll()

                        // MEDICO:
                        .requestMatchers("/medicos/**").hasAnyRole("MEDICO", "RECEPCIONISTA")

                        // PACIENTE:
                        .requestMatchers("/pacientes/**").hasAnyRole("PACIENTE", "RECEPCIONISTA")

                        // RECEPCIONISTA: tudo
                        .anyRequest().hasRole("RECEPCIONISTA")
                )
                .httpBasic(Customizer.withDefaults()); // TODO: remover quando adicionar JWT

        return http.build();
    }

    @Bean
    public UserDetailsService userDetailsService() {
        UserDetails recepcionista = User
                .withUsername("recep")
                .password(passwordEncoder().encode("1234"))
                .roles("RECEPCIONISTA")
                .build();

        UserDetails medico = User
                .withUsername("medico")
                .password(passwordEncoder().encode("1234"))
                .roles("MEDICO")
                .build();

        UserDetails paciente = User
                .withUsername("paciente")
                .password(passwordEncoder().encode("1234"))
                .roles("PACIENTE")
                .build();

        return new InMemoryUserDetailsManager(recepcionista, medico, paciente);
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

}
