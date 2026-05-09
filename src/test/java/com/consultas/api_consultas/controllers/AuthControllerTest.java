package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.consultas.api_consultas.security.AuthenticationService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@ActiveProfiles("test")
class AuthControllerTest {

    @Autowired
    private WebApplicationContext context;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private AuthenticationManager authenticationManager;

    @MockBean
    private AuthenticationService authenticationService;

    private MockMvc mvc() {
        return MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
    }

    private String corpoLogin(String username) throws Exception {
        return objectMapper.writeValueAsString(new LoginDTO(username, "senha-qualquer"));
    }

    @Test
    @DisplayName("Deve retornar 200 e token quando credenciais são válidas")
    void deveRetornar200ComTokenQuandoCredenciaisValidas() throws Exception {
        Authentication auth = new UsernamePasswordAuthenticationToken(
                "usuario_ok",
                "senha-qualquer",
                List.of(new SimpleGrantedAuthority("ROLE_ADMIN"))
        );
        when(authenticationManager.authenticate(any())).thenReturn(auth);
        when(authenticationService.authenticate(any())).thenReturn("token-jwt-fake");

        mvc().perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoLogin("usuario_ok")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").value("token-jwt-fake"));
    }

    @Test
    @DisplayName("Deve retornar 401 quando credenciais são inválidas")
    void deveRetornar401QuandoCredenciaisInvalidas() throws Exception {
        when(authenticationManager.authenticate(any()))
                .thenThrow(new BadCredentialsException("Credenciais inválidas"));

        mvc().perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoLogin("usuario_invalido")))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("Credenciais inválidas"));
    }

    @Test
    @DisplayName("Deve retornar 429 com Retry-After ao estourar o rate limit")
    void deveRetornar429AoEstourarRateLimit() throws Exception {
        when(authenticationManager.authenticate(any()))
                .thenThrow(new BadCredentialsException("Credenciais inválidas"));

        MockMvc mvc = mvc();
        String body = corpoLogin("usuario_brute");

        // Esgota a capacidade configurada em application-test.yml (3 tentativas)
        for (int i = 0; i < 3; i++) {
            mvc.perform(post("/auth/login")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(body))
                    .andExpect(status().isUnauthorized());
        }

        // 4ª chamada deve ser bloqueada antes de chegar no AuthenticationManager
        mvc.perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isTooManyRequests())
                .andExpect(header().exists("Retry-After"))
                .andExpect(header().string("Retry-After", "60"))
                .andExpect(jsonPath("$.message").value(
                        org.hamcrest.Matchers.containsString("Muitas tentativas de login")
                ));
    }
}
