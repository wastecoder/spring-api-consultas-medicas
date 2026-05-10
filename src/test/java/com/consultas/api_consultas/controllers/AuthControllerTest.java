package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.consultas.api_consultas.dtos.requisicoes.RefreshTokenRequestDTO;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;
import com.consultas.api_consultas.exceptions.RefreshTokenInvalidoException;
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
import static org.mockito.ArgumentMatchers.eq;
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

    private String corpoRefresh(String refreshToken) throws Exception {
        return objectMapper.writeValueAsString(new RefreshTokenRequestDTO(refreshToken));
    }

    @Test
    @DisplayName("Deve retornar 200 e par de tokens quando credenciais são válidas")
    void deveRetornar200ComParDeTokensQuandoCredenciaisValidas() throws Exception {
        Authentication auth = new UsernamePasswordAuthenticationToken(
                "usuario_ok",
                "senha-qualquer",
                List.of(new SimpleGrantedAuthority("ROLE_ADMIN"))
        );
        AuthTokenDTO tokens = new AuthTokenDTO(
                "access-token-fake",
                "refresh-token-fake",
                AppConstants.JWT_EXPIRACAO_SEGUNDOS,
                "Bearer"
        );
        when(authenticationManager.authenticate(any())).thenReturn(auth);
        when(authenticationService.authenticate(any())).thenReturn(tokens);

        mvc().perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoLogin("usuario_ok")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accessToken").value("access-token-fake"))
                .andExpect(jsonPath("$.refreshToken").value("refresh-token-fake"))
                .andExpect(jsonPath("$.expiresIn").value(AppConstants.JWT_EXPIRACAO_SEGUNDOS))
                .andExpect(jsonPath("$.tokenType").value("Bearer"));
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

    @Test
    @DisplayName("Deve retornar 200 e novo par de tokens quando refresh é válido")
    void deveRetornar200ComNovoParQuandoRefreshValido() throws Exception {
        AuthTokenDTO novosTokens = new AuthTokenDTO(
                "novo-access-token",
                "novo-refresh-token",
                AppConstants.JWT_EXPIRACAO_SEGUNDOS,
                "Bearer"
        );
        when(authenticationService.refresh(eq("refresh-valido"))).thenReturn(novosTokens);

        mvc().perform(post("/auth/refresh")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoRefresh("refresh-valido")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accessToken").value("novo-access-token"))
                .andExpect(jsonPath("$.refreshToken").value("novo-refresh-token"))
                .andExpect(jsonPath("$.tokenType").value("Bearer"));
    }

    @Test
    @DisplayName("Deve retornar 401 quando refresh token é inválido")
    void deveRetornar401QuandoRefreshInvalido() throws Exception {
        when(authenticationService.refresh(eq("refresh-invalido")))
                .thenThrow(new RefreshTokenInvalidoException("Refresh token inválido."));

        mvc().perform(post("/auth/refresh")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoRefresh("refresh-invalido")))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("Refresh token inválido."));
    }

    @Test
    @DisplayName("Deve retornar 400 quando refresh token está em branco")
    void deveRetornar400QuandoRefreshEmBranco() throws Exception {
        mvc().perform(post("/auth/refresh")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoRefresh("")))
                .andExpect(status().isBadRequest());
    }
}
