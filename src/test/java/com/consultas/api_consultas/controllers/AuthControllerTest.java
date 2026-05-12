package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.requisicoes.ForgotPasswordDto;
import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.consultas.api_consultas.dtos.requisicoes.RefreshTokenRequestDTO;
import com.consultas.api_consultas.dtos.requisicoes.ResetPasswordDto;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;
import com.consultas.api_consultas.exceptions.PasswordResetTokenInvalidoException;
import com.consultas.api_consultas.exceptions.RefreshTokenInvalidoException;
import com.consultas.api_consultas.security.AuthenticationService;
import com.consultas.api_consultas.services.PasswordRecoveryService;
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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.jwt;
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

    @MockBean
    private PasswordRecoveryService passwordRecoveryService;

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

    @Test
    @DisplayName("Deve retornar 204 ao deslogar usuário autenticado e delegar logout para o service")
    void deveRetornar204AoDeslogarUsuarioAutenticado() throws Exception {
        mvc().perform(post("/auth/logout")
                        .with(jwt().jwt(jwtBuilder -> jwtBuilder.subject("admin")))
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoRefresh("refresh-do-logout")))
                .andExpect(status().isNoContent());

        verify(authenticationService).logout(any(), eq("refresh-do-logout"));
    }

    @Test
    @DisplayName("Deve retornar 401 ao tentar logout sem Bearer")
    void deveRetornar401AoTentarLogoutSemBearer() throws Exception {
        mvc().perform(post("/auth/logout")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoRefresh("qualquer")))
                .andExpect(status().isUnauthorized());
    }


    private String corpoForgotPassword(String email) throws Exception {
        return objectMapper.writeValueAsString(new ForgotPasswordDto(email));
    }

    private String corpoResetPassword(String token, String novaSenha) throws Exception {
        return objectMapper.writeValueAsString(new ResetPasswordDto(token, novaSenha));
    }

    @Test
    @DisplayName("Deve retornar 204 em forgot-password e delegar ao service")
    void deveRetornar204AoSolicitarForgotPassword() throws Exception {
        mvc().perform(post("/auth/forgot-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoForgotPassword("usuario@example.com")))
                .andExpect(status().isNoContent());

        verify(passwordRecoveryService).solicitarRedefinicao("usuario@example.com");
    }

    @Test
    @DisplayName("Deve retornar 400 quando email em forgot-password é inválido")
    void deveRetornar400QuandoEmailForgotInvalido() throws Exception {
        mvc().perform(post("/auth/forgot-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoForgotPassword("nao-eh-email")))
                .andExpect(status().isBadRequest());
    }

    @Test
    @DisplayName("Deve retornar 429 ao estourar rate limit de forgot-password")
    void deveRetornar429AoEstourarRateLimitForgot() throws Exception {
        MockMvc mvc = mvc();
        String body = corpoForgotPassword("alvo.flood@example.com");

        // Capacidade configurada em application-test.yml = 2
        for (int i = 0; i < 2; i++) {
            mvc.perform(post("/auth/forgot-password")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(body))
                    .andExpect(status().isNoContent());
        }

        // 3ª chamada é bloqueada pelo rate limit
        mvc.perform(post("/auth/forgot-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(body))
                .andExpect(status().isTooManyRequests())
                .andExpect(header().exists("Retry-After"))
                .andExpect(jsonPath("$.message").value(
                        org.hamcrest.Matchers.containsString("Muitas solicitacoes")
                ));
    }

    @Test
    @DisplayName("Deve retornar 204 ao redefinir senha com token válido")
    void deveRetornar204AoResetarSenha() throws Exception {
        String token = "11111111-2222-3333-4444-555555555555";

        mvc().perform(post("/auth/reset-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoResetPassword(token, "novaSenha123")))
                .andExpect(status().isNoContent());

        verify(passwordRecoveryService).redefinirSenha(token, "novaSenha123");
    }

    @Test
    @DisplayName("Deve retornar 401 ao redefinir senha com token inválido ou expirado")
    void deveRetornar401AoResetarComTokenInvalido() throws Exception {
        String token = "99999999-9999-9999-9999-999999999999";
        org.mockito.Mockito.doThrow(new PasswordResetTokenInvalidoException("Token expirado"))
                .when(passwordRecoveryService).redefinirSenha(token, "novaSenha123");

        mvc().perform(post("/auth/reset-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoResetPassword(token, "novaSenha123")))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("Token de redefinição inválido ou expirado."));
    }

    @Test
    @DisplayName("Deve retornar 400 quando reset-password recebe payload inválido")
    void deveRetornar400QuandoResetPayloadInvalido() throws Exception {
        // token com tamanho fora de 36 caracteres
        mvc().perform(post("/auth/reset-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(corpoResetPassword("token-curto", "novaSenha123")))
                .andExpect(status().isBadRequest());
    }
}
