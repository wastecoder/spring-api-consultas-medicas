package com.consultas.api_consultas.integration;

import com.consultas.api_consultas.dtos.requisicoes.ForgotPasswordDto;
import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.consultas.api_consultas.dtos.requisicoes.ResetPasswordDto;
import com.consultas.api_consultas.entities.PasswordResetToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.repositories.PasswordResetTokenRepository;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.EmailService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

// Fluxo end-to-end de "esqueci minha senha":
// cadastra usuario -> forgot-password -> captura token via EmailService mock -> reset-password ->
// login com senha antiga falha -> login com senha nova passa -> token de reset nao pode ser reutilizado.
@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@DisplayName("Integração — fluxo esqueci minha senha")
class FluxoRecuperacaoSenhaIntegrationTest {

    @Autowired private MockMvc mvc;
    @Autowired private ObjectMapper objectMapper;
    @Autowired private UsuarioRepository usuarioRepository;
    @Autowired private PasswordResetTokenRepository tokenRepository;
    @Autowired private PasswordEncoder passwordEncoder;

    @MockBean
    private EmailService emailService;


    @Test
    @DisplayName("Deve permitir trocar a senha via fluxo completo e invalidar token apos uso")
    void deveExecutarFluxoCompletoDeRedefinicao() throws Exception {
        String emailUsuario = "esqueci@example.com";
        String senhaAntiga = "senhaAntiga1";
        String senhaNova = "senhaNova456";

        // 1. Cria usuario diretamente no banco com senha antiga ja codificada
        Usuario usuario = usuarioRepository.save(Usuario.builder()
                .username("esqueci_user")
                .email(emailUsuario)
                .senha(passwordEncoder.encode(senhaAntiga))
                .funcao(Funcao.RECEPCIONISTA)
                .ativo(true)
                .build());

        // 2. Solicita redefinicao -> 204 e EmailService eh chamado com o link
        mvc.perform(post("/auth/forgot-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new ForgotPasswordDto(emailUsuario))))
                .andExpect(status().isNoContent());

        ArgumentCaptor<String> destinatarioCapt = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<String> linkCapt = ArgumentCaptor.forClass(String.class);
        verify(emailService).enviarEmailRedefinicaoSenha(destinatarioCapt.capture(), linkCapt.capture());
        assertEquals(emailUsuario, destinatarioCapt.getValue());
        assertTrue(linkCapt.getValue().contains("/reset-password?token="));

        // 3. Captura o token persistido para usar no reset
        List<PasswordResetToken> tokens = tokenRepository.findAll();
        assertEquals(1, tokens.size());
        PasswordResetToken salvo = tokens.get(0);
        assertEquals(usuario.getId(), salvo.getUsuario().getId());
        assertNotNull(salvo.getToken());

        // 4. Redefine a senha com o token -> 204
        mvc.perform(post("/auth/reset-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new ResetPasswordDto(salvo.getToken(), senhaNova))))
                .andExpect(status().isNoContent());

        // 5. Login com senha antiga deve falhar (401)
        mvc.perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new LoginDTO(usuario.getUsername(), senhaAntiga))))
                .andExpect(status().isUnauthorized());

        // 6. Login com senha nova deve funcionar (200)
        mvc.perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new LoginDTO(usuario.getUsername(), senhaNova))))
                .andExpect(status().isOk());

        // 7. Reutilizar o mesmo token de reset deve falhar com 401
        mvc.perform(post("/auth/reset-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new ResetPasswordDto(salvo.getToken(), "outraSenhaQq"))))
                .andExpect(status().isUnauthorized());
    }


    @Test
    @DisplayName("Deve responder 204 e nao chamar email quando o email nao corresponde a nenhum usuario")
    void deveResponder204QuandoEmailDesconhecido() throws Exception {
        mvc.perform(post("/auth/forgot-password")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new ForgotPasswordDto("nao_existe_aqui@example.com"))))
                .andExpect(status().isNoContent());

        org.mockito.Mockito.verifyNoInteractions(emailService);
    }

}
