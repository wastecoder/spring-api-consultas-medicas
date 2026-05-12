package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.PasswordResetToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.exceptions.PasswordResetTokenInvalidoException;
import com.consultas.api_consultas.repositories.PasswordResetTokenRepository;
import com.consultas.api_consultas.repositories.RefreshTokenRepository;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.EmailService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de recuperação de senha")
class PasswordRecoveryServiceImplTest {

    private static final Instant AGORA = Instant.parse("2026-05-11T10:00:00Z");
    private static final String FRONTEND = "http://localhost:4200";

    @Mock private UsuarioRepository usuarioRepository;
    @Mock private PasswordResetTokenRepository tokenRepository;
    @Mock private RefreshTokenRepository refreshTokenRepository;
    @Mock private EmailService emailService;
    @Mock private PasswordEncoder passwordEncoder;

    private Clock clock;
    private PasswordRecoveryServiceImpl service;
    private Usuario usuario;

    @BeforeEach
    void setUp() {
        clock = Clock.fixed(AGORA, ZoneOffset.UTC);
        service = new PasswordRecoveryServiceImpl(
                usuarioRepository,
                tokenRepository,
                refreshTokenRepository,
                emailService,
                passwordEncoder,
                clock,
                FRONTEND
        );
        usuario = Usuario.builder()
                .id(42L)
                .username("usuario_x")
                .email("usuario.x@example.com")
                .senha("hash-antiga")
                .funcao(Funcao.PACIENTE)
                .ativo(true)
                .build();
    }


    @Test
    @DisplayName("solicitarRedefinicao() silencia quando o email nao corresponde a nenhum usuario ativo")
    void deveSilenciarQuandoEmailNaoExiste() {
        when(usuarioRepository.findByEmailIgnoreCaseAndAtivoTrue("inexistente@example.com"))
                .thenReturn(Optional.empty());

        service.solicitarRedefinicao("inexistente@example.com");

        verify(tokenRepository, never()).save(any());
        verifyNoInteractions(emailService);
    }

    @Test
    @DisplayName("solicitarRedefinicao() invalida tokens antigos, cria token novo e envia email com link")
    void deveCriarTokenEEnviarEmail() {
        when(usuarioRepository.findByEmailIgnoreCaseAndAtivoTrue(usuario.getEmail()))
                .thenReturn(Optional.of(usuario));

        service.solicitarRedefinicao(usuario.getEmail());

        verify(tokenRepository).invalidarTokensAtivosDoUsuario(usuario, AGORA);

        ArgumentCaptor<PasswordResetToken> capt = ArgumentCaptor.forClass(PasswordResetToken.class);
        verify(tokenRepository).save(capt.capture());
        PasswordResetToken salvo = capt.getValue();
        assertNotNull(salvo.getToken());
        assertEquals(36, salvo.getToken().length(), "Token deve ser um UUID de 36 caracteres");
        assertEquals(usuario, salvo.getUsuario());
        assertEquals(AGORA, salvo.getCriadoEm());
        assertEquals(AGORA.plus(Duration.ofMinutes(15)), salvo.getExpiraEm());

        ArgumentCaptor<String> linkCapt = ArgumentCaptor.forClass(String.class);
        verify(emailService).enviarEmailRedefinicaoSenha(eq(usuario.getEmail()), linkCapt.capture());
        String linkEsperado = FRONTEND + "/reset-password?token=" + salvo.getToken();
        assertEquals(linkEsperado, linkCapt.getValue());
    }

    @Test
    @DisplayName("redefinirSenha() troca senha com token valido e revoga refresh tokens")
    void deveRedefinirSenhaComTokenValido() {
        String tokenStr = "valido-uuid";
        PasswordResetToken token = PasswordResetToken.builder()
                .id(1L)
                .token(tokenStr)
                .usuario(usuario)
                .criadoEm(AGORA.minusSeconds(60))
                .expiraEm(AGORA.plusSeconds(60))
                .build();

        when(tokenRepository.findByToken(tokenStr)).thenReturn(Optional.of(token));
        when(passwordEncoder.encode("novaSenha123")).thenReturn("hash-nova");

        service.redefinirSenha(tokenStr, "novaSenha123");

        assertEquals("hash-nova", usuario.getSenha());
        assertEquals(AGORA, token.getUsadoEm());
        verify(usuarioRepository).save(usuario);
        verify(tokenRepository).save(token);
        verify(refreshTokenRepository).deleteByUsuario(usuario);
    }

    @Test
    @DisplayName("redefinirSenha() lanca PasswordResetTokenInvalidoException quando token nao existe")
    void deveLancarQuandoTokenNaoExiste() {
        when(tokenRepository.findByToken("nao-existe")).thenReturn(Optional.empty());

        assertThrows(
                PasswordResetTokenInvalidoException.class,
                () -> service.redefinirSenha("nao-existe", "qualquer")
        );

        verify(usuarioRepository, never()).save(any());
        verify(refreshTokenRepository, never()).deleteByUsuario(any());
    }

    @Test
    @DisplayName("redefinirSenha() lanca quando token ja foi usado")
    void deveLancarQuandoTokenJaUsado() {
        PasswordResetToken usado = PasswordResetToken.builder()
                .token("usado")
                .usuario(usuario)
                .criadoEm(AGORA.minusSeconds(60))
                .expiraEm(AGORA.plusSeconds(60))
                .usadoEm(AGORA.minusSeconds(10))
                .build();
        when(tokenRepository.findByToken("usado")).thenReturn(Optional.of(usado));

        assertThrows(
                PasswordResetTokenInvalidoException.class,
                () -> service.redefinirSenha("usado", "qualquer")
        );

        verify(usuarioRepository, never()).save(any());
    }

    @Test
    @DisplayName("redefinirSenha() lanca quando token esta expirado")
    void deveLancarQuandoTokenExpirado() {
        PasswordResetToken expirado = PasswordResetToken.builder()
                .token("expirado")
                .usuario(usuario)
                .criadoEm(AGORA.minusSeconds(1800))
                .expiraEm(AGORA.minusSeconds(1))
                .build();
        when(tokenRepository.findByToken("expirado")).thenReturn(Optional.of(expirado));

        assertThrows(
                PasswordResetTokenInvalidoException.class,
                () -> service.redefinirSenha("expirado", "qualquer")
        );

        verify(usuarioRepository, never()).save(any());
        verify(refreshTokenRepository, never()).deleteByUsuario(any());
    }

}
