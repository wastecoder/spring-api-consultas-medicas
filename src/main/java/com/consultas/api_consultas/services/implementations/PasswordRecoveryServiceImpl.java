package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.entities.PasswordResetToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.exceptions.PasswordResetTokenInvalidoException;
import com.consultas.api_consultas.repositories.PasswordResetTokenRepository;
import com.consultas.api_consultas.repositories.RefreshTokenRepository;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.EmailService;
import com.consultas.api_consultas.services.PasswordRecoveryService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

@Service
@Slf4j
public class PasswordRecoveryServiceImpl implements PasswordRecoveryService {

    private final UsuarioRepository usuarioRepository;
    private final PasswordResetTokenRepository tokenRepository;
    private final RefreshTokenRepository refreshTokenRepository;
    private final EmailService emailService;
    private final PasswordEncoder passwordEncoder;
    private final Clock clock;
    private final String frontendUrl;

    public PasswordRecoveryServiceImpl(
            UsuarioRepository usuarioRepository,
            PasswordResetTokenRepository tokenRepository,
            RefreshTokenRepository refreshTokenRepository,
            EmailService emailService,
            PasswordEncoder passwordEncoder,
            Clock clock,
            @Value("${app.frontend-url}") String frontendUrl
    ) {
        this.usuarioRepository = usuarioRepository;
        this.tokenRepository = tokenRepository;
        this.refreshTokenRepository = refreshTokenRepository;
        this.emailService = emailService;
        this.passwordEncoder = passwordEncoder;
        this.clock = clock;
        this.frontendUrl = frontendUrl;
    }

    @Override
    @Transactional
    public void solicitarRedefinicao(String email) {
        // Nao revela se o email existe: silencio quando nao acha usuario ativo.
        usuarioRepository.findByEmailIgnoreCaseAndAtivoTrue(email).ifPresentOrElse(
                this::gerarTokenEEnviarEmail,
                () -> log.info("Solicitacao de redefinicao recebida para email sem usuario ativo (silenciada)")
        );
    }

    private void gerarTokenEEnviarEmail(Usuario usuario) {
        Instant agora = Instant.now(clock);

        // Invalida qualquer token anterior ainda valido: so o link mais recente fica utilizavel.
        tokenRepository.invalidarTokensAtivosDoUsuario(usuario, agora);

        PasswordResetToken token = PasswordResetToken.builder()
                .token(UUID.randomUUID().toString())
                .usuario(usuario)
                .criadoEm(agora)
                .expiraEm(agora.plus(Duration.ofMinutes(AppConstants.PASSWORD_RESET_TOKEN_EXPIRACAO_MINUTOS)))
                .build();
        tokenRepository.save(token);

        String link = montarLinkRedefinicao(token.getToken());
        emailService.enviarEmailRedefinicaoSenha(usuario.getEmail(), link);

        log.info("Token de redefinicao gerado para usuario id={}", usuario.getId());
    }

    private String montarLinkRedefinicao(String token) {
        String base = frontendUrl.endsWith("/") ? frontendUrl.substring(0, frontendUrl.length() - 1) : frontendUrl;
        return base + "/reset-password?token=" + token;
    }

    @Override
    @Transactional
    public void redefinirSenha(String token, String novaSenha) {
        Instant agora = Instant.now(clock);

        PasswordResetToken registro = tokenRepository.findByToken(token)
                .orElseThrow(() -> new PasswordResetTokenInvalidoException("Token nao encontrado"));

        if (registro.getUsadoEm() != null) {
            throw new PasswordResetTokenInvalidoException("Token ja utilizado");
        }
        if (registro.getExpiraEm().isBefore(agora)) {
            throw new PasswordResetTokenInvalidoException("Token expirado");
        }

        Usuario usuario = registro.getUsuario();
        usuario.setSenha(passwordEncoder.encode(novaSenha));
        usuarioRepository.save(usuario);

        registro.setUsadoEm(agora);
        tokenRepository.save(registro);

        // Forca logout de todas as sessoes ativas: refresh tokens existentes sao revogados.
        refreshTokenRepository.deleteByUsuario(usuario);

        log.info("Senha redefinida para usuario id={}", usuario.getId());
    }

}
