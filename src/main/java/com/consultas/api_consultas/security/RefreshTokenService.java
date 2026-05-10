package com.consultas.api_consultas.security;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.entities.RefreshToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.exceptions.RefreshTokenInvalidoException;
import com.consultas.api_consultas.repositories.RefreshTokenRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class RefreshTokenService {

    private final RefreshTokenRepository refreshTokenRepository;
    private final Clock clock;

    @Transactional
    public RefreshToken criar(Usuario usuario) {
        Instant agora = Instant.now(clock);
        RefreshToken token = RefreshToken.builder()
                .token(UUID.randomUUID().toString())
                .usuario(usuario)
                .criadoEm(agora)
                .expiraEm(agora.plusSeconds(AppConstants.JWT_REFRESH_EXPIRACAO_SEGUNDOS))
                .build();
        return refreshTokenRepository.save(token);
    }

    @Transactional
    public RefreshToken validarERotacionar(String token) {
        RefreshToken antigo = refreshTokenRepository.findByToken(token)
                .orElseThrow(() -> new RefreshTokenInvalidoException("Refresh token inválido."));

        if (antigo.getExpiraEm().isBefore(Instant.now(clock))) {
            refreshTokenRepository.delete(antigo);
            throw new RefreshTokenInvalidoException("Refresh token expirado.");
        }

        Usuario usuario = antigo.getUsuario();
        refreshTokenRepository.delete(antigo);
        return criar(usuario);
    }

    @Transactional
    public void revogar(String token) {
        refreshTokenRepository.deleteByToken(token);
    }

}
