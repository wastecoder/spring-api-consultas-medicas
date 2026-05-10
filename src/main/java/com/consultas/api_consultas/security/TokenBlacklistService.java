package com.consultas.api_consultas.security;

import com.consultas.api_consultas.entities.TokenBlacklist;
import com.consultas.api_consultas.repositories.TokenBlacklistRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;

@Service
@RequiredArgsConstructor
public class TokenBlacklistService {

    private final TokenBlacklistRepository repository;
    private final Clock clock;

    @Transactional
    public void blacklistar(Jwt jwt) {
        String jti = jwt.getId();
        if (jti == null || jti.isBlank()) {
            return;
        }
        if (repository.existsByJti(jti)) {
            return;
        }
        Instant expiraEm = jwt.getExpiresAt() != null
                ? jwt.getExpiresAt()
                : Instant.now(clock);

        repository.save(TokenBlacklist.builder()
                .jti(jti)
                .expiraEm(expiraEm)
                .revogadoEm(Instant.now(clock))
                .build());
    }

    public boolean estaBlacklistado(String jti) {
        if (jti == null || jti.isBlank()) {
            return false;
        }
        return repository.existsByJti(jti);
    }

}
