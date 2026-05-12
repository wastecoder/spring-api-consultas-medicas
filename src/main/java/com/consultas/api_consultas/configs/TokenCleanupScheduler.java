package com.consultas.api_consultas.configs;

import com.consultas.api_consultas.repositories.PasswordResetTokenRepository;
import com.consultas.api_consultas.repositories.RefreshTokenRepository;
import com.consultas.api_consultas.repositories.TokenBlacklistRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;

@Component
@RequiredArgsConstructor
@Slf4j
public class TokenCleanupScheduler {

    private final RefreshTokenRepository refreshTokenRepository;
    private final TokenBlacklistRepository tokenBlacklistRepository;
    private final PasswordResetTokenRepository passwordResetTokenRepository;
    private final Clock clock;

    @Scheduled(cron = "0 0 3 * * *")
    @Transactional
    public void limparTokensExpirados() {
        Instant agora = Instant.now(clock);
        log.info("Iniciando limpeza de tokens (refresh, blacklist, password reset) com expiração anterior a {}", agora);
        refreshTokenRepository.deleteAllByExpiraEmBefore(agora);
        tokenBlacklistRepository.deleteAllByExpiraEmBefore(agora);
        passwordResetTokenRepository.deleteAllByExpiraEmBefore(agora);
    }

}
