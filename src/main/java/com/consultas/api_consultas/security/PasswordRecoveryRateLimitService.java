package com.consultas.api_consultas.security;

import com.consultas.api_consultas.exceptions.RateLimitExcedidoException;
import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.Duration;

@Service
@Slf4j
public class PasswordRecoveryRateLimitService {

    private final RateLimiterRegistry registry;
    private final long periodoSegundos;

    public PasswordRecoveryRateLimitService(
            @Value("${rate-limit.password-recovery.capacidade}") int capacidade,
            @Value("${rate-limit.password-recovery.periodo-segundos}") long periodoSegundos
    ) {
        this.periodoSegundos = periodoSegundos;
        RateLimiterConfig config = RateLimiterConfig.custom()
                .limitForPeriod(capacidade)
                .limitRefreshPeriod(Duration.ofSeconds(periodoSegundos))
                .timeoutDuration(Duration.ZERO)
                .build();
        this.registry = RateLimiterRegistry.of(config);
    }

    public void verificar(String ip, String email) {
        // Chave por par IP + email: impede flood massivo do mesmo IP e flood mirado num email especifico.
        String chave = ip + ":" + email.toLowerCase();
        RateLimiter rateLimiter = registry.rateLimiter(chave);

        if (!rateLimiter.acquirePermission()) {
            log.warn("Rate limit excedido para recuperacao de senha: ip={}, email={}", ip, email);
            String mensagem = String.format(
                    "Muitas solicitacoes de redefinicao. Tente novamente em %d segundos.",
                    periodoSegundos
            );
            throw new RateLimitExcedidoException(mensagem, periodoSegundos);
        }
    }

}
