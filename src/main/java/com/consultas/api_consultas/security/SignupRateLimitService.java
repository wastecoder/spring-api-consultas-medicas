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
public class SignupRateLimitService {

    private final RateLimiterRegistry registry;
    private final long periodoSegundos;

    public SignupRateLimitService(
            @Value("${rate-limit.signup.capacidade}") int capacidade,
            @Value("${rate-limit.signup.periodo-segundos}") long periodoSegundos
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
        // Chave por par IP + email: impede cadastro em massa do mesmo IP e flood mirado num email.
        String chave = ip + ":" + email.toLowerCase();
        RateLimiter rateLimiter = registry.rateLimiter(chave);

        if (!rateLimiter.acquirePermission()) {
            log.warn("Rate limit excedido para cadastro: ip={}, email={}", ip, email);
            String mensagem = String.format(
                    "Muitas tentativas de cadastro. Tente novamente em %d segundos.",
                    periodoSegundos
            );
            throw new RateLimitExcedidoException(mensagem, periodoSegundos);
        }
    }

}
