package com.consultas.api_consultas.security;

import com.consultas.api_consultas.exceptions.RateLimitExcedidoException;
import io.github.resilience4j.ratelimiter.RateLimiter;
import io.github.resilience4j.ratelimiter.RateLimiterConfig;
import io.github.resilience4j.ratelimiter.RateLimiterRegistry;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.Duration;

@Service
@Slf4j
public class LoginRateLimitService {

    private final RateLimiterRegistry registry;
    private final long periodoSegundos;

    public LoginRateLimitService(
            @Value("${rate-limit.login.capacidade}") int capacidade,
            @Value("${rate-limit.login.periodo-segundos}") long periodoSegundos
    ) {
        this.periodoSegundos = periodoSegundos;
        RateLimiterConfig config = RateLimiterConfig.custom()
                .limitForPeriod(capacidade)
                .limitRefreshPeriod(Duration.ofSeconds(periodoSegundos))
                .timeoutDuration(Duration.ZERO)
                .build();
        this.registry = RateLimiterRegistry.of(config);
    }

    public void verificar(String ip, String username) {
        String chave = ip + ":" + username;
        RateLimiter rateLimiter = registry.rateLimiter(chave);

        if (!rateLimiter.acquirePermission()) {
            log.warn("Rate limit excedido para login: ip={}, username={}", ip, username);
            String mensagem = String.format(
                    "Muitas tentativas de login. Tente novamente em %d segundos.",
                    periodoSegundos
            );
            throw new RateLimitExcedidoException(mensagem, periodoSegundos);
        }
    }

    public static String extrairIpCliente(HttpServletRequest request) {
        String forwarded = request.getHeader("X-Forwarded-For");
        if (forwarded != null && !forwarded.isBlank()) {
            int virgula = forwarded.indexOf(',');
            return (virgula == -1 ? forwarded : forwarded.substring(0, virgula)).trim();
        }
        return request.getRemoteAddr();
    }
}
