package com.consultas.api_consultas.security;

import com.consultas.api_consultas.exceptions.RateLimitExcedidoException;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class LoginRateLimitServiceTest {

    private static final int CAPACIDADE = 3;
    private static final long PERIODO_SEGUNDOS = 60L;

    private LoginRateLimitService novoService() {
        return new LoginRateLimitService(CAPACIDADE, PERIODO_SEGUNDOS);
    }

    @Test
    @DisplayName("Deve permitir tentativas até a capacidade configurada")
    void devePermitirTentativasAteCapacidade() {
        LoginRateLimitService service = novoService();

        for (int i = 0; i < CAPACIDADE; i++) {
            assertDoesNotThrow(() -> service.verificar("10.0.0.1", "admin"));
        }
    }

    @Test
    @DisplayName("Deve lançar RateLimitExcedidoException ao estourar a capacidade")
    void deveLancarExcecaoAoEstourarCapacidade() {
        LoginRateLimitService service = novoService();

        for (int i = 0; i < CAPACIDADE; i++) {
            service.verificar("10.0.0.1", "admin");
        }

        RateLimitExcedidoException ex = assertThrows(
                RateLimitExcedidoException.class,
                () -> service.verificar("10.0.0.1", "admin")
        );
        assertEquals(PERIODO_SEGUNDOS, ex.getRetryAfterSegundos());
    }

    @Test
    @DisplayName("Buckets devem ser independentes por username (mesmo IP)")
    void bucketIndependentePorUsername() {
        LoginRateLimitService service = novoService();

        for (int i = 0; i < CAPACIDADE; i++) {
            service.verificar("10.0.0.1", "admin");
        }

        assertDoesNotThrow(() -> service.verificar("10.0.0.1", "outro"));
    }

    @Test
    @DisplayName("Buckets devem ser independentes por IP (mesmo username)")
    void bucketIndependentePorIp() {
        LoginRateLimitService service = novoService();

        for (int i = 0; i < CAPACIDADE; i++) {
            service.verificar("10.0.0.1", "admin");
        }

        assertDoesNotThrow(() -> service.verificar("10.0.0.2", "admin"));
    }

    @Test
    @DisplayName("extrairIpCliente deve usar X-Forwarded-For quando presente")
    void extrairIpUsaForwardedForQuandoPresente(@Mock HttpServletRequest request) {
        when(request.getHeader("X-Forwarded-For")).thenReturn("203.0.113.10");

        assertEquals("203.0.113.10", LoginRateLimitService.extrairIpCliente(request));
    }

    @Test
    @DisplayName("extrairIpCliente deve usar o primeiro IP quando X-Forwarded-For tem vários")
    void extrairIpUsaPrimeiroIpDeForwardedFor(@Mock HttpServletRequest request) {
        when(request.getHeader("X-Forwarded-For")).thenReturn("203.0.113.10, 198.51.100.5, 10.0.0.1");

        assertEquals("203.0.113.10", LoginRateLimitService.extrairIpCliente(request));
    }

    @Test
    @DisplayName("extrairIpCliente deve cair para remoteAddr quando X-Forwarded-For ausente")
    void extrairIpCaiParaRemoteAddrQuandoForwardedAusente(@Mock HttpServletRequest request) {
        when(request.getHeader("X-Forwarded-For")).thenReturn(null);
        when(request.getRemoteAddr()).thenReturn("10.0.0.42");

        assertEquals("10.0.0.42", LoginRateLimitService.extrairIpCliente(request));
    }

    @Test
    @DisplayName("extrairIpCliente deve cair para remoteAddr quando X-Forwarded-For está em branco")
    void extrairIpCaiParaRemoteAddrQuandoForwardedEmBranco(@Mock HttpServletRequest request) {
        when(request.getHeader("X-Forwarded-For")).thenReturn("   ");
        when(request.getRemoteAddr()).thenReturn("10.0.0.42");

        assertEquals("10.0.0.42", LoginRateLimitService.extrairIpCliente(request));
    }
}
