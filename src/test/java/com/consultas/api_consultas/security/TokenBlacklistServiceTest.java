package com.consultas.api_consultas.security;

import com.consultas.api_consultas.entities.TokenBlacklist;
import com.consultas.api_consultas.repositories.TokenBlacklistRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.oauth2.jwt.Jwt;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class TokenBlacklistServiceTest {

    private static final Instant AGORA = Instant.parse("2026-05-09T10:00:00Z");

    @Mock
    private TokenBlacklistRepository repository;

    private TokenBlacklistService service;

    @BeforeEach
    void setUp() {
        Clock clock = Clock.fixed(AGORA, ZoneOffset.UTC);
        service = new TokenBlacklistService(repository, clock);
    }

    private Jwt jwtComJti(String jti, Instant expiresAt) {
        Jwt.Builder builder = Jwt.withTokenValue("token-fake")
                .header("alg", "RS256")
                .subject("admin")
                .issuedAt(AGORA);
        if (jti != null) {
            builder.claim("jti", jti);
        }
        if (expiresAt != null) {
            builder.expiresAt(expiresAt);
        }
        return builder.build();
    }

    @Test
    @DisplayName("blacklistar() persiste registro com jti, expiraEm e revogadoEm")
    void blacklistarPersiste() {
        Instant exp = AGORA.plusSeconds(3600);
        Jwt jwt = jwtComJti("jti-1", exp);
        when(repository.existsByJti("jti-1")).thenReturn(false);

        service.blacklistar(jwt);

        ArgumentCaptor<TokenBlacklist> captor = ArgumentCaptor.forClass(TokenBlacklist.class);
        verify(repository).save(captor.capture());
        TokenBlacklist salvo = captor.getValue();

        assertEquals("jti-1", salvo.getJti());
        assertEquals(exp, salvo.getExpiraEm());
        assertEquals(AGORA, salvo.getRevogadoEm());
    }

    @Test
    @DisplayName("blacklistar() é idempotente: não duplica quando jti já está presente")
    void blacklistarNaoDuplica() {
        Jwt jwt = jwtComJti("jti-existente", AGORA.plusSeconds(60));
        when(repository.existsByJti("jti-existente")).thenReturn(true);

        service.blacklistar(jwt);

        verify(repository, never()).save(any(TokenBlacklist.class));
    }

    @Test
    @DisplayName("blacklistar() ignora token sem jti")
    void blacklistarIgnoraSemJti() {
        Jwt jwt = jwtComJti(null, AGORA.plusSeconds(60));

        service.blacklistar(jwt);

        verify(repository, never()).existsByJti(any());
        verify(repository, never()).save(any(TokenBlacklist.class));
    }

    @Test
    @DisplayName("estaBlacklistado() consulta o repositório")
    void estaBlacklistadoConsulta() {
        when(repository.existsByJti("jti-revogado")).thenReturn(true);
        when(repository.existsByJti("jti-livre")).thenReturn(false);

        assertTrue(service.estaBlacklistado("jti-revogado"));
        assertFalse(service.estaBlacklistado("jti-livre"));
    }

    @Test
    @DisplayName("estaBlacklistado() retorna false para jti nulo ou em branco")
    void estaBlacklistadoNuloOuBranco() {
        assertFalse(service.estaBlacklistado(null));
        assertFalse(service.estaBlacklistado(""));
        assertFalse(service.estaBlacklistado("  "));
        verify(repository, never()).existsByJti(any());
    }

}
