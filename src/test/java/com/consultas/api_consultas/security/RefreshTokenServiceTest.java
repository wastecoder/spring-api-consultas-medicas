package com.consultas.api_consultas.security;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.entities.RefreshToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.exceptions.RefreshTokenInvalidoException;
import com.consultas.api_consultas.repositories.RefreshTokenRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class RefreshTokenServiceTest {

    private static final Instant AGORA = Instant.parse("2026-05-09T10:00:00Z");

    @Mock
    private RefreshTokenRepository repository;

    private Clock clock;
    private RefreshTokenService service;
    private Usuario usuario;

    @BeforeEach
    void setUp() {
        clock = Clock.fixed(AGORA, ZoneOffset.UTC);
        service = new RefreshTokenService(repository, clock);
        usuario = Usuario.builder()
                .id(1L)
                .username("admin")
                .senha("hash")
                .funcao(Funcao.ADMIN)
                .ativo(true)
                .build();
    }

    @Test
    @DisplayName("criar() persiste token com expiração 7 dias à frente do clock")
    void criarPersisteComExpiracaoCorreta() {
        when(repository.save(any(RefreshToken.class))).thenAnswer(inv -> inv.getArgument(0));

        RefreshToken criado = service.criar(usuario);

        ArgumentCaptor<RefreshToken> captor = ArgumentCaptor.forClass(RefreshToken.class);
        verify(repository).save(captor.capture());
        RefreshToken salvo = captor.getValue();

        assertSame(usuario, salvo.getUsuario());
        assertEquals(AGORA, salvo.getCriadoEm());
        assertEquals(AGORA.plusSeconds(AppConstants.JWT_REFRESH_EXPIRACAO_SEGUNDOS), salvo.getExpiraEm());
        assertEquals(36, salvo.getToken().length()); // UUID
        assertSame(salvo, criado);
    }

    @Test
    @DisplayName("validarERotacionar() deleta o antigo e emite novo para refresh válido")
    void validarERotacionarRotacionaQuandoValido() {
        RefreshToken antigo = RefreshToken.builder()
                .id(10L)
                .token("antigo-uuid")
                .usuario(usuario)
                .criadoEm(AGORA.minusSeconds(60))
                .expiraEm(AGORA.plusSeconds(3600))
                .build();
        when(repository.findByToken("antigo-uuid")).thenReturn(Optional.of(antigo));
        when(repository.save(any(RefreshToken.class))).thenAnswer(inv -> inv.getArgument(0));

        RefreshToken novo = service.validarERotacionar("antigo-uuid");

        verify(repository).delete(antigo);
        verify(repository).save(any(RefreshToken.class));
        assertNotEquals("antigo-uuid", novo.getToken());
        assertSame(usuario, novo.getUsuario());
    }

    @Test
    @DisplayName("validarERotacionar() lança 401 quando token não existe no banco")
    void validarERotacionarLancaQuandoNaoEncontrado() {
        when(repository.findByToken("inexistente")).thenReturn(Optional.empty());

        assertThrows(RefreshTokenInvalidoException.class,
                () -> service.validarERotacionar("inexistente"));

        verify(repository, never()).delete(any(RefreshToken.class));
        verify(repository, never()).save(any(RefreshToken.class));
    }

    @Test
    @DisplayName("validarERotacionar() lança 401 e remove o registro quando expirado")
    void validarERotacionarLancaQuandoExpirado() {
        RefreshToken expirado = RefreshToken.builder()
                .id(11L)
                .token("expirado-uuid")
                .usuario(usuario)
                .criadoEm(AGORA.minusSeconds(8 * 24 * 3600))
                .expiraEm(AGORA.minusSeconds(60))
                .build();
        when(repository.findByToken("expirado-uuid")).thenReturn(Optional.of(expirado));

        assertThrows(RefreshTokenInvalidoException.class,
                () -> service.validarERotacionar("expirado-uuid"));

        verify(repository).delete(expirado);
        verify(repository, never()).save(any(RefreshToken.class));
    }

    @Test
    @DisplayName("revogar() delega para deleteByToken")
    void revogarDelegaParaRepositorio() {
        service.revogar("qualquer-uuid");

        verify(repository, times(1)).deleteByToken(eq("qualquer-uuid"));
    }

}
