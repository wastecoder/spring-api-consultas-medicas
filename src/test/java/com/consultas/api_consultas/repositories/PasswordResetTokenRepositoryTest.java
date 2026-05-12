package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.PasswordResetToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ActiveProfiles;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
@DisplayName("Repositório de PasswordResetToken")
class PasswordResetTokenRepositoryTest {

    private static final Instant AGORA = Instant.parse("2026-05-11T10:00:00Z");

    @Autowired private PasswordResetTokenRepository repository;
    @Autowired private UsuarioRepository usuarioRepository;
    @Autowired private EntityManager em;

    private Usuario usuario;

    @BeforeEach
    void setUp() {
        usuario = usuarioRepository.save(Usuario.builder()
                .username("recover_user")
                .email("recover@example.com")
                .senha("hash")
                .funcao(Funcao.PACIENTE)
                .ativo(true)
                .build());
    }

    private PasswordResetToken salvar(Instant criadoEm, Instant expiraEm, Instant usadoEm) {
        PasswordResetToken t = PasswordResetToken.builder()
                .token(UUID.randomUUID().toString())
                .usuario(usuario)
                .criadoEm(criadoEm)
                .expiraEm(expiraEm)
                .usadoEm(usadoEm)
                .build();
        return repository.save(t);
    }


    @Test
    @DisplayName("findByToken() retorna registro persistido")
    void deveEncontrarPorToken() {
        PasswordResetToken salvo = salvar(AGORA, AGORA.plusSeconds(900), null);

        Optional<PasswordResetToken> achado = repository.findByToken(salvo.getToken());

        assertTrue(achado.isPresent());
        assertEquals(salvo.getId(), achado.get().getId());
        assertEquals(usuario.getId(), achado.get().getUsuario().getId());
    }

    @Test
    @DisplayName("findByToken() devolve vazio para token desconhecido")
    void deveDevolverVazioQuandoTokenNaoExiste() {
        assertTrue(repository.findByToken("nao-existe").isEmpty());
    }

    @Test
    @DisplayName("deleteAllByExpiraEmBefore() remove apenas registros expirados")
    void deveRemoverApenasExpirados() {
        PasswordResetToken expirado = salvar(AGORA.minusSeconds(3600), AGORA.minusSeconds(60), null);
        PasswordResetToken ativo = salvar(AGORA.minusSeconds(60), AGORA.plusSeconds(900), null);

        repository.deleteAllByExpiraEmBefore(AGORA);
        em.flush();
        em.clear();

        assertTrue(repository.findById(expirado.getId()).isEmpty());
        assertTrue(repository.findById(ativo.getId()).isPresent());
    }

    @Test
    @DisplayName("invalidarTokensAtivosDoUsuario() marca usadoEm em tokens validos, ignorando expirados e ja usados")
    void deveInvalidarApenasTokensAtivos() {
        PasswordResetToken ativo = salvar(AGORA.minusSeconds(60), AGORA.plusSeconds(900), null);
        PasswordResetToken jaUsado = salvar(AGORA.minusSeconds(120), AGORA.plusSeconds(900), AGORA.minusSeconds(30));
        PasswordResetToken expirado = salvar(AGORA.minusSeconds(3600), AGORA.minusSeconds(60), null);

        int afetados = repository.invalidarTokensAtivosDoUsuario(usuario, AGORA);

        em.flush();
        em.clear();

        assertEquals(1, afetados);
        PasswordResetToken ativoAtualizado = repository.findById(ativo.getId()).orElseThrow();
        assertNotNull(ativoAtualizado.getUsadoEm());
        assertEquals(AGORA, ativoAtualizado.getUsadoEm());

        // ja usado nao deve ter sido reescrito
        PasswordResetToken usadoOriginal = repository.findById(jaUsado.getId()).orElseThrow();
        assertEquals(jaUsado.getUsadoEm(), usadoOriginal.getUsadoEm());

        // expirado continua sem marcacao de uso (logica do scheduler eh quem o remove)
        PasswordResetToken expiradoOriginal = repository.findById(expirado.getId()).orElseThrow();
        assertNull(expiradoOriginal.getUsadoEm());
    }

    @Test
    @DisplayName("Tokens duplicados violam a unique constraint")
    void deveImpedirTokenDuplicado() {
        String mesmoToken = UUID.randomUUID().toString();
        PasswordResetToken primeiro = PasswordResetToken.builder()
                .token(mesmoToken)
                .usuario(usuario)
                .criadoEm(AGORA)
                .expiraEm(AGORA.plusSeconds(900))
                .build();
        repository.saveAndFlush(primeiro);

        PasswordResetToken duplicado = PasswordResetToken.builder()
                .token(mesmoToken)
                .usuario(usuario)
                .criadoEm(AGORA)
                .expiraEm(AGORA.plusSeconds(900))
                .build();

        boolean lancouExcecao = false;
        try {
            repository.saveAndFlush(duplicado);
        } catch (Exception ex) {
            lancouExcecao = true;
        }
        assertTrue(lancouExcecao, "Era esperada uma exceção de constraint para token duplicado");
    }

}
