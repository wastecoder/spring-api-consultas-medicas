package com.consultas.api_consultas.auditing;

import com.consultas.api_consultas.configs.AuditingConfig;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.repositories.MedicoRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import jakarta.persistence.EntityManager;

import java.time.Instant;
import java.time.temporal.ChronoUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

// Sem SecurityContext ativo, AuditorAware retorna "system".
// @DataJpaTest não carrega @Configuration regulares, por isso o @Import explícito.
@DataJpaTest
@Import(AuditingConfig.class)
@ActiveProfiles("test")
@TestPropertySource(properties = "popular.banco=false")
@DisplayName("Auditoria — Spring Data JPA Auditing")
class AuditoriaIntegrationTest {

    @Autowired
    private MedicoRepository medicoRepository;

    @Autowired
    private EntityManager entityManager;


    @Test
    @DisplayName("Deve preencher createdBy/createdDate e lastModifiedBy/lastModifiedDate no save inicial")
    void deveAuditarCriacao() {
        Medico medico = new Medico("Carlos Audit", "carlos.audit@medexample.com", "11900000001",
                SiglaCrm.SP, "999111", Especialidade.CARDIOLOGIA);

        Medico salvo = medicoRepository.saveAndFlush(medico);

        assertEquals("system", salvo.getCreatedBy());
        assertEquals("system", salvo.getLastModifiedBy());
        assertNotNull(salvo.getCreatedDate());
        assertNotNull(salvo.getLastModifiedDate());
    }

    @Test
    @DisplayName("Deve atualizar lastModifiedDate sem alterar createdDate no update")
    void deveAuditarAtualizacao() throws InterruptedException {
        Medico medico = new Medico("Diana Audit", "diana.audit@medexample.com", "11900000002",
                SiglaCrm.SP, "999222", Especialidade.NEUROLOGIA);
        Medico salvo = medicoRepository.saveAndFlush(medico);

        Instant createdDateOriginal = salvo.getCreatedDate();
        Instant lastModifiedOriginal = salvo.getLastModifiedDate();

        Thread.sleep(10);

        salvo.setNome("Diana Audit Editada");
        Medico atualizado = medicoRepository.saveAndFlush(salvo);

        // Garante que o listener viu o UPDATE como uma operação separada
        entityManager.flush();
        entityManager.refresh(atualizado);

        // Compara em ms para tolerar a diferença de precisão entre o Instant em memória (ns)
        // e o valor truncado para us que o H2 devolve ao recarregar.
        assertEquals(
                createdDateOriginal.truncatedTo(ChronoUnit.MILLIS),
                atualizado.getCreatedDate().truncatedTo(ChronoUnit.MILLIS),
                "createdDate deve permanecer inalterado");
        assertTrue(atualizado.getLastModifiedDate().isAfter(lastModifiedOriginal),
                "lastModifiedDate deve avançar no update");
        assertEquals("system", atualizado.getLastModifiedBy());
    }

}
