package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoMensalDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdaMensalCancelamentoDto;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

// Valida as queries JPQL agregadas do RelatorioFinanceiro contra o H2 real,
// já que toda a complexidade (GROUP BY ano/mes, soma de preço, filtro por status)
// vive no @Query do ConsultaRepository — testes Mockito do service não exercitam isso.
@DataJpaTest
@Import(RelatorioFinanceiroServiceImpl.class)
@AutoConfigureTestDatabase
@ActiveProfiles("test")
class RelatorioFinanceiroServiceImplJpaTest {

    @Autowired
    private RelatorioFinanceiroServiceImpl service;

    @Autowired
    private TestEntityManager em;

    @Autowired
    private EntityManager entityManager;


    private Medico medico;
    private Paciente paciente;


    private void seed() {
        medico = new Medico("Dra. Ana", "ana@x.com", "11999990000",
                SiglaCrm.SP, "111111", Especialidade.CARDIOLOGIA);
        medico.setAtivo(true);
        em.persist(medico);

        paciente = new Paciente("João", "joao@x.com", "11999990001",
                "12345678901", Sexo.MASCULINO, LocalDate.of(1990, 1, 1));
        paciente.setAtivo(true);
        em.persist(paciente);
    }

    private void persistirConsulta(LocalDate data, BigDecimal preco, StatusConsulta status) {
        Consulta c = new Consulta(
                data, LocalTime.of(10, 0), Duration.ofMinutes(30),
                preco, "Rotina", medico, paciente
        );
        em.persist(c); // @PrePersist seta status = AGENDADA
        c.setStatus(status); // sobrescreve para o cenário de teste
        em.flush();
    }


    @Test
    @DisplayName("faturamentoMensal — soma preços de consultas REALIZADAS por (ano, mês)")
    void faturamentoMensalAgrupaPorAnoMes() {
        seed();
        persistirConsulta(LocalDate.of(2025, 1, 15), BigDecimal.valueOf(100), StatusConsulta.REALIZADA);
        persistirConsulta(LocalDate.of(2025, 1, 20), BigDecimal.valueOf(200), StatusConsulta.REALIZADA);
        persistirConsulta(LocalDate.of(2025, 2, 10), BigDecimal.valueOf(50), StatusConsulta.REALIZADA);
        // CANCELADA não deve entrar
        persistirConsulta(LocalDate.of(2025, 1, 25), BigDecimal.valueOf(999), StatusConsulta.CANCELADA);
        em.clear();

        List<FaturamentoMensalDto> dtos = service.faturamentoMensal();

        assertEquals(2, dtos.size(), "esperado 2 grupos (jan/2025 e fev/2025)");
        FaturamentoMensalDto janeiro = dtos.stream().filter(d -> d.mes() == 1).findFirst().orElseThrow();
        FaturamentoMensalDto fevereiro = dtos.stream().filter(d -> d.mes() == 2).findFirst().orElseThrow();
        assertEquals(0, BigDecimal.valueOf(300).compareTo(janeiro.totalFaturado()));
        assertEquals(0, BigDecimal.valueOf(50).compareTo(fevereiro.totalFaturado()));
    }

    @Test
    @DisplayName("perdaMensalComCancelamentos — soma só CANCELADAS por (ano, mês)")
    void perdaMensalSomaApenasCanceladas() {
        seed();
        persistirConsulta(LocalDate.of(2025, 3, 1), BigDecimal.valueOf(150), StatusConsulta.CANCELADA);
        persistirConsulta(LocalDate.of(2025, 3, 5), BigDecimal.valueOf(150), StatusConsulta.CANCELADA);
        persistirConsulta(LocalDate.of(2025, 3, 10), BigDecimal.valueOf(999), StatusConsulta.REALIZADA);
        em.clear();

        List<PerdaMensalCancelamentoDto> dtos = service.perdaMensalComCancelamentos();

        assertEquals(1, dtos.size());
        PerdaMensalCancelamentoDto unico = dtos.get(0);
        assertEquals(3, unico.mes());
        assertEquals(0, BigDecimal.valueOf(300).compareTo(unico.totalPerdido()));
    }

    @Test
    @DisplayName("faturamentoPorPeriodo — devolve a soma do intervalo (REALIZADAS) e ZERO quando vazio")
    void faturamentoPorPeriodoSomaIntervalo() {
        seed();
        persistirConsulta(LocalDate.of(2025, 6, 10), BigDecimal.valueOf(400), StatusConsulta.REALIZADA);
        persistirConsulta(LocalDate.of(2025, 6, 15), BigDecimal.valueOf(100), StatusConsulta.REALIZADA);
        persistirConsulta(LocalDate.of(2025, 7, 1), BigDecimal.valueOf(999), StatusConsulta.REALIZADA);
        em.clear();

        var dto = service.faturamentoPorPeriodo(LocalDate.of(2025, 6, 1), LocalDate.of(2025, 6, 30));
        assertEquals(0, BigDecimal.valueOf(500).compareTo(dto.totalFaturado()));

        var vazio = service.faturamentoPorPeriodo(LocalDate.of(2024, 1, 1), LocalDate.of(2024, 12, 31));
        assertEquals(BigDecimal.ZERO, vazio.totalFaturado());
    }

    @Test
    @DisplayName("perdasComCancelamentos — total geral de cancelamentos")
    void totalPerdasComCancelamentos() {
        seed();
        persistirConsulta(LocalDate.of(2025, 1, 1), BigDecimal.valueOf(100), StatusConsulta.CANCELADA);
        persistirConsulta(LocalDate.of(2025, 6, 1), BigDecimal.valueOf(250), StatusConsulta.CANCELADA);
        persistirConsulta(LocalDate.of(2025, 7, 1), BigDecimal.valueOf(999), StatusConsulta.REALIZADA);
        em.clear();

        var dto = service.perdasComCancelamentos();
        assertTrue(BigDecimal.valueOf(350).compareTo(dto.totalPerdido()) == 0);
    }
}
