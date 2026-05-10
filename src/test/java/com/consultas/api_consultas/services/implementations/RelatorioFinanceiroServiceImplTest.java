package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoMensalDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoPorEspecialidadeDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoPorMedicoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoPorPeriodoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdaMensalCancelamentoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdaPorPeriodoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdasComCancelamentosDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Relatório Financeiro")
class RelatorioFinanceiroServiceImplTest {

    @InjectMocks
    private RelatorioFinanceiroServiceImpl service;

    @Mock
    private ConsultaRepository repository;


    @Nested
    @DisplayName("faturamentoMensal")
    class FaturamentoMensal {

        @Test
        @DisplayName("Deve mapear (ano, mes, total)")
        void deveMapear() {
            when(repository.faturamentoMensal()).thenReturn(List.<Object[]>of(
                    new Object[]{2025, 1, BigDecimal.valueOf(1500)},
                    new Object[]{2025, 2, BigDecimal.valueOf(2200)}
            ));

            List<FaturamentoMensalDto> dtos = service.faturamentoMensal();

            assertEquals(2, dtos.size());
            assertEquals(new FaturamentoMensalDto(2025, 1, BigDecimal.valueOf(1500)), dtos.get(0));
            assertEquals(BigDecimal.valueOf(2200), dtos.get(1).totalFaturado());
        }

        @Test
        @DisplayName("Deve retornar lista vazia quando não há registros")
        void deveRetornarVazio() {
            when(repository.faturamentoMensal()).thenReturn(List.of());
            assertTrue(service.faturamentoMensal().isEmpty());
        }
    }


    @Nested
    @DisplayName("faturamentoPorMedico / faturamentoPorEspecialidade")
    class FaturamentoAgregado {

        @Test
        @DisplayName("Deve mapear faturamento por médico")
        void deveMapearPorMedico() {
            when(repository.faturamentoPorMedico()).thenReturn(List.<Object[]>of(
                    new Object[]{1L, "Dra. Ana", BigDecimal.valueOf(900)}
            ));

            List<FaturamentoPorMedicoDto> dtos = service.faturamentoPorMedico();

            assertEquals(1, dtos.size());
            assertEquals(1L, dtos.get(0).idMedico());
            assertEquals("Dra. Ana", dtos.get(0).nomeMedico());
        }

        @Test
        @DisplayName("Deve mapear faturamento por especialidade")
        void deveMapearPorEspecialidade() {
            when(repository.faturamentoPorEspecialidade()).thenReturn(List.<Object[]>of(
                    new Object[]{Especialidade.CARDIOLOGIA, BigDecimal.valueOf(2500)}
            ));

            List<FaturamentoPorEspecialidadeDto> dtos = service.faturamentoPorEspecialidade();

            assertEquals(Especialidade.CARDIOLOGIA, dtos.get(0).especialidadeMedica());
        }
    }


    @Nested
    @DisplayName("faturamentoPorPeriodo")
    class FaturamentoPorPeriodo {

        @Test
        @DisplayName("Deve devolver total quando repositório retorna valor")
        void deveRetornarTotal() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 12, 31);
            when(repository.faturamentoPorPeriodo(inicio, fim)).thenReturn(BigDecimal.valueOf(7000));

            FaturamentoPorPeriodoDto dto = service.faturamentoPorPeriodo(inicio, fim);

            assertEquals(BigDecimal.valueOf(7000), dto.totalFaturado());
        }

        @Test
        @DisplayName("Deve devolver ZERO quando repositório retorna null")
        void deveDevolverZeroQuandoNull() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 12, 31);
            when(repository.faturamentoPorPeriodo(inicio, fim)).thenReturn(null);

            assertEquals(BigDecimal.ZERO, service.faturamentoPorPeriodo(inicio, fim).totalFaturado());
        }

        @Test
        @DisplayName("Deve lançar IllegalArgumentException quando inicio > fim")
        void deveLancarQuandoInicioMaiorQueFim() {
            assertThrows(IllegalArgumentException.class, () ->
                    service.faturamentoPorPeriodo(LocalDate.of(2025, 6, 1), LocalDate.of(2025, 1, 1)));
        }

        @Test
        @DisplayName("Deve lançar quando datas são null")
        void deveLancarQuandoNull() {
            assertThrows(IllegalArgumentException.class, () ->
                    service.faturamentoPorPeriodo(null, LocalDate.of(2025, 1, 1)));
        }

        @Test
        @DisplayName("Deve lançar quando inicio antes do limite inferior")
        void deveLancarQuandoForaDoLimiteInferior() {
            assertThrows(IllegalArgumentException.class, () ->
                    service.faturamentoPorPeriodo(LocalDate.of(1999, 12, 31), LocalDate.of(2025, 1, 1)));
        }

        @Test
        @DisplayName("Deve lançar quando fim depois do limite superior (now + 5 anos)")
        void deveLancarQuandoForaDoLimiteSuperior() {
            LocalDate fimAlemDoLimite = LocalDate.now().plusYears(6);
            assertThrows(IllegalArgumentException.class, () ->
                    service.faturamentoPorPeriodo(LocalDate.of(2025, 1, 1), fimAlemDoLimite));
        }
    }


    @Nested
    @DisplayName("perdasComCancelamentos / perdaMensal / perdaPorPeriodo")
    class Perdas {

        @Test
        @DisplayName("Deve devolver total perdido")
        void deveRetornarPerdas() {
            when(repository.perdasComCancelamentos()).thenReturn(BigDecimal.valueOf(450));

            PerdasComCancelamentosDto dto = service.perdasComCancelamentos();
            assertEquals(BigDecimal.valueOf(450), dto.totalPerdido());
        }

        @Test
        @DisplayName("Deve devolver ZERO quando repositório retorna null para perdas totais")
        void deveDevolverZeroQuandoPerdasNull() {
            when(repository.perdasComCancelamentos()).thenReturn(null);
            assertEquals(BigDecimal.ZERO, service.perdasComCancelamentos().totalPerdido());
        }

        @Test
        @DisplayName("Deve mapear perda mensal por (ano, mes, total)")
        void deveMapearPerdaMensal() {
            when(repository.perdaMensalComCancelamentos()).thenReturn(List.<Object[]>of(
                    new Object[]{2025, 3, BigDecimal.valueOf(300)}
            ));

            List<PerdaMensalCancelamentoDto> dtos = service.perdaMensalComCancelamentos();
            assertEquals(new PerdaMensalCancelamentoDto(2025, 3, BigDecimal.valueOf(300)), dtos.get(0));
        }

        @Test
        @DisplayName("Deve devolver ZERO quando perda por período retorna null")
        void deveDevolverZeroPerdaPorPeriodo() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 6, 30);
            when(repository.perdaPorPeriodo(inicio, fim)).thenReturn(null);

            PerdaPorPeriodoDto dto = service.perdaPorPeriodo(inicio, fim);
            assertEquals(BigDecimal.ZERO, dto.totalPerdido());
        }
    }
}
