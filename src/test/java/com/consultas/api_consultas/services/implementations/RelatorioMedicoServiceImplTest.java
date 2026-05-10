package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.ConsultasRealizadasPorMedicoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.FaturamentoPorMedicoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.MedicosComMaisConsultasNoMesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.MedicosPorEspecialidadeDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.TaxaCancelamentoPorMedicoDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.Year;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Relatório de Médico")
class RelatorioMedicoServiceImplTest {

    @InjectMocks
    private RelatorioMedicoServiceImpl service;

    @Mock
    private ConsultaRepository consultaRepository;

    @Mock
    private MedicoRepository medicoRepository;


    @Nested
    @DisplayName("consultasRealizadasPorMedico")
    class ConsultasRealizadas {

        @Test
        @DisplayName("Deve mapear (id, nome, total)")
        void deveMapear() {
            when(consultaRepository.contarConsultasRealizadasPorMedico()).thenReturn(List.<Object[]>of(
                    new Object[]{1L, "Dra. Ana", 12L}
            ));

            List<ConsultasRealizadasPorMedicoDto> dtos = service.consultasRealizadasPorMedico();

            assertEquals(1, dtos.size());
            assertEquals(new ConsultasRealizadasPorMedicoDto(1L, "Dra. Ana", 12), dtos.get(0));
        }

        @Test
        @DisplayName("Deve retornar lista vazia quando não há registros")
        void deveRetornarVazio() {
            when(consultaRepository.contarConsultasRealizadasPorMedico()).thenReturn(List.of());
            assertTrue(service.consultasRealizadasPorMedico().isEmpty());
        }
    }


    @Nested
    @DisplayName("medicosComMaisConsultasNoMes")
    class MaisConsultasNoMes {

        @Test
        @DisplayName("Deve mapear quando mes/ano são válidos")
        void deveMapear() {
            when(consultaRepository.medicosComMaisConsultasNoMes(5, 2025)).thenReturn(List.<Object[]>of(
                    new Object[]{1L, "Dra. Ana", 7L}
            ));

            List<MedicosComMaisConsultasNoMesDto> dtos = service.medicosComMaisConsultasNoMes(5, 2025);
            assertEquals(7, dtos.get(0).total());
        }

        @Test
        @DisplayName("Deve lançar IllegalArgumentException quando mes < 1 ou > 12")
        void deveLancarQuandoMesInvalido() {
            assertThrows(IllegalArgumentException.class, () -> service.medicosComMaisConsultasNoMes(0, 2025));
            assertThrows(IllegalArgumentException.class, () -> service.medicosComMaisConsultasNoMes(13, 2025));
        }

        @Test
        @DisplayName("Deve lançar IllegalArgumentException quando ano < 2000")
        void deveLancarQuandoAnoMuitoAntigo() {
            assertThrows(IllegalArgumentException.class, () -> service.medicosComMaisConsultasNoMes(1, 1999));
        }

        @Test
        @DisplayName("Deve lançar quando ano > anoAtual + 5")
        void deveLancarQuandoAnoMuitoFuturo() {
            int futuroAlem = Year.now().getValue() + 6;
            assertThrows(IllegalArgumentException.class, () -> service.medicosComMaisConsultasNoMes(1, futuroAlem));
        }
    }


    @Nested
    @DisplayName("medicosPorEspecialidade")
    class PorEspecialidade {

        @Test
        @DisplayName("Deve mapear (especialidade, total)")
        void deveMapear() {
            when(medicoRepository.contarMedicosPorEspecialidade()).thenReturn(List.<Object[]>of(
                    new Object[]{Especialidade.PEDIATRIA, 3L}
            ));

            List<MedicosPorEspecialidadeDto> dtos = service.medicosPorEspecialidade();
            assertEquals(Especialidade.PEDIATRIA, dtos.get(0).especialidade());
            assertEquals(3, dtos.get(0).total());
        }
    }


    @Nested
    @DisplayName("taxaCancelamentoPorMedico")
    class TaxaCancelamento {

        @Test
        @DisplayName("Deve formatar a taxa com 2 casas e símbolo de %")
        void deveFormatarTaxa() {
            when(consultaRepository.calcularTaxaCancelamentoPorMedico()).thenReturn(List.<Object[]>of(
                    new Object[]{1L, "Dra. Ana", 10.0}
            ));

            List<TaxaCancelamentoPorMedicoDto> dtos = service.taxaCancelamentoPorMedico();
            // O DecimalFormat usa o locale do sistema; aceitamos "10,00%" (pt-BR) ou "10.00%" (default)
            assertTrue(dtos.get(0).taxa().matches("10[.,]00%"), "taxa deveria bater 10,00% ou 10.00%");
        }
    }


    @Nested
    @DisplayName("faturamentoPorMedico")
    class Faturamento {

        @Test
        @DisplayName("Deve mapear (id, nome, total)")
        void deveMapear() {
            when(consultaRepository.calcularFaturamentoPorMedico()).thenReturn(List.<Object[]>of(
                    new Object[]{1L, "Dra. Ana", BigDecimal.valueOf(900)}
            ));

            List<FaturamentoPorMedicoDto> dtos = service.faturamentoPorMedico();
            assertEquals(BigDecimal.valueOf(900), dtos.get(0).total());
        }
    }
}
