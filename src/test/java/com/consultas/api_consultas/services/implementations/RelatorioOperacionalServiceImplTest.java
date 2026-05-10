package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Relatório Operacional")
class RelatorioOperacionalServiceImplTest {

    @InjectMocks
    private RelatorioOperacionalServiceImpl service;

    @Mock
    private ConsultaRepository consultaRepository;

    @Mock
    private MedicoRepository medicoRepository;


    @Nested
    @DisplayName("consultasPorData")
    class PorData {

        @Test
        @DisplayName("Deve usar a data informada e mapear o resultado")
        void deveUsarDataInformada() {
            LocalDate data = LocalDate.of(2025, 6, 1);
            when(consultaRepository.buscarConsultasPorData(data)).thenReturn(List.<Object[]>of(
                    new Object[]{1L, LocalTime.of(10, 0), "Dra. Ana", "João", StatusConsulta.AGENDADA}
            ));

            List<ConsultasPorDataDto> dtos = service.consultasPorData(data);

            assertEquals(1, dtos.size());
            assertEquals(new ConsultasPorDataDto(1L, LocalTime.of(10, 0), "Dra. Ana", "João", StatusConsulta.AGENDADA), dtos.get(0));
        }

        @Test
        @DisplayName("Deve usar hoje quando data não informada")
        void deveUsarHojeQuandoNull() {
            when(consultaRepository.buscarConsultasPorData(any())).thenReturn(List.of());
            assertEquals(0, service.consultasPorData(null).size());
        }

        @Test
        @DisplayName("Deve lançar quando data fora dos limites")
        void deveLancarQuandoDataInvalida() {
            assertThrows(IllegalArgumentException.class, () ->
                    service.consultasPorData(LocalDate.of(1999, 12, 31)));
        }
    }


    @Nested
    @DisplayName("consultasProximosDias")
    class ProximosDias {

        @Test
        @DisplayName("Deve mapear consultas próximas")
        void deveMapear() {
            LocalDate data = LocalDate.now().plusDays(2);
            when(consultaRepository.buscarConsultasProximosDias(any(), any())).thenReturn(List.<Object[]>of(
                    new Object[]{1L, data, "Dra. Ana", "João", StatusConsulta.AGENDADA}
            ));

            List<ConsultasProximosDiasDto> dtos = service.consultasProximosDias();
            assertEquals(1L, dtos.get(0).idConsulta());
        }
    }


    @Nested
    @DisplayName("consultasPendentes")
    class Pendentes {

        @Test
        @DisplayName("Deve mapear consultas agendadas no passado")
        void deveMapear() {
            LocalDate data = LocalDate.now().minusDays(3);
            when(consultaRepository.buscarConsultasPendentes()).thenReturn(List.<Object[]>of(
                    new Object[]{1L, data, "Dra. Ana", "João"}
            ));

            List<ConsultasPendentesDto> dtos = service.consultasPendentes();
            assertEquals(new ConsultasPendentesDto(1L, data, "Dra. Ana", "João"), dtos.get(0));
        }
    }


    @Nested
    @DisplayName("medicosSemAgendamento")
    class SemAgendamento {

        @Test
        @DisplayName("Deve usar mes/ano informados")
        void deveUsarMesAnoInformados() {
            when(medicoRepository.buscarMedicosSemAgendamentoNoMes(2025, 5)).thenReturn(List.<Object[]>of(
                    new Object[]{1L, "Dra. Ana", Especialidade.CARDIOLOGIA}
            ));

            List<MedicoSemAgendamentoDto> dtos = service.medicosSemAgendamento(2025, 5);
            assertEquals(Especialidade.CARDIOLOGIA, dtos.get(0).especialidadeMedica());
        }

        @Test
        @DisplayName("Deve usar mes/ano atuais quando null")
        void deveUsarMesAnoAtuais() {
            when(medicoRepository.buscarMedicosSemAgendamentoNoMes(anyIntOk(), anyIntOk())).thenReturn(List.of());
            assertEquals(0, service.medicosSemAgendamento(null, null).size());
        }

        @Test
        @DisplayName("Deve lançar quando mes inválido")
        void deveLancarQuandoMesInvalido() {
            assertThrows(IllegalArgumentException.class, () -> service.medicosSemAgendamento(2025, 13));
        }

        @Test
        @DisplayName("Deve lançar quando ano fora do limite")
        void deveLancarQuandoAnoInvalido() {
            assertThrows(IllegalArgumentException.class, () -> service.medicosSemAgendamento(1999, 1));
        }

        private int anyIntOk() {
            return org.mockito.ArgumentMatchers.anyInt();
        }
    }
}
