package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultaResumoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorAnoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorEspecialidadeDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorMesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorStatusDto;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Relatório de Consulta")
class RelatorioConsultaServiceImplTest {

    @InjectMocks
    private RelatorioConsultaServiceImpl service;

    @Mock
    private ConsultaRepository repository;


    private Consulta consulta(long id) {
        Medico medico = new Medico("Dra. Ana", "ana@x.com", "11999990000",
                SiglaCrm.SP, "111111", Especialidade.CARDIOLOGIA);
        medico.setId(1L);
        Paciente paciente = new Paciente("João", "joao@x.com", "11999990001",
                "12345678901", Sexo.MASCULINO, LocalDate.of(1990, 1, 1));
        paciente.setId(2L);

        Consulta c = new Consulta(LocalDate.of(2025, 6, 1), LocalTime.of(10, 0),
                Duration.ofMinutes(30), BigDecimal.valueOf(150), "Rotina", medico, paciente);
        c.setId(id);
        c.setStatus(StatusConsulta.AGENDADA);
        return c;
    }


    @Nested
    @DisplayName("gerarRelatorioConsultasPorStatus")
    class PorStatus {

        @Test
        @DisplayName("Deve agrupar totais conhecidos por status")
        void deveAgruparPorStatus() {
            when(repository.contarConsultasPorStatus()).thenReturn(List.<Object[]>of(
                    new Object[]{"AGENDADA", 5L},
                    new Object[]{"CANCELADA", 2L},
                    new Object[]{"REALIZADA", 8L}
            ));

            ConsultasPorStatusDto dto = service.gerarRelatorioConsultasPorStatus();

            assertEquals(5, dto.agendada());
            assertEquals(2, dto.cancelada());
            assertEquals(8, dto.realizada());
        }

        @Test
        @DisplayName("Deve retornar zeros quando não há registros")
        void deveRetornarZerosQuandoVazio() {
            when(repository.contarConsultasPorStatus()).thenReturn(List.of());

            ConsultasPorStatusDto dto = service.gerarRelatorioConsultasPorStatus();

            assertEquals(0, dto.agendada());
            assertEquals(0, dto.cancelada());
            assertEquals(0, dto.realizada());
        }

        @Test
        @DisplayName("Deve ignorar status desconhecido sem quebrar")
        void deveIgnorarStatusDesconhecido() {
            when(repository.contarConsultasPorStatus()).thenReturn(List.<Object[]>of(
                    new Object[]{"AGENDADA", 3L},
                    new Object[]{"INVALIDO", 99L}
            ));

            ConsultasPorStatusDto dto = service.gerarRelatorioConsultasPorStatus();

            assertEquals(3, dto.agendada());
            assertEquals(0, dto.cancelada());
            assertEquals(0, dto.realizada());
        }
    }


    @Nested
    @DisplayName("consultasPorMes")
    class PorMes {

        @Test
        @DisplayName("Deve mapear (mes, total) preservando ordem")
        void deveMapear() {
            when(repository.contarConsultasPorMes()).thenReturn(List.<Object[]>of(
                    new Object[]{1, 4L},
                    new Object[]{2, 7L}
            ));

            List<ConsultasPorMesDto> dtos = service.consultasPorMes();

            assertEquals(2, dtos.size());
            assertEquals(new ConsultasPorMesDto(1, 4), dtos.get(0));
            assertEquals(new ConsultasPorMesDto(2, 7), dtos.get(1));
        }

        @Test
        @DisplayName("Deve retornar lista vazia quando não há registros")
        void deveRetornarVazio() {
            when(repository.contarConsultasPorMes()).thenReturn(List.of());

            assertTrue(service.consultasPorMes().isEmpty());
        }
    }


    @Nested
    @DisplayName("consultasPorAno")
    class PorAno {

        @Test
        @DisplayName("Deve mapear (ano, total)")
        void deveMapear() {
            when(repository.contarConsultasPorAno()).thenReturn(List.<Object[]>of(
                    new Object[]{2024, 100L},
                    new Object[]{2025, 200L}
            ));

            List<ConsultasPorAnoDto> dtos = service.consultasPorAno();

            assertEquals(2, dtos.size());
            assertEquals(2024, dtos.get(0).ano());
            assertEquals(200, dtos.get(1).total());
        }

        @Test
        @DisplayName("Deve retornar lista vazia quando não há registros")
        void deveRetornarVazio() {
            when(repository.contarConsultasPorAno()).thenReturn(List.of());
            assertTrue(service.consultasPorAno().isEmpty());
        }
    }


    @Nested
    @DisplayName("consultasPorEspecialidade")
    class PorEspecialidade {

        @Test
        @DisplayName("Deve mapear (especialidade, total)")
        void deveMapear() {
            when(repository.contarConsultasPorEspecialidade()).thenReturn(List.<Object[]>of(
                    new Object[]{Especialidade.CARDIOLOGIA, 12L}
            ));

            List<ConsultasPorEspecialidadeDto> dtos = service.consultasPorEspecialidade();

            assertEquals(1, dtos.size());
            assertEquals(Especialidade.CARDIOLOGIA, dtos.get(0).especialidade());
            assertEquals(12, dtos.get(0).total());
        }
    }


    @Nested
    @DisplayName("consultasPorPaciente / consultasPorMedico / consultasPorPeriodo")
    class PorEntidade {

        @Test
        @DisplayName("Deve mapear consultas do paciente para ConsultaResumoDto")
        void deveMapearConsultasPorPaciente() {
            when(repository.findByPacienteId(2L)).thenReturn(List.of(consulta(10L)));

            List<ConsultaResumoDto> dtos = service.consultasPorPaciente(2L);

            assertEquals(1, dtos.size());
            assertEquals(10L, dtos.get(0).id());
            assertEquals("Dra. Ana", dtos.get(0).medico().getNome());
        }

        @Test
        @DisplayName("Deve mapear consultas do médico para ConsultaResumoDto")
        void deveMapearConsultasPorMedico() {
            when(repository.findByMedicoId(1L)).thenReturn(List.of(consulta(11L)));

            List<ConsultaResumoDto> dtos = service.consultasPorMedico(1L);

            assertEquals(1, dtos.size());
            assertEquals(11L, dtos.get(0).id());
        }

        @Test
        @DisplayName("Deve mapear consultas no período para ConsultaResumoDto")
        void deveMapearConsultasPorPeriodo() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 12, 31);
            when(repository.findByDataAtendimentoBetween(inicio, fim)).thenReturn(List.of(consulta(12L)));

            List<ConsultaResumoDto> dtos = service.consultasPorPeriodo(inicio, fim);

            assertEquals(1, dtos.size());
            assertEquals(12L, dtos.get(0).id());
        }

        @Test
        @DisplayName("Deve retornar lista vazia quando paciente não tem consultas")
        void deveRetornarVazioParaPaciente() {
            when(repository.findByPacienteId(99L)).thenReturn(List.of());
            assertTrue(service.consultasPorPaciente(99L).isEmpty());
        }
    }
}
