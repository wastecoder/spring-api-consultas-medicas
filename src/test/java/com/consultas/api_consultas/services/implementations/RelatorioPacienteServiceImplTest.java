package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.CancelamentosPorPacienteDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.DistribuicaoPacientesPorFaixaEtariaDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.DistribuicaoPacientesPorSexoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.HistoricoConsultaPacienteDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.PacientesComMaisConsultasDto;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.utils.SecurityUtil;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.access.AccessDeniedException;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Relatório de Paciente")
class RelatorioPacienteServiceImplTest {

    @InjectMocks
    private RelatorioPacienteServiceImpl service;

    @Mock
    private ConsultaRepository consultaRepository;

    @Mock
    private PacienteRepository pacienteRepository;

    @Mock
    private PacienteService pacienteService;

    @Mock
    private SecurityUtil securityUtil;


    private Paciente paciente(long id) {
        Paciente p = new Paciente("João", "joao@x.com", "11999990000",
                "12345678901", Sexo.MASCULINO, LocalDate.of(1990, 1, 1));
        p.setId(id);
        p.setAtivo(true);
        return p;
    }

    private Medico medico(long id) {
        Medico m = new Medico("Dra. Ana", "ana@x.com", "11999990001",
                SiglaCrm.SP, "111111", Especialidade.CARDIOLOGIA);
        m.setId(id);
        m.setAtivo(true);
        return m;
    }


    @Nested
    @DisplayName("historicoPorPaciente")
    class Historico {

        @Test
        @DisplayName("Deve mapear histórico quando usuário pode acessar")
        void devePermitirAcessoEMapear() {
            Paciente paciente = paciente(2L);
            Medico medico = medico(1L);

            when(pacienteService.buscarPorId(2L)).thenReturn(paciente);
            when(securityUtil.canAccessPatient(paciente)).thenReturn(true);
            when(consultaRepository.buscarHistoricoPorPaciente(2L)).thenReturn(List.<Object[]>of(
                    new Object[]{
                            10L,
                            LocalDate.of(2025, 6, 1),
                            LocalTime.of(10, 0),
                            StatusConsulta.REALIZADA,
                            medico
                    }
            ));

            List<HistoricoConsultaPacienteDto> dtos = service.historicoPorPaciente(2L);

            assertEquals(1, dtos.size());
            assertEquals(10L, dtos.get(0).idConsulta());
            assertEquals(StatusConsulta.REALIZADA, dtos.get(0).status());
            assertEquals("Dra. Ana", dtos.get(0).medico().getNome());
        }

        @Test
        @DisplayName("Deve lançar AccessDeniedException quando usuário não pode acessar")
        void deveBloquearAcesso() {
            Paciente paciente = paciente(2L);
            when(pacienteService.buscarPorId(2L)).thenReturn(paciente);
            when(securityUtil.canAccessPatient(paciente)).thenReturn(false);

            assertThrows(AccessDeniedException.class, () -> service.historicoPorPaciente(2L));
        }
    }


    @Nested
    @DisplayName("cancelamentosPorPaciente")
    class Cancelamentos {

        @Test
        @DisplayName("Deve mapear (id, nome, total)")
        void deveMapear() {
            when(consultaRepository.contarCancelamentosPorPaciente()).thenReturn(List.<Object[]>of(
                    new Object[]{2L, "João", 3L}
            ));

            List<CancelamentosPorPacienteDto> dtos = service.cancelamentosPorPaciente();
            assertEquals(new CancelamentosPorPacienteDto(2L, "João", 3), dtos.get(0));
        }

        @Test
        @DisplayName("Deve retornar lista vazia quando não há cancelamentos")
        void deveRetornarVazio() {
            when(consultaRepository.contarCancelamentosPorPaciente()).thenReturn(List.of());
            assertTrue(service.cancelamentosPorPaciente().isEmpty());
        }
    }


    @Nested
    @DisplayName("pacientesComMaisConsultasPorPeriodo")
    class MaisConsultasPorPeriodo {

        @Test
        @DisplayName("Deve mapear (id, nome, total)")
        void deveMapear() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 12, 31);
            when(consultaRepository.pacientesComMaisConsultasPorPeriodo(inicio, fim)).thenReturn(List.<Object[]>of(
                    new Object[]{2L, "João", 5L}
            ));

            List<PacientesComMaisConsultasDto> dtos = service.pacientesComMaisConsultasPorPeriodo(inicio, fim);
            assertEquals("João", dtos.get(0).nomePaciente());
            assertEquals(5, dtos.get(0).totalConsultas());
        }
    }


    @Nested
    @DisplayName("distribuicaoPorSexo / distribuicaoPorFaixaEtaria")
    class Distribuicao {

        @Test
        @DisplayName("Deve mapear distribuição por sexo")
        void deveMapearPorSexo() {
            when(pacienteRepository.distribuicaoPorSexo()).thenReturn(List.<Object[]>of(
                    new Object[]{Sexo.FEMININO, 10L}
            ));

            List<DistribuicaoPacientesPorSexoDto> dtos = service.distribuicaoPorSexo();
            assertEquals(Sexo.FEMININO, dtos.get(0).sexo());
            assertEquals(10, dtos.get(0).totalPacientes());
        }

        @Test
        @DisplayName("Deve mapear distribuição por faixa etária")
        void deveMapearPorFaixaEtaria() {
            when(pacienteRepository.distribuicaoPorFaixaEtaria()).thenReturn(List.<Object[]>of(
                    new Object[]{"30-44", 5L}
            ));

            List<DistribuicaoPacientesPorFaixaEtariaDto> dtos = service.distribuicaoPorFaixaEtaria();
            assertEquals("30-44", dtos.get(0).faixaEtaria());
        }
    }
}
