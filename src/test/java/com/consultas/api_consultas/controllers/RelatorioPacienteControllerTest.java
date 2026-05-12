package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.respostas.PessoaResumo;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.CancelamentosPorPacienteDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.DistribuicaoPacientesPorFaixaEtariaDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.DistribuicaoPacientesPorSexoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.HistoricoConsultaPacienteDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.PacientesComMaisConsultasDto;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.export.implementations.CsvExporter;
import com.consultas.api_consultas.export.implementations.PdfExporter;
import com.consultas.api_consultas.export.implementations.RelatorioExportServiceImpl;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.RelatorioPacienteService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(RelatorioPacienteController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class,
         RelatorioExportServiceImpl.class, CsvExporter.class, PdfExporter.class})
@ActiveProfiles("test")
class RelatorioPacienteControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private RelatorioPacienteService service;


    @Test
    @WithMockUser(roles = "PACIENTE")
    @DisplayName("GET /historico/{id} — 200 quando paciente acessa (regra especial)")
    void devePermitirPacienteVerHistorico() throws Exception {
        when(service.historicoPorPaciente(7L)).thenReturn(List.of(
                new HistoricoConsultaPacienteDto(
                        1L, LocalDate.of(2025, 6, 1), LocalTime.of(10, 0),
                        StatusConsulta.REALIZADA, new PessoaResumo(1L, "Dra. Ana")
                )
        ));

        mvc.perform(get("/relatorios/paciente/historico/{id}", 7L))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].idConsulta").value(1));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /cancelamentos — 200")
    void deveListarCancelamentos() throws Exception {
        when(service.cancelamentosPorPaciente()).thenReturn(List.of(
                new CancelamentosPorPacienteDto(2L, "João", 3)
        ));

        mvc.perform(get("/relatorios/paciente/cancelamentos"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].totalCancelamentos").value(3));
    }

    @Test
    @WithMockUser(roles = "RECEPCIONISTA")
    @DisplayName("GET /mais-consultas — 200")
    void deveListarMaisConsultasNoPeriodo() throws Exception {
        when(service.pacientesComMaisConsultasPorPeriodo(LocalDate.of(2025, 1, 1), LocalDate.of(2025, 12, 31)))
                .thenReturn(List.of(new PacientesComMaisConsultasDto(2L, "João", 5)));

        mvc.perform(get("/relatorios/paciente/mais-consultas")
                        .param("inicio", "2025-01-01")
                        .param("fim", "2025-12-31"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].nomePaciente").value("João"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /distribuicao-sexo — 200")
    void deveListarDistribuicaoSexo() throws Exception {
        when(service.distribuicaoPorSexo()).thenReturn(List.of(
                new DistribuicaoPacientesPorSexoDto(Sexo.FEMININO, 10)
        ));

        mvc.perform(get("/relatorios/paciente/distribuicao-sexo"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].sexo").value("FEMININO"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /distribuicao-faixa-etaria — 200")
    void deveListarDistribuicaoFaixaEtaria() throws Exception {
        when(service.distribuicaoPorFaixaEtaria()).thenReturn(List.of(
                new DistribuicaoPacientesPorFaixaEtariaDto("30-39", 5)
        ));

        mvc.perform(get("/relatorios/paciente/distribuicao-faixa-etaria"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].faixaEtaria").value("30-39"));
    }

    @Test
    @WithMockUser(roles = "PACIENTE")
    @DisplayName("Deve retornar 403 quando paciente acessa cancelamentos (não é histórico próprio)")
    void deveRetornar403QuandoPacienteAcessaCancelamentos() throws Exception {
        mvc.perform(get("/relatorios/paciente/cancelamentos"))
                .andExpect(status().isForbidden());
    }

    @Test
    @WithMockUser(roles = "MEDICO")
    @DisplayName("Deve retornar 403 quando médico acessa relatórios de paciente")
    void deveRetornar403QuandoMedico() throws Exception {
        mvc.perform(get("/relatorios/paciente/historico/{id}", 7L))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("Deve retornar 401 quando não autenticado")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/relatorios/paciente/cancelamentos"))
                .andExpect(status().isUnauthorized());
    }


    // --- Export ---

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /distribuicao-sexo?formato=csv — devolve CSV com header e enum como texto")
    void deveExportarDistribuicaoPorSexoEmCsv() throws Exception {
        when(service.distribuicaoPorSexo()).thenReturn(List.of(
                new DistribuicaoPacientesPorSexoDto(Sexo.FEMININO, 10)
        ));
        mvc.perform(get("/relatorios/paciente/distribuicao-sexo").param("formato", "csv"))
                .andExpect(status().isOk())
                .andExpect(header().string("Content-Type", containsString("text/csv")))
                .andExpect(content().string(containsString("Sexo;Total de Pacientes")))
                .andExpect(content().string(containsString("FEMININO;10")));
    }
}
