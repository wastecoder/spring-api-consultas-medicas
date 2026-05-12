package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.ConsultasRealizadasPorMedicoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.FaturamentoPorMedicoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.MedicosComMaisConsultasNoMesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.MedicosPorEspecialidadeDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.TaxaCancelamentoPorMedicoDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.export.implementations.CsvExporter;
import com.consultas.api_consultas.export.implementations.PdfExporter;
import com.consultas.api_consultas.export.implementations.RelatorioExportServiceImpl;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.RelatorioMedicoService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.util.List;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(RelatorioMedicoController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class,
         RelatorioExportServiceImpl.class, CsvExporter.class, PdfExporter.class})
@ActiveProfiles("test")
class RelatorioMedicoControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private RelatorioMedicoService service;


    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /consultas-realizadas — 200")
    void deveListarConsultasRealizadas() throws Exception {
        when(service.consultasRealizadasPorMedico()).thenReturn(List.of(
                new ConsultasRealizadasPorMedicoDto(1L, "Dra. Ana", 12)
        ));

        mvc.perform(get("/relatorios/medico/consultas-realizadas"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].nome").value("Dra. Ana"));
    }

    @Test
    @WithMockUser(roles = "RECEPCIONISTA")
    @DisplayName("GET /mais-consultas-no-mes — 200 com mes/ano válidos")
    void deveListarMaisConsultasNoMes() throws Exception {
        when(service.medicosComMaisConsultasNoMes(5, 2025)).thenReturn(List.of(
                new MedicosComMaisConsultasNoMesDto(1L, "Dra. Ana", 7)
        ));

        mvc.perform(get("/relatorios/medico/mais-consultas-no-mes")
                        .param("mes", "5")
                        .param("ano", "2025"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].total").value(7));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /mais-consultas-no-mes — 400 quando mes inválido (>12)")
    void deveRetornar400QuandoMesInvalido() throws Exception {
        mvc.perform(get("/relatorios/medico/mais-consultas-no-mes")
                        .param("mes", "13")
                        .param("ano", "2025"))
                .andExpect(status().isBadRequest());
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /por-especialidade — 200")
    void deveListarPorEspecialidade() throws Exception {
        when(service.medicosPorEspecialidade()).thenReturn(List.of(
                new MedicosPorEspecialidadeDto(Especialidade.PEDIATRIA, 3)
        ));

        mvc.perform(get("/relatorios/medico/por-especialidade"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].especialidade").value("PEDIATRIA"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /taxa-cancelamento — 200")
    void deveListarTaxaCancelamento() throws Exception {
        when(service.taxaCancelamentoPorMedico()).thenReturn(List.of(
                new TaxaCancelamentoPorMedicoDto(1L, "Dra. Ana", "10,00%")
        ));

        mvc.perform(get("/relatorios/medico/taxa-cancelamento"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].taxa").value("10,00%"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /faturamento — 200")
    void deveListarFaturamento() throws Exception {
        when(service.faturamentoPorMedico()).thenReturn(List.of(
                new FaturamentoPorMedicoDto(1L, "Dra. Ana", BigDecimal.valueOf(900.00))
        ));

        mvc.perform(get("/relatorios/medico/faturamento"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].total").value(900.00));
    }

    @Test
    @WithMockUser(roles = "PACIENTE")
    @DisplayName("Deve retornar 403 quando paciente acessa relatórios de médico")
    void deveRetornar403QuandoPaciente() throws Exception {
        mvc.perform(get("/relatorios/medico/consultas-realizadas"))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("Deve retornar 401 quando não autenticado")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/relatorios/medico/consultas-realizadas"))
                .andExpect(status().isUnauthorized());
    }


    // --- Export ---

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /mais-consultas-no-mes?formato=csv — devolve CSV com Mês/Ano no nome do arquivo")
    void deveExportarMaisConsultasNoMesEmCsv() throws Exception {
        when(service.medicosComMaisConsultasNoMes(5, 2025)).thenReturn(List.of(
                new MedicosComMaisConsultasNoMesDto(1L, "Dra. Ana", 7)
        ));
        mvc.perform(get("/relatorios/medico/mais-consultas-no-mes")
                        .param("mes", "5").param("ano", "2025").param("formato", "csv"))
                .andExpect(status().isOk())
                .andExpect(header().string("Content-Type", containsString("text/csv")))
                .andExpect(header().string("Content-Disposition", containsString("medicos-com-mais-consultas-no-mes-")))
                .andExpect(content().string(containsString("Dra. Ana;7")));
    }
}
