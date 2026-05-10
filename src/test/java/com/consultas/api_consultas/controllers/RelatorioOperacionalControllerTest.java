package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.RelatorioOperacionalService;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(RelatorioOperacionalController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class})
@ActiveProfiles("test")
class RelatorioOperacionalControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private RelatorioOperacionalService service;


    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /consultas-por-data — 200 sem parâmetro de data (usa hoje)")
    void deveListarConsultasPorData() throws Exception {
        when(service.consultasPorData(any())).thenReturn(List.of(
                new ConsultasPorDataDto(1L, LocalTime.of(10, 0), "Dra. Ana", "João", StatusConsulta.AGENDADA)
        ));

        mvc.perform(get("/relatorios/operacional/consultas-por-data"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].idConsulta").value(1));
    }

    @Test
    @WithMockUser(roles = "RECEPCIONISTA")
    @DisplayName("GET /consultas-proximos-7-dias — 200")
    void deveListarProximos7Dias() throws Exception {
        when(service.consultasProximosDias()).thenReturn(List.of(
                new ConsultasProximosDiasDto(1L, LocalDate.now().plusDays(2), "Dra. Ana", "João", StatusConsulta.AGENDADA)
        ));

        mvc.perform(get("/relatorios/operacional/consultas-proximos-7-dias"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /consultas-pendentes — 200")
    void deveListarPendentes() throws Exception {
        when(service.consultasPendentes()).thenReturn(List.of(
                new ConsultasPendentesDto(1L, LocalDate.now().minusDays(3), "Dra. Ana", "João")
        ));

        mvc.perform(get("/relatorios/operacional/consultas-pendentes"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].nomeMedico").value("Dra. Ana"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /medicos-sem-agendamento — 200")
    void deveListarMedicosSemAgendamento() throws Exception {
        when(service.medicosSemAgendamento(any(), any())).thenReturn(List.of(
                new MedicoSemAgendamentoDto(1L, "Dra. Ana", Especialidade.CARDIOLOGIA)
        ));

        mvc.perform(get("/relatorios/operacional/medicos-sem-agendamento"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].especialidadeMedica").value("CARDIOLOGIA"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /medicos-sem-agendamento — 400 quando mes inválido")
    void deveRetornar400QuandoMesInvalido() throws Exception {
        mvc.perform(get("/relatorios/operacional/medicos-sem-agendamento")
                        .param("mes", "0")
                        .param("ano", "2025"))
                .andExpect(status().isBadRequest());
    }

    @Test
    @WithMockUser(roles = "MEDICO")
    @DisplayName("Deve retornar 403 quando médico acessa relatório operacional")
    void deveRetornar403QuandoMedico() throws Exception {
        mvc.perform(get("/relatorios/operacional/consultas-pendentes"))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("Deve retornar 401 quando não autenticado")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/relatorios/operacional/consultas-pendentes"))
                .andExpect(status().isUnauthorized());
    }
}
