package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.respostas.PessoaResumo;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultaResumoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorAnoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorEspecialidadeDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorMesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.ConsultasPorStatusDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.RelatorioConsultaService;
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

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(RelatorioConsultaController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class})
@ActiveProfiles("test")
class RelatorioConsultaControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private RelatorioConsultaService service;


    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /consultas-por-status — 200 com totais por status")
    void deveRetornarConsultasPorStatus() throws Exception {
        when(service.gerarRelatorioConsultasPorStatus())
                .thenReturn(new ConsultasPorStatusDto(5, 2, 8));

        mvc.perform(get("/relatorios/consulta/consultas-por-status"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.agendada").value(5))
                .andExpect(jsonPath("$.cancelada").value(2))
                .andExpect(jsonPath("$.realizada").value(8));
    }

    @Test
    @WithMockUser(roles = "RECEPCIONISTA")
    @DisplayName("GET /por-mes — 200 paginado")
    void deveListarPorMes() throws Exception {
        when(service.consultasPorMes()).thenReturn(List.of(
                new ConsultasPorMesDto(1, 4),
                new ConsultasPorMesDto(2, 7)
        ));

        mvc.perform(get("/relatorios/consulta/por-mes"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].mes").value(1))
                .andExpect(jsonPath("$.content[0].total").value(4))
                .andExpect(jsonPath("$.totalElements").value(2));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /por-ano — 200 paginado")
    void deveListarPorAno() throws Exception {
        when(service.consultasPorAno()).thenReturn(List.of(new ConsultasPorAnoDto(2025, 100)));

        mvc.perform(get("/relatorios/consulta/por-ano"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].ano").value(2025));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /por-especialidade — 200 paginado")
    void deveListarPorEspecialidade() throws Exception {
        when(service.consultasPorEspecialidade()).thenReturn(List.of(
                new ConsultasPorEspecialidadeDto(Especialidade.CARDIOLOGIA, 12)
        ));

        mvc.perform(get("/relatorios/consulta/por-especialidade"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].especialidade").value("CARDIOLOGIA"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /por-paciente/{id} — 200 paginado")
    void deveListarPorPaciente() throws Exception {
        ConsultaResumoDto dto = new ConsultaResumoDto(
                1L, LocalDate.of(2025, 10, 5), LocalTime.of(10, 0),
                StatusConsulta.REALIZADA,
                new PessoaResumo(1L, "Dra. Ana"),
                new PessoaResumo(2L, "João")
        );
        when(service.consultasPorPaciente(2L)).thenReturn(List.of(dto));

        mvc.perform(get("/relatorios/consulta/por-paciente/{id}", 2L))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].id").value(1));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /por-medico/{id} — 200 paginado")
    void deveListarPorMedico() throws Exception {
        when(service.consultasPorMedico(1L)).thenReturn(List.of());

        mvc.perform(get("/relatorios/consulta/por-medico/{id}", 1L))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(0));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /por-periodo — 200 paginado")
    void deveListarPorPeriodo() throws Exception {
        when(service.consultasPorPeriodo(LocalDate.of(2025, 1, 1), LocalDate.of(2025, 12, 31)))
                .thenReturn(List.of());

        mvc.perform(get("/relatorios/consulta/por-periodo")
                        .param("inicio", "2025-01-01")
                        .param("fim", "2025-12-31"))
                .andExpect(status().isOk());
    }

    @Test
    @WithMockUser(roles = "PACIENTE")
    @DisplayName("Deve retornar 403 quando paciente acessa relatórios de consulta")
    void deveRetornar403QuandoPaciente() throws Exception {
        mvc.perform(get("/relatorios/consulta/consultas-por-status"))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("Deve retornar 401 quando não autenticado")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/relatorios/consulta/consultas-por-status"))
                .andExpect(status().isUnauthorized());
    }
}
