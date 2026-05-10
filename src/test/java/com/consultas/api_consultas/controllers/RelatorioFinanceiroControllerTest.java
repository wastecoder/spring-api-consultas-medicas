package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoMensalDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoPorEspecialidadeDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoPorMedicoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.FaturamentoPorPeriodoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdaMensalCancelamentoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdaPorPeriodoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.PerdasComCancelamentosDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.RelatorioFinanceiroService;
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
import java.time.LocalDate;
import java.util.List;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(RelatorioFinanceiroController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class})
@ActiveProfiles("test")
class RelatorioFinanceiroControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private RelatorioFinanceiroService service;


    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /faturamento-mensal — 200 paginado")
    void deveListarFaturamentoMensal() throws Exception {
        when(service.faturamentoMensal()).thenReturn(List.of(
                new FaturamentoMensalDto(2025, 1, BigDecimal.valueOf(1500.00))
        ));

        mvc.perform(get("/relatorios/financeiro/faturamento-mensal"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].ano").value(2025))
                .andExpect(jsonPath("$.content[0].mes").value(1))
                .andExpect(jsonPath("$.content[0].totalFaturado").value(1500.00));
    }

    @Test
    @WithMockUser(roles = "RECEPCIONISTA")
    @DisplayName("GET /faturamento-por-medico — 200")
    void deveListarFaturamentoPorMedico() throws Exception {
        when(service.faturamentoPorMedico()).thenReturn(List.of(
                new FaturamentoPorMedicoDto(1L, "Dra. Ana", BigDecimal.valueOf(900.00))
        ));

        mvc.perform(get("/relatorios/financeiro/faturamento-por-medico"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].idMedico").value(1));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /faturamento-por-especialidade — 200")
    void deveListarFaturamentoPorEspecialidade() throws Exception {
        when(service.faturamentoPorEspecialidade()).thenReturn(List.of(
                new FaturamentoPorEspecialidadeDto(Especialidade.CARDIOLOGIA, BigDecimal.valueOf(2500.00))
        ));

        mvc.perform(get("/relatorios/financeiro/faturamento-por-especialidade"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].especialidadeMedica").value("CARDIOLOGIA"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /faturamento-por-periodo — 200")
    void deveRetornarFaturamentoPorPeriodo() throws Exception {
        when(service.faturamentoPorPeriodo(LocalDate.of(2025, 1, 1), LocalDate.of(2025, 12, 31)))
                .thenReturn(new FaturamentoPorPeriodoDto(BigDecimal.valueOf(7000.00)));

        mvc.perform(get("/relatorios/financeiro/faturamento-por-periodo")
                        .param("inicio", "2025-01-01")
                        .param("fim", "2025-12-31"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalFaturado").value(7000.00));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /perdas-com-cancelamentos — 200")
    void deveRetornarPerdasComCancelamentos() throws Exception {
        when(service.perdasComCancelamentos())
                .thenReturn(new PerdasComCancelamentosDto(BigDecimal.valueOf(450.00)));

        mvc.perform(get("/relatorios/financeiro/perdas-com-cancelamentos"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalPerdido").value(450.00));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /perda-mensal-com-cancelamentos — 200")
    void deveListarPerdaMensal() throws Exception {
        when(service.perdaMensalComCancelamentos()).thenReturn(List.of(
                new PerdaMensalCancelamentoDto(2025, 3, BigDecimal.valueOf(300.00))
        ));

        mvc.perform(get("/relatorios/financeiro/perda-mensal-com-cancelamentos"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].mes").value(3));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /perda-por-periodo — 200")
    void deveRetornarPerdaPorPeriodo() throws Exception {
        when(service.perdaPorPeriodo(LocalDate.of(2025, 1, 1), LocalDate.of(2025, 6, 30)))
                .thenReturn(new PerdaPorPeriodoDto(BigDecimal.valueOf(120.00)));

        mvc.perform(get("/relatorios/financeiro/perda-por-periodo")
                        .param("inicio", "2025-01-01")
                        .param("fim", "2025-06-30"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalPerdido").value(120.00));
    }

    @Test
    @WithMockUser(roles = "MEDICO")
    @DisplayName("Deve retornar 403 quando médico acessa relatório financeiro")
    void deveRetornar403QuandoMedico() throws Exception {
        mvc.perform(get("/relatorios/financeiro/perdas-com-cancelamentos"))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("Deve retornar 401 quando não autenticado")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/relatorios/financeiro/perdas-com-cancelamentos"))
                .andExpect(status().isUnauthorized());
    }
}
