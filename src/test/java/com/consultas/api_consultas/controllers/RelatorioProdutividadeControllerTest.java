package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.MediaConsultasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TaxaComparecimentoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TempoMedioDuracaoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TempoMedioEsperaDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TotalConsultasRealizadasNoMesDto;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.export.implementations.CsvExporter;
import com.consultas.api_consultas.export.implementations.PdfExporter;
import com.consultas.api_consultas.export.implementations.RelatorioExportServiceImpl;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.RelatorioProdutividadeService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(RelatorioProdutividadeController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class,
         RelatorioExportServiceImpl.class, CsvExporter.class, PdfExporter.class})
@ActiveProfiles("test")
class RelatorioProdutividadeControllerTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private RelatorioProdutividadeService service;


    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /consultas-por-mes — 200 com status default REALIZADA")
    void deveListarConsultasPorMes() throws Exception {
        when(service.totalConsultasPorMes(StatusConsulta.REALIZADA)).thenReturn(List.of(
                new TotalConsultasRealizadasNoMesDto(2025, 5, 12)
        ));

        mvc.perform(get("/relatorios/produtividade/consultas-por-mes"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].totalConsultas").value(12));
    }

    @Test
    @WithMockUser(roles = "RECEPCIONISTA")
    @DisplayName("GET /media-consultas — 200")
    void deveRetornarMediaConsultas() throws Exception {
        when(service.mediaConsultas()).thenReturn(new MediaConsultasDto(2.5, 17.5, 75.0));

        mvc.perform(get("/relatorios/produtividade/media-consultas"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.porDia").value(2.5))
                .andExpect(jsonPath("$.porSemana").value(17.5));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /tempo-medio-duracao — 200")
    void deveRetornarTempoMedioDuracao() throws Exception {
        when(service.tempoMedioDuracao()).thenReturn(new TempoMedioDuracaoDto(35.0));

        mvc.perform(get("/relatorios/produtividade/tempo-medio-duracao"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.minutos").value(35.0));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /tempo-medio-espera — 200")
    void deveRetornarTempoMedioEspera() throws Exception {
        when(service.tempoMedioEspera()).thenReturn(new TempoMedioEsperaDto(7.5));

        mvc.perform(get("/relatorios/produtividade/tempo-medio-espera"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.dias").value(7.5));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /taxa-comparecimento — 200")
    void deveRetornarTaxaComparecimento() throws Exception {
        when(service.taxaComparecimento()).thenReturn(new TaxaComparecimentoDto(85.5));

        mvc.perform(get("/relatorios/produtividade/taxa-comparecimento"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.percentual").value(85.5));
    }

    @Test
    @WithMockUser(roles = "PACIENTE")
    @DisplayName("Deve retornar 403 quando paciente acessa relatórios de produtividade")
    void deveRetornar403QuandoPaciente() throws Exception {
        mvc.perform(get("/relatorios/produtividade/media-consultas"))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("Deve retornar 401 quando não autenticado")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/relatorios/produtividade/media-consultas"))
                .andExpect(status().isUnauthorized());
    }


    // --- Export ---

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("GET /media-consultas?formato=csv — objeto único vira tabela chave/valor")
    void deveExportarMediaConsultasComoChaveValor() throws Exception {
        when(service.mediaConsultas()).thenReturn(new MediaConsultasDto(2.5, 17.5, 75.0));

        mvc.perform(get("/relatorios/produtividade/media-consultas").param("formato", "csv"))
                .andExpect(status().isOk())
                .andExpect(header().string("Content-Type", containsString("text/csv")))
                .andExpect(header().string("Content-Disposition", containsString("media-de-consultas-")))
                .andExpect(content().string(containsString("Campo;Valor")))
                .andExpect(content().string(containsString("Por Dia;2,50")));
    }
}
