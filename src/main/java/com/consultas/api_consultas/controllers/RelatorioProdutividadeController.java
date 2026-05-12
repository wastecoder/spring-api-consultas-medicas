package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.*;
import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.services.RelatorioProdutividadeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/relatorios/produtividade")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios do Produtividade", description = "Relatórios relacionados à produtividade clínica")
public class RelatorioProdutividadeController {

    private final RelatorioProdutividadeService service;
    private final RelatorioExportService exportService;


    @GetMapping("/consultas-por-mes")
    @Operation(
            summary = "Consultas por mês, filtradas por status",
            description = "Retorna a quantidade de consultas por mês de acordo com o status informado (REALIZADA, AGENDADA, CANCELADA). Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Consultas por mês filtradas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarConsultasPorMesComFiltro(
            @RequestParam(defaultValue = "REALIZADA") StatusConsulta status,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<TotalConsultasRealizadasNoMesDto> lista = service.totalConsultasPorMes(status);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, TotalConsultasRealizadasNoMesDto.class, formato,
                ExportContext.of("Consultas por Mês (" + status.name() + ")", Map.of("Status", status.name())));
    }

    @GetMapping("/media-consultas")
    @Operation(
            summary = "Média de consultas por dia, semana e mês",
            description = "Retorna a média de consultas realizadas por dia, por semana e por mês. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Média de consultas calculada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarMediaConsultasPorPeriodo(
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        MediaConsultasDto dto = service.mediaConsultas();
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Média de Consultas"));
    }

    @GetMapping("/tempo-medio-duracao")
    @Operation(
            summary = "Tempo médio de duração das consultas",
            description = "Retorna o tempo médio de duração das consultas realizadas, em minutos. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Tempo médio de duração calculado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarTempoMedioDuracaoConsultas(
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        TempoMedioDuracaoDto dto = service.tempoMedioDuracao();
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Tempo Médio de Duração"));
    }

    @GetMapping("/tempo-medio-espera")
    @Operation(
            summary = "Tempo médio de espera entre agendamento e atendimento",
            description = "Retorna o tempo médio, em dias, entre o agendamento e o atendimento das consultas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Tempo médio de espera calculado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarTempoMedioEsperaEntreAgendamentoEAtendimento(
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        TempoMedioEsperaDto dto = service.tempoMedioEspera();
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Tempo Médio de Espera"));
    }

    @GetMapping("/taxa-comparecimento")
    @Operation(
            summary = "Taxa de comparecimento dos pacientes",
            description = "Retorna a taxa de comparecimento, calculada como consultas realizadas ÷ consultas agendadas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Taxa de comparecimento calculada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarTaxaComparecimento(
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        TaxaComparecimentoDto dto = service.taxaComparecimento();
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Taxa de Comparecimento"));
    }

}
