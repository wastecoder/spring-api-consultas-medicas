package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.*;
import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.FormatadorValor;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.services.RelatorioFinanceiroService;
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

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/relatorios/financeiro")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios - Financeiro", description = "Relatórios financeiros da clínica")
public class RelatorioFinanceiroController {

    private final RelatorioFinanceiroService service;
    private final RelatorioExportService exportService;


    @GetMapping("/faturamento-mensal")
    @Operation(
            summary = "Faturamento total por mês",
            description = "Retorna o valor total faturado em cada mês com base nas consultas realizadas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Faturamento mensal listado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarFaturamentoMensal(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<FaturamentoMensalDto> lista = service.faturamentoMensal();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, FaturamentoMensalDto.class, formato,
                ExportContext.of("Faturamento Mensal"));
    }

    @GetMapping("/faturamento-por-medico")
    @Operation(
            summary = "Faturamento por médico",
            description = "Retorna o valor total faturado por cada médico com base nas consultas realizadas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Faturamento por médico listado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarFaturamentoPorMedico(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<FaturamentoPorMedicoDto> lista = service.faturamentoPorMedico();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, FaturamentoPorMedicoDto.class, formato,
                ExportContext.of("Faturamento por Médico"));
    }

    @GetMapping("/faturamento-por-especialidade")
    @Operation(
            summary = "Faturamento por especialidade",
            description = "Retorna o valor total faturado por especialidade médica, considerando apenas consultas realizadas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Faturamento por especialidade listado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarFaturamentoPorEspecialidade(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<FaturamentoPorEspecialidadeDto> lista = service.faturamentoPorEspecialidade();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, FaturamentoPorEspecialidadeDto.class, formato,
                ExportContext.of("Faturamento por Especialidade"));
    }

    @GetMapping("/faturamento-por-periodo")
    @Operation(
            summary = "Faturamento entre duas datas",
            description = "Retorna o valor total faturado no intervalo de datas informado, considerando apenas consultas com status REALIZADA. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Faturamento do período calculado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> consultarFaturamentoPorPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        FaturamentoPorPeriodoDto dto = service.faturamentoPorPeriodo(inicio, fim);
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Faturamento por Período", Map.of(
                        "Período", FormatadorValor.formatar(inicio) + " a " + FormatadorValor.formatar(fim))));
    }

    @GetMapping("/perdas-com-cancelamentos")
    @Operation(
            summary = "Perdas financeiras com cancelamentos",
            description = "Retorna o valor total perdido devido a cancelamentos de consultas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Perdas com cancelamentos calculadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> consultarPerdasFinanceirasPorCancelamentos(
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        PerdasComCancelamentosDto dto = service.perdasComCancelamentos();
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Perdas com Cancelamentos"));
    }

    @GetMapping("/perda-mensal-com-cancelamentos")
    @Operation(
            summary = "Perda mensal com cancelamentos",
            description = "Retorna o total financeiro perdido por cancelamentos, agrupado por ano e mês. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Perda mensal listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> obterPerdaMensalComCancelamentos(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<PerdaMensalCancelamentoDto> lista = service.perdaMensalComCancelamentos();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, PerdaMensalCancelamentoDto.class, formato,
                ExportContext.of("Perda Mensal com Cancelamentos"));
    }

    @GetMapping("/perda-por-periodo")
    @Operation(
            summary = "Perda financeira por cancelamento entre duas datas",
            description = "Retorna o total perdido por cancelamentos de consultas no período informado. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Perda do período calculada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> getPerdaPorPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        PerdaPorPeriodoDto dto = service.perdaPorPeriodo(inicio, fim);
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Perda por Período", Map.of(
                        "Período", FormatadorValor.formatar(inicio) + " a " + FormatadorValor.formatar(fim))));
    }

}
