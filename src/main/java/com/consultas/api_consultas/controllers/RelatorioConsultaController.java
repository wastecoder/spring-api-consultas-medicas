package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.*;
import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.FormatadorValor;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.services.RelatorioConsultaService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/relatorios/consulta")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Consulta", description = "Relatórios específicos sobre consultas médicas")
public class RelatorioConsultaController {

    private final RelatorioConsultaService service;
    private final RelatorioExportService exportService;


    @GetMapping("/consultas-por-status")
    @Operation(summary = "Relatório de consultas por status",
            description = "Retorna a quantidade de consultas agrupadas por status: AGENDADA, CANCELADA e REALIZADA. Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Relatório de consultas por status gerado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> getConsultasPorStatus(
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        ConsultasPorStatusDto dto = service.gerarRelatorioConsultasPorStatus();
        if (formato.isJson()) return ResponseEntity.ok(dto);
        return exportService.exportarObjeto(dto, formato,
                ExportContext.of("Consultas por Status"));
    }

    @GetMapping("/por-mes")
    @Operation(summary = "Consultas por mês",
            description = "Retorna a quantidade de consultas agrupadas por mês (de todos os anos). Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Consultas por mês listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> porMes(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasPorMesDto> lista = service.consultasPorMes();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultasPorMesDto.class, formato,
                ExportContext.of("Consultas por Mês"));
    }

    @GetMapping("/por-ano")
    @Operation(summary = "Consultas por ano",
            description = "Retorna a quantidade de consultas agrupadas por ano. Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Consultas por ano listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> porAno(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasPorAnoDto> lista = service.consultasPorAno();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultasPorAnoDto.class, formato,
                ExportContext.of("Consultas por Ano"));
    }

    @GetMapping("/por-especialidade")
    @Operation(summary = "Consultas por especialidade",
            description = "Retorna a quantidade de consultas agrupadas por especialidade médica. Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Consultas por especialidade listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> porEspecialidade(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasPorEspecialidadeDto> lista = service.consultasPorEspecialidade();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultasPorEspecialidadeDto.class, formato,
                ExportContext.of("Consultas por Especialidade"));
    }

    @GetMapping("/por-paciente/{id}")
    @Operation(summary = "Consultas por paciente",
            description = "Retorna todas as consultas de um paciente específico. Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Consultas do paciente listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> porPaciente(
            @PathVariable @Min(1) Long id,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultaResumoDto> lista = service.consultasPorPaciente(id);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultaResumoDto.class, formato,
                ExportContext.of("Consultas por Paciente", Map.of("Paciente (ID)", id.toString())));
    }

    @GetMapping("/por-medico/{id}")
    @Operation(summary = "Consultas por médico",
            description = "Retorna todas as consultas de um médico específico. Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Consultas do médico listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Médico não encontrado",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> porMedico(
            @PathVariable @Min(1) Long id,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultaResumoDto> lista = service.consultasPorMedico(id);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultaResumoDto.class, formato,
                ExportContext.of("Consultas por Médico", Map.of("Médico (ID)", id.toString())));
    }

    @GetMapping("/por-periodo")
    @Operation(summary = "Consultas por período",
            description = "Retorna todas as consultas dentro de um intervalo de datas. Aceita ?formato=csv ou ?formato=pdf para download.")
    @ApiResponse(responseCode = "200", description = "Consultas do período listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> porPeriodo(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate inicio,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fim,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultaResumoDto> lista = service.consultasPorPeriodo(inicio, fim);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultaResumoDto.class, formato,
                ExportContext.of("Consultas por Período", Map.of(
                        "Período", FormatadorValor.formatar(inicio) + " a " + FormatadorValor.formatar(fim))));
    }

}
