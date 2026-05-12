package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.*;
import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.services.RelatorioMedicoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.Max;
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
@RequestMapping("/relatorios/medico")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Médico", description = "Relatórios relacionados aos médicos")
public class RelatorioMedicoController {

    private final RelatorioMedicoService service;
    private final RelatorioExportService exportService;


    @GetMapping("/consultas-realizadas")
    @Operation(
            summary = "Consultas realizadas por médico",
            description = "Retorna a quantidade de consultas realizadas, agrupadas por médico. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Consultas realizadas por médico listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarConsultasRealizadasPorMedico(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasRealizadasPorMedicoDto> lista = service.consultasRealizadasPorMedico();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultasRealizadasPorMedicoDto.class, formato,
                ExportContext.of("Consultas Realizadas por Médico"));
    }

    @GetMapping("/mais-consultas-no-mes")
    @Operation(
            summary = "Médicos com mais consultas no mês",
            description = "Retorna a lista de médicos com maior número de consultas no mês e ano especificados. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Médicos com mais consultas no mês listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarMedicosComMaisConsultasNoMes(
            @RequestParam @Min(1) @Max(12) int mes,
            @RequestParam @Min(2000) int ano,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<MedicosComMaisConsultasNoMesDto> lista = service.medicosComMaisConsultasNoMes(mes, ano);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, MedicosComMaisConsultasNoMesDto.class, formato,
                ExportContext.of("Médicos com Mais Consultas no Mês", Map.of(
                        "Mês/Ano", String.format("%02d/%04d", mes, ano))));
    }

    @GetMapping("/por-especialidade")
    @Operation(
            summary = "Médicos por especialidade",
            description = "Retorna a quantidade de médicos agrupados por especialidade médica. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Médicos por especialidade listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarMedicosPorEspecialidade(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<MedicosPorEspecialidadeDto> lista = service.medicosPorEspecialidade();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, MedicosPorEspecialidadeDto.class, formato,
                ExportContext.of("Médicos por Especialidade"));
    }

    @GetMapping("/taxa-cancelamento")
    @Operation(
            summary = "Taxa de cancelamentos por médico",
            description = "Retorna a taxa de cancelamentos de consultas para cada médico. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Taxa de cancelamentos por médico listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarTaxaCancelamentoPorMedico(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<TaxaCancelamentoPorMedicoDto> lista = service.taxaCancelamentoPorMedico();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, TaxaCancelamentoPorMedicoDto.class, formato,
                ExportContext.of("Taxa de Cancelamento por Médico"));
    }

    @GetMapping("/faturamento")
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
                ExportContext.of("Faturamento por Médico (Relatório Médico)"));
    }

}
