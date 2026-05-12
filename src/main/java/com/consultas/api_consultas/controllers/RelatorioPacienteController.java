package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.*;
import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.FormatadorValor;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.services.RelatorioPacienteService;
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
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/relatorios/paciente")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Paciente", description = "Relatórios relacionados aos pacientes")
public class RelatorioPacienteController {

    private final RelatorioPacienteService service;
    private final RelatorioExportService exportService;


    @GetMapping("/historico/{id}")
    @Operation(
            summary = "Histórico completo de consultas por paciente",
            description = "Retorna o histórico completo de consultas de um paciente específico, incluindo realizadas, agendadas e canceladas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Histórico do paciente listado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarHistoricoAtendimentosPorPaciente(
            @PathVariable @Min(1) Long id,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<HistoricoConsultaPacienteDto> lista = service.historicoPorPaciente(id);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, HistoricoConsultaPacienteDto.class, formato,
                ExportContext.of("Histórico do Paciente", Map.of("Paciente (ID)", id.toString())));
    }

    @GetMapping("/cancelamentos")
    @Operation(
            summary = "Cancelamentos feitos por paciente",
            description = "Retorna a quantidade de consultas canceladas, agrupadas por paciente. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Cancelamentos por paciente listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarCancelamentosPorPaciente(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<CancelamentosPorPacienteDto> lista = service.cancelamentosPorPaciente();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, CancelamentosPorPacienteDto.class, formato,
                ExportContext.of("Cancelamentos por Paciente"));
    }

    @GetMapping("/mais-consultas")
    @Operation(
            summary = "Pacientes com mais consultas no período",
            description = "Retorna a lista dos pacientes que mais realizaram consultas dentro do intervalo de datas informado. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Pacientes com mais consultas listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarPacientesComMaisConsultasNoPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<PacientesComMaisConsultasDto> lista = service.pacientesComMaisConsultasPorPeriodo(inicio, fim);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, PacientesComMaisConsultasDto.class, formato,
                ExportContext.of("Pacientes com Mais Consultas", Map.of(
                        "Período", FormatadorValor.formatar(inicio) + " a " + FormatadorValor.formatar(fim))));
    }

    @GetMapping("/distribuicao-sexo")
    @Operation(
            summary = "Distribuição de pacientes por sexo",
            description = "Retorna a quantidade de pacientes agrupados por sexo. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Distribuição por sexo listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarDistribuicaoPacientesPorSexo(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<DistribuicaoPacientesPorSexoDto> lista = service.distribuicaoPorSexo();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, DistribuicaoPacientesPorSexoDto.class, formato,
                ExportContext.of("Distribuição de Pacientes por Sexo"));
    }

    @GetMapping("/distribuicao-faixa-etaria")
    @Operation(
            summary = "Distribuição de pacientes por faixa etária",
            description = "Retorna a quantidade de pacientes agrupados por faixa etária. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Distribuição por faixa etária listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarDistribuicaoPacientesPorFaixaEtaria(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<DistribuicaoPacientesPorFaixaEtariaDto> lista = service.distribuicaoPorFaixaEtaria();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, DistribuicaoPacientesPorFaixaEtariaDto.class, formato,
                ExportContext.of("Distribuição de Pacientes por Faixa Etária"));
    }

}
