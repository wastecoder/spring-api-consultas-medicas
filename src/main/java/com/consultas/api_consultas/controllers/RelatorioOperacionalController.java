package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;
import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.FormatadorValor;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.services.RelatorioOperacionalService;
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

import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/relatorios/operacional")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Operacional", description = "Relatórios operacionais para acompanhamento de consultas e médicos")
public class RelatorioOperacionalController {

    private final RelatorioOperacionalService service;
    private final RelatorioExportService exportService;


    @GetMapping("/consultas-por-data")
    @Operation(
            summary = "Consultas marcadas em uma data específica (padrão: hoje)",
            description = "Retorna a lista de consultas cuja data de atendimento corresponde à data informada, independentemente do status. Se nenhuma data for informada, a data atual será utilizada como padrão. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Consultas da data listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarConsultasPorDataEspecifica(
            @RequestParam(required = false) LocalDate data,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasPorDataDto> lista = service.consultasPorData(data);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        Map<String, String> params = new HashMap<>();
        if (data != null) params.put("Data", FormatadorValor.formatar(data));
        return exportService.exportarLista(lista, ConsultasPorDataDto.class, formato,
                ExportContext.of("Consultas por Data", params));
    }

    @GetMapping("/consultas-proximos-7-dias")
    @Operation(
            summary = "Consultas marcadas para os próximos 7 dias",
            description = "Retorna as consultas que estão agendadas para os próximos sete dias a partir da data atual. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Consultas dos próximos 7 dias listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarConsultasProximosSeteDias(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasProximosDiasDto> lista = service.consultasProximosDias();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultasProximosDiasDto.class, formato,
                ExportContext.of("Consultas dos Próximos 7 Dias"));
    }

    @GetMapping("/consultas-pendentes")
    @Operation(
            summary = "Consultas que estão agendadas no passado (pendentes)",
            description = "Retorna as consultas agendadas em datas passadas que ainda não foram realizadas. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Consultas pendentes listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarConsultasPendentes(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<ConsultasPendentesDto> lista = service.consultasPendentes();
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        return exportService.exportarLista(lista, ConsultasPendentesDto.class, formato,
                ExportContext.of("Consultas Pendentes"));
    }

    @GetMapping("/medicos-sem-agendamento")
    @Operation(
            summary = "Médicos sem consultas agendadas no mês (padrão: mês atual)",
            description = "Retorna a lista de médicos que não possuem nenhuma consulta agendada no mês especificado. Caso nenhum mês/ano seja informado, considera o atual. Aceita ?formato=csv ou ?formato=pdf para download."
    )
    @ApiResponse(responseCode = "200", description = "Médicos sem agendamento listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<?> listarMedicosSemAgendamentosNoMes(
            @RequestParam(required = false) @Min(1) @Max(12) Integer mes,
            @RequestParam(required = false) @Min(2000) Integer ano,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(defaultValue = "JSON") FormatoExportacao formato
    ) {
        List<MedicoSemAgendamentoDto> lista = service.medicosSemAgendamento(ano, mes);
        if (formato.isJson()) {
            return ResponseEntity.ok(PageResponse.fromList(lista, PageRequest.of(pagina, tamanho)));
        }
        Map<String, String> params = new HashMap<>();
        if (mes != null && ano != null) params.put("Mês/Ano", String.format("%02d/%04d", mes, ano));
        return exportService.exportarLista(lista, MedicoSemAgendamentoDto.class, formato,
                ExportContext.of("Médicos sem Agendamento", params));
    }

}
