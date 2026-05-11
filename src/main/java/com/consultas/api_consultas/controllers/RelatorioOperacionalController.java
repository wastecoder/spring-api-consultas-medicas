package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;
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

@RestController
@RequestMapping("/relatorios/operacional")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Operacional", description = "Relatórios operacionais para acompanhamento de consultas e médicos")
public class RelatorioOperacionalController {

    private final RelatorioOperacionalService service;


    @GetMapping("/consultas-por-data")
    @Operation(
            summary = "Consultas marcadas em uma data específica (padrão: hoje)",
            description = "Retorna a lista de consultas cuja data de atendimento corresponde à data informada, independentemente do status. Se nenhuma data for informada, a data atual será utilizada como padrão."
    )
    @ApiResponse(responseCode = "200", description = "Consultas da data listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasPorDataDto>> listarConsultasPorDataEspecifica(
            @RequestParam(required = false) LocalDate data,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorData(data), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/consultas-proximos-7-dias")
    @Operation(
            summary = "Consultas marcadas para os próximos 7 dias",
            description = "Retorna as consultas que estão agendadas para os próximos sete dias a partir da data atual."
    )
    @ApiResponse(responseCode = "200", description = "Consultas dos próximos 7 dias listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasProximosDiasDto>> listarConsultasProximosSeteDias(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasProximosDias(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/consultas-pendentes")
    @Operation(
            summary = "Consultas que estão agendadas no passado (pendentes)",
            description = "Retorna as consultas agendadas em datas passadas que ainda não foram realizadas."
    )
    @ApiResponse(responseCode = "200", description = "Consultas pendentes listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasPendentesDto>> listarConsultasPendentes(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPendentes(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/medicos-sem-agendamento")
    @Operation(
            summary = "Médicos sem consultas agendadas no mês (padrão: mês atual)",
            description = "Retorna a lista de médicos que não possuem nenhuma consulta agendada no mês especificado. Caso nenhum mês/ano seja informado, considera o atual."
    )
    @ApiResponse(responseCode = "200", description = "Médicos sem agendamento listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<MedicoSemAgendamentoDto>> listarMedicosSemAgendamentosNoMes(
            @RequestParam(required = false) @Min(1) @Max(12) Integer mes,
            @RequestParam(required = false) @Min(2000) Integer ano,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.medicosSemAgendamento(ano, mes), PageRequest.of(pagina, tamanho)));
    }

}
