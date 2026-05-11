package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.*;
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

@RestController
@RequestMapping("/relatorios/consulta")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Consulta", description = "Relatórios específicos sobre consultas médicas")
public class RelatorioConsultaController {

    private final RelatorioConsultaService service;


    @GetMapping("/consultas-por-status")
    @Operation(summary = "Relatório de consultas por status", description = "Retorna a quantidade de consultas agrupadas por status: AGENDADA, CANCELADA e REALIZADA")
    @ApiResponse(responseCode = "200", description = "Relatório de consultas por status gerado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultasPorStatusDto> getConsultasPorStatus() {
        return ResponseEntity.ok(service.gerarRelatorioConsultasPorStatus());
    }

    @GetMapping("/por-mes")
    @Operation(summary = "Consultas por mês", description = "Retorna a quantidade de consultas agrupadas por mês (de todos os anos)")
    @ApiResponse(responseCode = "200", description = "Consultas por mês listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasPorMesDto>> porMes(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorMes(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/por-ano")
    @Operation(summary = "Consultas por ano", description = "Retorna a quantidade de consultas agrupadas por ano")
    @ApiResponse(responseCode = "200", description = "Consultas por ano listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasPorAnoDto>> porAno(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorAno(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/por-especialidade")
    @Operation(summary = "Consultas por especialidade", description = "Retorna a quantidade de consultas agrupadas por especialidade médica")
    @ApiResponse(responseCode = "200", description = "Consultas por especialidade listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasPorEspecialidadeDto>> porEspecialidade(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorEspecialidade(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/por-paciente/{id}")
    @Operation(summary = "Consultas por paciente", description = "Retorna todas as consultas de um paciente específico")
    @ApiResponse(responseCode = "200", description = "Consultas do paciente listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultaResumoDto>> porPaciente(
            @PathVariable @Min(1) Long id,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorPaciente(id), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/por-medico/{id}")
    @Operation(summary = "Consultas por médico", description = "Retorna todas as consultas de um médico específico")
    @ApiResponse(responseCode = "200", description = "Consultas do médico listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Médico não encontrado",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultaResumoDto>> porMedico(
            @PathVariable @Min(1) Long id,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorMedico(id), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/por-periodo")
    @Operation(summary = "Consultas por período", description = "Retorna todas as consultas dentro de um intervalo de datas")
    @ApiResponse(responseCode = "200", description = "Consultas do período listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultaResumoDto>> porPeriodo(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate inicio,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fim,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasPorPeriodo(inicio, fim), PageRequest.of(pagina, tamanho)));
    }

}
