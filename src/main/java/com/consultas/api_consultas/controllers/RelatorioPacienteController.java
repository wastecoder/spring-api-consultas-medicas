package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.*;
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

@RestController
@RequestMapping("/relatorios/paciente")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Paciente", description = "Relatórios relacionados aos pacientes")
public class RelatorioPacienteController {

    private final RelatorioPacienteService service;


    @GetMapping("/historico/{id}")
    @Operation(
            summary = "Histórico completo de consultas por paciente",
            description = "Retorna o histórico completo de consultas de um paciente específico, incluindo realizadas, agendadas e canceladas"
    )
    @ApiResponse(responseCode = "200", description = "Histórico do paciente listado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<HistoricoConsultaPacienteDto>> listarHistoricoAtendimentosPorPaciente(
            @PathVariable @Min(1) Long id,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.historicoPorPaciente(id), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/cancelamentos")
    @Operation(
            summary = "Cancelamentos feitos por paciente",
            description = "Retorna a quantidade de consultas canceladas, agrupadas por paciente"
    )
    @ApiResponse(responseCode = "200", description = "Cancelamentos por paciente listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<CancelamentosPorPacienteDto>> listarCancelamentosPorPaciente(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.cancelamentosPorPaciente(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/mais-consultas")
    @Operation(
            summary = "Pacientes com mais consultas no período",
            description = "Retorna a lista dos pacientes que mais realizaram consultas dentro do intervalo de datas informado"
    )
    @ApiResponse(responseCode = "200", description = "Pacientes com mais consultas listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<PacientesComMaisConsultasDto>> listarPacientesComMaisConsultasNoPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.pacientesComMaisConsultasPorPeriodo(inicio, fim), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/distribuicao-sexo")
    @Operation(
            summary = "Distribuição de pacientes por sexo",
            description = "Retorna a quantidade de pacientes agrupados por sexo"
    )
    @ApiResponse(responseCode = "200", description = "Distribuição por sexo listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<DistribuicaoPacientesPorSexoDto>> listarDistribuicaoPacientesPorSexo(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.distribuicaoPorSexo(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/distribuicao-faixa-etaria")
    @Operation(
            summary = "Distribuição de pacientes por faixa etária",
            description = "Retorna a quantidade de pacientes agrupados por faixa etária"
    )
    @ApiResponse(responseCode = "200", description = "Distribuição por faixa etária listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<DistribuicaoPacientesPorFaixaEtariaDto>> listarDistribuicaoPacientesPorFaixaEtaria(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.distribuicaoPorFaixaEtaria(), PageRequest.of(pagina, tamanho)));
    }

}
