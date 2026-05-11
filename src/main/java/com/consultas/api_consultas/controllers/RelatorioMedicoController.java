package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.*;
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

@RestController
@RequestMapping("/relatorios/medico")
@RequiredArgsConstructor
@Validated
@Tag(name = "Relatórios de Médico", description = "Relatórios relacionados aos médicos")
public class RelatorioMedicoController {

    private final RelatorioMedicoService service;


    @GetMapping("/consultas-realizadas")
    @Operation(
            summary = "Consultas realizadas por médico",
            description = "Retorna a quantidade de consultas realizadas, agrupadas por médico"
    )
    @ApiResponse(responseCode = "200", description = "Consultas realizadas por médico listadas com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<ConsultasRealizadasPorMedicoDto>> listarConsultasRealizadasPorMedico(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.consultasRealizadasPorMedico(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/mais-consultas-no-mes")
    @Operation(
            summary = "Médicos com mais consultas no mês",
            description = "Retorna a lista de médicos com maior número de consultas no mês e ano especificados"
    )
    @ApiResponse(responseCode = "200", description = "Médicos com mais consultas no mês listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<MedicosComMaisConsultasNoMesDto>> listarMedicosComMaisConsultasNoMes(
            @RequestParam @Min(1) @Max(12) int mes,
            @RequestParam @Min(2000) int ano,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.medicosComMaisConsultasNoMes(mes, ano), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/por-especialidade")
    @Operation(
            summary = "Médicos por especialidade",
            description = "Retorna a quantidade de médicos agrupados por especialidade médica"
    )
    @ApiResponse(responseCode = "200", description = "Médicos por especialidade listados com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<MedicosPorEspecialidadeDto>> listarMedicosPorEspecialidade(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.medicosPorEspecialidade(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/taxa-cancelamento")
    @Operation(
            summary = "Taxa de cancelamentos por médico",
            description = "Retorna a taxa de cancelamentos de consultas para cada médico"
    )
    @ApiResponse(responseCode = "200", description = "Taxa de cancelamentos por médico listada com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<TaxaCancelamentoPorMedicoDto>> listarTaxaCancelamentoPorMedico(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.taxaCancelamentoPorMedico(), PageRequest.of(pagina, tamanho)));
    }

    @GetMapping("/faturamento")
    @Operation(
            summary = "Faturamento por médico",
            description = "Retorna o valor total faturado por cada médico com base nas consultas realizadas"
    )
    @ApiResponse(responseCode = "200", description = "Faturamento por médico listado com sucesso")
    @ApiResponse(responseCode = "400", description = "Parâmetros inválidos",
                 content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PageResponse<FaturamentoPorMedicoDto>> listarFaturamentoPorMedico(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho
    ) {
        return ResponseEntity.ok(PageResponse.fromList(service.faturamentoPorMedico(), PageRequest.of(pagina, tamanho)));
    }

}
