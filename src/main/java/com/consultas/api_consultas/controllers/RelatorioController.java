package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.ConsultasPorStatusDto;
import com.consultas.api_consultas.services.RelatorioService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/relatorios")
@RequiredArgsConstructor
@Tag(name = "Relatórios", description = "Relatórios de consultas, produtividade e faturamento")
public class RelatorioController {

    private final RelatorioService relatorioService;


    @GetMapping("/consultas-por-status")
    @Operation(summary = "Relatório de consultas por status", description = "Retorna a quantidade de consultas agrupadas por status: AGENDADA, CANCELADA e REALIZADA")
    @ApiResponse(responseCode = "200", description = "Relatório gerado com sucesso")
    public ResponseEntity<ConsultasPorStatusDto> getConsultasPorStatus() {
        ConsultasPorStatusDto dto = relatorioService.gerarRelatorioConsultasPorStatus();
        return ResponseEntity.ok(dto);
    }

}
