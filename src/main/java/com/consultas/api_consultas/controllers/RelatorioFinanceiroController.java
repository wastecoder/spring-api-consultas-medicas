package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.*;
import com.consultas.api_consultas.services.RelatorioFinanceiroService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/relatorios/financeiro")
@RequiredArgsConstructor
@Tag(name = "Relatórios - Financeiro", description = "Relatórios financeiros da clínica")
public class RelatorioFinanceiroController {

    private final RelatorioFinanceiroService service;


    @GetMapping("/faturamento-mensal")
    @Operation(
            summary = "Faturamento total por mês",
            description = "Retorna o valor total faturado em cada mês com base nas consultas realizadas"
    )
    public ResponseEntity<List<FaturamentoMensalDto>> listarFaturamentoMensal() {
        return ResponseEntity.ok(service.faturamentoMensal());
    }

    @GetMapping("/faturamento-por-medico")
    @Operation(
            summary = "Faturamento por médico",
            description = "Retorna o valor total faturado por cada médico com base nas consultas realizadas"
    )
    public ResponseEntity<List<FaturamentoPorMedicoDto>> listarFaturamentoPorMedico() {
        return ResponseEntity.ok(service.faturamentoPorMedico());
    }

    @GetMapping("/faturamento-por-especialidade")
    @Operation(
            summary = "Faturamento por especialidade",
            description = "Retorna o valor total faturado por especialidade médica, considerando apenas consultas realizadas"
    )
    public ResponseEntity<List<FaturamentoPorEspecialidadeDto>> listarFaturamentoPorEspecialidade() {
        return ResponseEntity.ok(service.faturamentoPorEspecialidade());
    }

    @GetMapping("/faturamento-por-periodo")
    @Operation(
            summary = "Faturamento entre duas datas",
            description = "Retorna o valor total faturado no intervalo de datas informado, considerando apenas consultas com status REALIZADA"
    )
    public ResponseEntity<FaturamentoPorPeriodoDto> consultarFaturamentoPorPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim
    ) {
        return ResponseEntity.ok(service.faturamentoPorPeriodo(inicio, fim));
    }

    @GetMapping("/perdas-com-cancelamentos")
    @Operation(
            summary = "Perdas financeiras com cancelamentos",
            description = "Retorna o valor total perdido devido a cancelamentos de consultas"
    )
    public ResponseEntity<PerdasComCancelamentosDto> consultarPerdasFinanceirasPorCancelamentos() {
        return ResponseEntity.ok(service.perdasComCancelamentos());
    }

    @GetMapping("/perda-mensal-com-cancelamentos")
    @Operation(
            summary = "Perda mensal com cancelamentos",
            description = "Retorna o total financeiro perdido por cancelamentos, agrupado por ano e mês"
    )
    public ResponseEntity<List<PerdaMensalCancelamentoDto>> obterPerdaMensalComCancelamentos() {
        return ResponseEntity.ok(service.perdaMensalComCancelamentos());
    }

    @GetMapping("/perda-por-periodo")
    @Operation(
            summary = "Perda financeira por cancelamento entre duas datas",
            description = "Retorna o total perdido por cancelamentos de consultas no período informado"
    )
    public ResponseEntity<PerdaPorPeriodoDto> getPerdaPorPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim
    ) {
        return ResponseEntity.ok(service.perdaPorPeriodo(inicio, fim));
    }

}
