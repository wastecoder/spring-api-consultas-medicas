package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.*;
import com.consultas.api_consultas.services.RelatorioConsultaService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/relatorios/consulta")
@RequiredArgsConstructor
@Tag(name = "Relatórios de Consulta", description = "Relatórios específicos sobre consultas médicas")
public class RelatorioConsultaController {

    private final RelatorioConsultaService service;


    @GetMapping("/consultas-por-status")
    @Operation(summary = "Relatório de consultas por status", description = "Retorna a quantidade de consultas agrupadas por status: AGENDADA, CANCELADA e REALIZADA")
    @ApiResponse(responseCode = "200", description = "Relatório gerado com sucesso")
    public ResponseEntity<ConsultasPorStatusDto> getConsultasPorStatus() {
        return ResponseEntity.ok(service.gerarRelatorioConsultasPorStatus());
    }

    @GetMapping("/por-mes")
    @Operation(summary = "Consultas por mês", description = "Retorna a quantidade de consultas agrupadas por mês (de todos os anos)")
    public ResponseEntity<List<ConsultasPorMesDto>> porMes() {
        return ResponseEntity.ok(service.consultasPorMes());
    }

    @GetMapping("/por-ano")
    @Operation(summary = "Consultas por ano", description = "Retorna a quantidade de consultas agrupadas por ano")
    public ResponseEntity<List<ConsultasPorAnoDto>> porAno() {
        return ResponseEntity.ok(service.consultasPorAno());
    }

    @GetMapping("/por-especialidade")
    @Operation(summary = "Consultas por especialidade", description = "Retorna a quantidade de consultas agrupadas por especialidade médica")
    public ResponseEntity<List<ConsultasPorEspecialidadeDto>> porEspecialidade() {
        return ResponseEntity.ok(service.consultasPorEspecialidade());
    }

    @GetMapping("/por-paciente/{id}")
    @Operation(summary = "Consultas por paciente", description = "Retorna todas as consultas de um paciente específico")
    public ResponseEntity<List<ConsultaResumoDto>> porPaciente(@PathVariable Long id) {
        return ResponseEntity.ok(service.consultasPorPaciente(id));
    }

    @GetMapping("/por-medico/{id}")
    @Operation(summary = "Consultas por médico", description = "Retorna todas as consultas de um médico específico")
    public ResponseEntity<List<ConsultaResumoDto>> porMedico(@PathVariable Long id) {
        return ResponseEntity.ok(service.consultasPorMedico(id));
    }

    @GetMapping("/por-periodo")
    @Operation(summary = "Consultas por período", description = "Retorna todas as consultas dentro de um intervalo de datas")
    public ResponseEntity<List<ConsultaResumoDto>> porPeriodo(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate inicio,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fim) {
        return ResponseEntity.ok(service.consultasPorPeriodo(inicio, fim));
    }

}
