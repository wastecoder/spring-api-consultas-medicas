package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.*;
import com.consultas.api_consultas.services.RelatorioPacienteService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/relatorios/paciente")
@RequiredArgsConstructor
@Tag(name = "Relatórios de Paciente", description = "Relatórios relacionados aos pacientes")
public class RelatorioPacienteController {

    private final RelatorioPacienteService service;


    @GetMapping("/historico/{id}")
    @Operation(
            summary = "Histórico completo de consultas por paciente",
            description = "Retorna o histórico completo de consultas de um paciente específico, incluindo realizadas, agendadas e canceladas"
    )
    public ResponseEntity<List<HistoricoConsultaPacienteDto>> listarHistoricoAtendimentosPorPaciente(@PathVariable Long id) {
        return ResponseEntity.ok(service.historicoPorPaciente(id));
    }

    @GetMapping("/cancelamentos")
    @Operation(
            summary = "Cancelamentos feitos por paciente",
            description = "Retorna a quantidade de consultas canceladas, agrupadas por paciente"
    )
    public ResponseEntity<List<CancelamentosPorPacienteDto>> listarCancelamentosPorPaciente() {
        return ResponseEntity.ok(service.cancelamentosPorPaciente());
    }

    @GetMapping("/mais-consultas")
    @Operation(
            summary = "Pacientes com mais consultas no período",
            description = "Retorna a lista dos pacientes que mais realizaram consultas dentro do intervalo de datas informado"
    )
    public ResponseEntity<List<PacientesComMaisConsultasDto>> listarPacientesComMaisConsultasNoPeriodo(
            @RequestParam LocalDate inicio,
            @RequestParam LocalDate fim
    ) {
        return ResponseEntity.ok(service.pacientesComMaisConsultasPorPeriodo(inicio, fim));
    }

    @GetMapping("/distribuicao-sexo")
    @Operation(
            summary = "Distribuição de pacientes por sexo",
            description = "Retorna a quantidade de pacientes agrupados por sexo"
    )
    public ResponseEntity<List<DistribuicaoPacientesPorSexoDto>> listarDistribuicaoPacientesPorSexo() {
        return ResponseEntity.ok(service.distribuicaoPorSexo());
    }

    @GetMapping("/distribuicao-faixa-etaria")
    @Operation(
            summary = "Distribuição de pacientes por faixa etária",
            description = "Retorna a quantidade de pacientes agrupados por faixa etária"
    )
    public ResponseEntity<List<DistribuicaoPacientesPorFaixaEtariaDto>> listarDistribuicaoPacientesPorFaixaEtaria() {
        return ResponseEntity.ok(service.distribuicaoPorFaixaEtaria());
    }

}
