package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.*;
import com.consultas.api_consultas.services.RelatorioMedicoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/relatorios/medico")
@RequiredArgsConstructor
@Tag(name = "Relatórios de Médico", description = "Relatórios relacionados aos médicos")
public class RelatorioMedicoController {

    private final RelatorioMedicoService service;


    @GetMapping("/consultas-realizadas")
    @Operation(
            summary = "Consultas realizadas por médico",
            description = "Retorna a quantidade de consultas realizadas, agrupadas por médico"
    )
    public ResponseEntity<List<ConsultasRealizadasPorMedicoDto>> listarConsultasRealizadasPorMedico() {
        return ResponseEntity.ok(service.consultasRealizadasPorMedico());
    }

    @GetMapping("/mais-consultas-no-mes")
    @Operation(
            summary = "Médicos com mais consultas no mês",
            description = "Retorna a lista de médicos com maior número de consultas no mês e ano especificados"
    )
    public ResponseEntity<List<MedicosComMaisConsultasNoMesDto>> listarMedicosComMaisConsultasNoMes(
            @RequestParam int mes,
            @RequestParam int ano) {
        return ResponseEntity.ok(service.medicosComMaisConsultasNoMes(mes, ano));
    }

    @GetMapping("/por-especialidade")
    @Operation(
            summary = "Médicos por especialidade",
            description = "Retorna a quantidade de médicos agrupados por especialidade médica"
    )
    public ResponseEntity<List<MedicosPorEspecialidadeDto>> listarMedicosPorEspecialidade() {
        return ResponseEntity.ok(service.medicosPorEspecialidade());
    }

    @GetMapping("/taxa-cancelamento")
    @Operation(
            summary = "Taxa de cancelamentos por médico",
            description = "Retorna a taxa de cancelamentos de consultas para cada médico"
    )
    public ResponseEntity<List<TaxaCancelamentoPorMedicoDto>> listarTaxaCancelamentoPorMedico() {
        return ResponseEntity.ok(service.taxaCancelamentoPorMedico());
    }

    @GetMapping("/faturamento")
    @Operation(
            summary = "Faturamento por médico",
            description = "Retorna o valor total faturado por cada médico com base nas consultas realizadas"
    )
    public ResponseEntity<List<FaturamentoPorMedicoDto>> listarFaturamentoPorMedico() {
        return ResponseEntity.ok(service.faturamentoPorMedico());
    }

}
