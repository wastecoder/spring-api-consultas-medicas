package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;
import com.consultas.api_consultas.services.RelatorioOperacionalService;
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
@RequestMapping("/relatorios/operacional")
@RequiredArgsConstructor
@Tag(name = "Relatórios de Operacional", description = "Relatórios operacionais para acompanhamento de consultas e médicos")
public class RelatorioOperacionalController {

    private final RelatorioOperacionalService service;


    @GetMapping("/consultas-por-data")
    @Operation(
            summary = "Consultas marcadas em uma data específica (padrão: hoje)",
            description = "Retorna a lista de consultas cuja data de atendimento corresponde à data informada, independentemente do status. Se nenhuma data for informada, a data atual será utilizada como padrão."
    )
    public ResponseEntity<List<ConsultasPorDataDto>> listarConsultasPorDataEspecifica(
            @RequestParam(required = false) LocalDate data
    ) {
        return ResponseEntity.ok(service.consultasPorData(data));
    }

    @GetMapping("/consultas-proximos-7-dias")
    @Operation(
            summary = "Consultas marcadas para os próximos 7 dias",
            description = "Retorna as consultas que estão agendadas para os próximos sete dias a partir da data atual."
    )
    public ResponseEntity<List<ConsultasProximosDiasDto>> listarConsultasProximosSeteDias() {
        return ResponseEntity.ok(service.consultasProximosDias());
    }

    @GetMapping("/consultas-pendentes")
    @Operation(
            summary = "Consultas que estão agendadas no passado (pendentes)",
            description = "Retorna as consultas agendadas em datas passadas que ainda não foram realizadas."
    )
    public ResponseEntity<List<ConsultasPendentesDto>> listarConsultasPendentes() {
        return ResponseEntity.ok(service.consultasPendentes());
    }

    @GetMapping("/medicos-sem-agendamento")
    @Operation(
            summary = "Médicos sem consultas agendadas no mês (padrão: mês atual)",
            description = "Retorna a lista de médicos que não possuem nenhuma consulta agendada no mês especificado. Caso nenhum mês/ano seja informado, considera o atual."
    )
    public ResponseEntity<List<MedicoSemAgendamentoDto>> listarMedicosSemAgendamentosNoMes(
            @RequestParam(required = false) Integer mes,
            @RequestParam(required = false) Integer ano
    ) {
        return ResponseEntity.ok(service.medicosSemAgendamento(ano, mes));
    }

}
