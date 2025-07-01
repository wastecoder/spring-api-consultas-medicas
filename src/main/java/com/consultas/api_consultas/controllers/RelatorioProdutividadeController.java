package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.*;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.services.RelatorioProdutividadeService;
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
@RequestMapping("/relatorios/paciente")
@RequiredArgsConstructor
@Tag(name = "Relatórios do Produtividade", description = "Relatórios relacionados à produtividade clínica")
public class RelatorioProdutividadeController {

    private final RelatorioProdutividadeService service;


    @GetMapping("/consultas-por-mes")
    @Operation(
            summary = "Consultas por mês, filtradas por status",
            description = "Retorna a quantidade de consultas por mês de acordo com o status informado (REALIZADA, AGENDADA, CANCELADA)"
    )
    public ResponseEntity<List<TotalConsultasRealizadasNoMesDto>> listarConsultasPorMesComFiltro(
            @RequestParam(defaultValue = "REALIZADA") StatusConsulta status
    ) {
        return ResponseEntity.ok(service.totalConsultasPorMes(status));
    }

    @GetMapping("/media-consultas")
    @Operation(
            summary = "Média de consultas por dia, semana e mês",
            description = "Retorna a média de consultas realizadas por dia, por semana e por mês"
    )
    public ResponseEntity<MediaConsultasDto> listarMediaConsultasPorPeriodo() {
        return ResponseEntity.ok(service.mediaConsultas());
    }

    @GetMapping("/tempo-medio-duracao")
    @Operation(
            summary = "Tempo médio de duração das consultas",
            description = "Retorna o tempo médio de duração das consultas realizadas, em minutos"
    )
    public ResponseEntity<TempoMedioDuracaoDto> listarTempoMedioDuracaoConsultas() {
        return ResponseEntity.ok(service.tempoMedioDuracao());
    }

    @GetMapping("/tempo-medio-espera")
    @Operation(
            summary = "Tempo médio de espera entre agendamento e atendimento",
            description = "Retorna o tempo médio, em dias, entre o agendamento e o atendimento das consultas"
    )
    public ResponseEntity<TempoMedioEsperaDto> listarTempoMedioEsperaEntreAgendamentoEAtendimento() {
        return ResponseEntity.ok(service.tempoMedioEspera());
    }

    @GetMapping("/taxa-comparecimento")
    @Operation(
            summary = "Taxa de comparecimento dos pacientes",
            description = "Retorna a taxa de comparecimento, calculada como consultas realizadas ÷ consultas agendadas"
    )
    public ResponseEntity<TaxaComparecimentoDto> listarTaxaComparecimento() {
        return ResponseEntity.ok(service.taxaComparecimento());
    }

}
