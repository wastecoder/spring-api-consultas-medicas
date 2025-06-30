package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.*;

import java.time.LocalDate;
import java.util.List;

public interface RelatorioPacienteService {

    List<HistoricoConsultaPacienteDto> historicoPorPaciente(Long id);

    List<CancelamentosPorPacienteDto> cancelamentosPorPaciente();

    List<PacientesComMaisConsultasDto> pacientesComMaisConsultasPorPeriodo(LocalDate inicio, LocalDate fim);

    List<DistribuicaoPacientesPorSexoDto> distribuicaoPorSexo();

    List<DistribuicaoPacientesPorFaixaEtariaDto> distribuicaoPorFaixaEtaria();

}
