package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.*;

import java.time.LocalDate;
import java.util.List;

public interface RelatorioConsultaService {

    ConsultasPorStatusDto gerarRelatorioConsultasPorStatus();

    List<ConsultasPorMesDto> consultasPorMes();

    List<ConsultasPorAnoDto> consultasPorAno();

    List<ConsultasPorEspecialidadeDto> consultasPorEspecialidade();

    List<ConsultaResumoDto> consultasPorPaciente(Long id);

    List<ConsultaResumoDto> consultasPorMedico(Long id);

    List<ConsultaResumoDto> consultasPorPeriodo(LocalDate inicio, LocalDate fim);

}
