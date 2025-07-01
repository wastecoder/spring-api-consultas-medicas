package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;

import java.time.LocalDate;
import java.util.List;

public interface RelatorioOperacionalService {

    List<ConsultasPorDataDto> consultasPorData(LocalDate data);

    List<ConsultasProximosDiasDto> consultasProximosDias();

    List<ConsultasPendentesDto> consultasPendentes();

    List<MedicoSemAgendamentoDto> medicosSemAgendamento(Integer ano, Integer mes);

}
