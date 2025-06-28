package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.*;

import java.util.List;

public interface RelatorioMedicoService {

    List<ConsultasRealizadasPorMedicoDto> consultasRealizadasPorMedico();

    List<MedicosComMaisConsultasNoMesDto> medicosComMaisConsultasNoMes(int mes, int ano);

    List<MedicosPorEspecialidadeDto> medicosPorEspecialidade();

    List<TaxaCancelamentoPorMedicoDto> taxaCancelamentoPorMedico();

    List<FaturamentoPorMedicoDto> faturamentoPorMedico();

}
