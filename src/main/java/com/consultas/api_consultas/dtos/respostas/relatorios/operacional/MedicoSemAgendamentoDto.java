package com.consultas.api_consultas.dtos.respostas.relatorios.operacional;

import com.consultas.api_consultas.enums.Especialidade;

public record MedicoSemAgendamentoDto(
        Long idMedico,
        String nomeMedico,
        Especialidade especialidadeMedica
) {}
