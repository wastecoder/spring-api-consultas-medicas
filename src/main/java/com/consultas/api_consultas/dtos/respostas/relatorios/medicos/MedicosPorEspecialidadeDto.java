package com.consultas.api_consultas.dtos.respostas.relatorios.medicos;

import com.consultas.api_consultas.enums.Especialidade;

public record MedicosPorEspecialidadeDto(
        Especialidade especialidade,
        long total
) {}
