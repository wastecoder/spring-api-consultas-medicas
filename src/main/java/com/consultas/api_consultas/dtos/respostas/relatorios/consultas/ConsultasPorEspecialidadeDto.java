package com.consultas.api_consultas.dtos.respostas.relatorios.consultas;

import com.consultas.api_consultas.enums.Especialidade;

public record ConsultasPorEspecialidadeDto(
        Especialidade especialidade,
        long total
) {}
