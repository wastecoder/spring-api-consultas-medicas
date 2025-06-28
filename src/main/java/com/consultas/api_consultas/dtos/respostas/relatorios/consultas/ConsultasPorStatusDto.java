package com.consultas.api_consultas.dtos.respostas.relatorios.consultas;

public record ConsultasPorStatusDto(
        long agendada,
        long cancelada,
        long realizada
) {}
