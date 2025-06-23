package com.consultas.api_consultas.dtos.respostas;

public record ConsultasPorStatusDto(
        long agendada,
        long cancelada,
        long realizada
) {}
