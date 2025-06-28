package com.consultas.api_consultas.dtos.respostas.relatorios.medicos;

public record ConsultasRealizadasPorMedicoDto(
        Long id,
        String nome,
        long total
) {}
