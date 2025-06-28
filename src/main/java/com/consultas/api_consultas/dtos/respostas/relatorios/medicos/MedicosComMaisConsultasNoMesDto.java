package com.consultas.api_consultas.dtos.respostas.relatorios.medicos;

public record MedicosComMaisConsultasNoMesDto(
        Long id,
        String nome,
        long total
) {}
