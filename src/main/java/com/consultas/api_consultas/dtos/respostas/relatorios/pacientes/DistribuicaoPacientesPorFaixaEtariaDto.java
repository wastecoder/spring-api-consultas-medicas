package com.consultas.api_consultas.dtos.respostas.relatorios.pacientes;

public record DistribuicaoPacientesPorFaixaEtariaDto(
        String faixaEtaria,
        long totalPacientes
) {}
