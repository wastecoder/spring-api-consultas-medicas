package com.consultas.api_consultas.dtos.respostas.relatorios.pacientes;

public record CancelamentosPorPacienteDto(
        Long idPaciente,
        String nomePaciente,
        long totalCancelamentos
) {}
