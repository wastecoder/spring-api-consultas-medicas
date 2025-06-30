package com.consultas.api_consultas.dtos.respostas.relatorios.pacientes;

public record PacientesComMaisConsultasDto(
        Long idPaciente,
        String nomePaciente,
        long totalConsultas
) {}
