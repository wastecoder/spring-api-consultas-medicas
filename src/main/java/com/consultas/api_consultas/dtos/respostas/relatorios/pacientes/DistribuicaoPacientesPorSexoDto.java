package com.consultas.api_consultas.dtos.respostas.relatorios.pacientes;

import com.consultas.api_consultas.enums.Sexo;

public record DistribuicaoPacientesPorSexoDto(
        Sexo sexo,
        long totalPacientes
) {}
