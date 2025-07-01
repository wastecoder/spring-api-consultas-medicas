package com.consultas.api_consultas.dtos.respostas.relatorios.operacional;

import java.time.LocalDate;

public record ConsultasPendentesDto(
        Long idConsulta,
        LocalDate dataConsulta,
        String nomeMedico,
        String nomePaciente
) {}
