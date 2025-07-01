package com.consultas.api_consultas.dtos.respostas.relatorios.operacional;

import com.consultas.api_consultas.enums.StatusConsulta;

import java.time.LocalDate;

public record ConsultasProximosDiasDto(
        Long idConsulta,
        LocalDate dataConsulta,
        String nomeMedico,
        String nomePaciente,
        StatusConsulta statusConsulta
) {}
