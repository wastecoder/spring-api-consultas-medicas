package com.consultas.api_consultas.dtos.respostas.relatorios.operacional;

import com.consultas.api_consultas.enums.StatusConsulta;

import java.time.LocalTime;

public record ConsultasPorDataDto(
        Long idConsulta,
        LocalTime horarioConsulta,
        String nomeMedico,
        String nomePaciente,
        StatusConsulta statusConsulta
) {}
