package com.consultas.api_consultas.dtos.respostas.relatorios.financeiro;

import com.consultas.api_consultas.enums.Especialidade;

import java.math.BigDecimal;

public record FaturamentoPorEspecialidadeDto(
        Especialidade especialidadeMedica,
        BigDecimal totalFaturado
) {}
