package com.consultas.api_consultas.dtos.respostas.relatorios.financeiro;

import java.math.BigDecimal;

public record FaturamentoPorMedicoDto(
        Long idMedico,
        String nomeMedico,
        BigDecimal totalFaturado
) {}
