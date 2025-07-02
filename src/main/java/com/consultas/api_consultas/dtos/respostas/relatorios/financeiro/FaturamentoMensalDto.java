package com.consultas.api_consultas.dtos.respostas.relatorios.financeiro;

import java.math.BigDecimal;

public record FaturamentoMensalDto(
        int ano,
        int mes,
        BigDecimal totalFaturado
) {}
