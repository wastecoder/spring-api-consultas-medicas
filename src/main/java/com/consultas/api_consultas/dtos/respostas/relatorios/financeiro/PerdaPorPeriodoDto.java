package com.consultas.api_consultas.dtos.respostas.relatorios.financeiro;

import java.math.BigDecimal;

public record PerdaPorPeriodoDto(
        BigDecimal totalPerdido
) {}

