package com.consultas.api_consultas.dtos.respostas.relatorios.financeiro;

import java.math.BigDecimal;

public record PerdaMensalCancelamentoDto(
        int ano,
        int mes,
        BigDecimal totalPerdido
) {}
