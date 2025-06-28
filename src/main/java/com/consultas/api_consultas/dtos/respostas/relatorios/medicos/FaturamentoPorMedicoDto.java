package com.consultas.api_consultas.dtos.respostas.relatorios.medicos;

import java.math.BigDecimal;

public record FaturamentoPorMedicoDto(
        Long id,
        String nome,
        BigDecimal total
) {}
