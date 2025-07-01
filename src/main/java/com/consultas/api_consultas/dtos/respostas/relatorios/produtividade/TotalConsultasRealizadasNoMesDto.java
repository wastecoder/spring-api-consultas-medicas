package com.consultas.api_consultas.dtos.respostas.relatorios.produtividade;

public record TotalConsultasRealizadasNoMesDto(
        int ano,
        int mes,
        long totalConsultas
) {}
