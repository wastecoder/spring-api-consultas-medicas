package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.ConsultasPorStatusDto;

public interface RelatorioService {

    ConsultasPorStatusDto gerarRelatorioConsultasPorStatus();

}
