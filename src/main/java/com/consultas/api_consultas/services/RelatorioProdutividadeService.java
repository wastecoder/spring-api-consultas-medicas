package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.*;
import com.consultas.api_consultas.enums.StatusConsulta;

import java.util.List;

public interface RelatorioProdutividadeService {

    List<TotalConsultasRealizadasNoMesDto> totalConsultasPorMes(StatusConsulta status);

    MediaConsultasDto mediaConsultas();

    TempoMedioDuracaoDto tempoMedioDuracao();

    TempoMedioEsperaDto tempoMedioEspera();

    TaxaComparecimentoDto taxaComparecimento();

}
