package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.*;

import java.time.LocalDate;
import java.util.List;

public interface RelatorioFinanceiroService {

    List<FaturamentoMensalDto> faturamentoMensal();

    List<FaturamentoPorMedicoDto> faturamentoPorMedico();

    List<FaturamentoPorEspecialidadeDto> faturamentoPorEspecialidade();

    FaturamentoPorPeriodoDto faturamentoPorPeriodo(LocalDate inicio, LocalDate fim);

    PerdasComCancelamentosDto perdasComCancelamentos();

    List<PerdaMensalCancelamentoDto> perdaMensalComCancelamentos();

    PerdaPorPeriodoDto perdaPorPeriodo(LocalDate inicio, LocalDate fim);

}
