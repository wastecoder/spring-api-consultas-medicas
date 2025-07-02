package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.financeiro.*;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.RelatorioFinanceiroService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioFinanceiroServiceImpl implements RelatorioFinanceiroService {

    private final ConsultaRepository consultaRepository;


    @Override
    public List<FaturamentoMensalDto> faturamentoMensal() {
        log.info("Gerando relatório de faturamento mensal");

        return consultaRepository.faturamentoMensal()
                .stream()
                .map(registro -> {
                    int ano = (int) registro[0];
                    int mes = (int) registro[1];
                    BigDecimal total = (BigDecimal) registro[2];
                    return new FaturamentoMensalDto(ano, mes, total);
                })
                .toList();
    }

    @Override
    public List<FaturamentoPorMedicoDto> faturamentoPorMedico() {
        log.info("Gerando relatório de faturamento por médico");

        return consultaRepository.faturamentoPorMedico()
                .stream()
                .map(registro -> {
                    Long id = (Long) registro[0];
                    String nome = (String) registro[1];
                    BigDecimal total = (BigDecimal) registro[2];
                    return new FaturamentoPorMedicoDto(id, nome, total);
                })
                .toList();
    }

    @Override
    public List<FaturamentoPorEspecialidadeDto> faturamentoPorEspecialidade() {
        log.info("Gerando relatório de faturamento por especialidade");

        return consultaRepository.faturamentoPorEspecialidade()
                .stream()
                .map(registro -> {
                    Especialidade especialidade = (Especialidade) registro[0];
                    BigDecimal total = (BigDecimal) registro[1];
                    return new FaturamentoPorEspecialidadeDto(especialidade, total);
                })
                .toList();
    }

    @Override
    public FaturamentoPorPeriodoDto faturamentoPorPeriodo(LocalDate inicio, LocalDate fim) {
        validarDatasPeriodo(inicio, fim);

        log.info("Gerando relatório de faturamento por período: {} até {}", inicio, fim);

        BigDecimal total = consultaRepository.faturamentoPorPeriodo(inicio, fim);
        return new FaturamentoPorPeriodoDto(total != null ? total : BigDecimal.ZERO);
    }

    @Override
    public PerdasComCancelamentosDto perdasComCancelamentos() {
        log.info("Calculando perdas com cancelamentos");

        BigDecimal total = consultaRepository.perdasComCancelamentos();
        return new PerdasComCancelamentosDto(total != null ? total : BigDecimal.ZERO);
    }

    @Override
    public List<PerdaMensalCancelamentoDto> perdaMensalComCancelamentos() {
        log.info("Gerando relatório de perda mensal com cancelamentos");

        return consultaRepository.perdaMensalComCancelamentos()
                .stream()
                .map(registro -> {
                    int ano = (int) registro[0];
                    int mes = (int) registro[1];
                    BigDecimal totalPerdido = (BigDecimal) registro[2];
                    return new PerdaMensalCancelamentoDto(ano, mes, totalPerdido);
                })
                .toList();
    }

    @Override
    public PerdaPorPeriodoDto perdaPorPeriodo(LocalDate inicio, LocalDate fim) {
        validarDatasPeriodo(inicio, fim);
        log.info("Gerando relatório de perda por cancelamento no período: {} até {}", inicio, fim);

        BigDecimal total = consultaRepository.perdaPorPeriodo(inicio, fim);
        return new PerdaPorPeriodoDto(total != null ? total : BigDecimal.ZERO);
    }


    private void validarDatasPeriodo(LocalDate inicio, LocalDate fim) {
        if (inicio == null || fim == null) {
            throw new IllegalArgumentException("Data de início e data de fim devem ser informadas.");
        }

        if (inicio.isAfter(fim)) {
            throw new IllegalArgumentException("Data de início não pode ser posterior à data de fim.");
        }

        LocalDate limiteInferior = LocalDate.of(2000, 1, 1);
        LocalDate limiteSuperior = LocalDate.now().plusYears(5);

        if (inicio.isBefore(limiteInferior) || inicio.isAfter(limiteSuperior)) {
            throw new IllegalArgumentException("Data de início inválida: " + inicio +
                    ". Deve estar entre " + limiteInferior + " e " + limiteSuperior + ".");
        }

        if (fim.isBefore(limiteInferior) || fim.isAfter(limiteSuperior)) {
            throw new IllegalArgumentException("Data de fim inválida: " + fim +
                    ". Deve estar entre " + limiteInferior + " e " + limiteSuperior + ".");
        }
    }

}
