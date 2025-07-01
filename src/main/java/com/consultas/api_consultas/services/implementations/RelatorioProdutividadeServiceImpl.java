package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.*;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.RelatorioProdutividadeService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioProdutividadeServiceImpl implements RelatorioProdutividadeService {

    private final ConsultaRepository repository;

    @Override
    public List<TotalConsultasRealizadasNoMesDto> totalConsultasPorMes(StatusConsulta status) {
        log.info("Listando total de consultas por mês com status: {}", status);

        List<Object[]> resultados = repository.totalConsultasPorMes(status);

        return resultados.stream()
                .map(registro -> {
                    Integer ano = (Integer) registro[0];
                    Integer mes = (Integer) registro[1];
                    Long totalConsultas = (Long) registro[2];

                    return new TotalConsultasRealizadasNoMesDto(ano, mes, totalConsultas);
                })
                .toList();
    }

    @Override
    public MediaConsultasDto mediaConsultas() {
        log.info("Calculando média de consultas por dia, semana e mês");

        long totalConsultas = repository.contarConsultasRealizadas();
        Object[] intervalo = (Object[]) repository.intervaloConsultasRealizadas();

        if (intervalo[0] == null || intervalo[1] == null || totalConsultas == 0) {
            return new MediaConsultasDto(0, 0, 0);
        }

        LocalDate dataInicio = (LocalDate) intervalo[0];
        LocalDate dataFim = (LocalDate) intervalo[1];
        long totalDias = ChronoUnit.DAYS.between(dataInicio, dataFim) + 1;

        double mediaPorDia = totalConsultas * 1.0 / totalDias;
        double mediaPorSemana = mediaPorDia * 7;
        double mediaPorMes = mediaPorDia * 30;

        return new MediaConsultasDto(mediaPorDia, mediaPorSemana, mediaPorMes);
    }

    @Override
    public TempoMedioDuracaoDto tempoMedioDuracao() {
        log.info("Calculando tempo médio de duração das consultas");

        Double minutos = repository.tempoMedioDuracaoConsultas();
        return new TempoMedioDuracaoDto(minutos != null ? minutos : 0);
    }

    @Override
    public TempoMedioEsperaDto tempoMedioEspera() {
        log.info("Calculando tempo médio de espera entre agendamento e atendimento");

        final double MINUTOS_POR_DIA = 1440.0;
        Double minutos = repository.tempoMedioEsperaEmMinutos();
        double dias = (minutos != null ? minutos : 0) / MINUTOS_POR_DIA;
        double diasFormatados = formatar(dias);

        return new TempoMedioEsperaDto(diasFormatados);
    }

    @Override
    public TaxaComparecimentoDto taxaComparecimento() {
        log.info("Calculando taxa de comparecimento");

        Double taxa = repository.taxaComparecimento();
        double taxaFormatada = formatar(taxa != null ? taxa : 0);

        return new TaxaComparecimentoDto(taxaFormatada);
    }

    private double formatar(double valor) {
        return BigDecimal.valueOf(valor)
                .setScale(2, RoundingMode.HALF_UP)
                .doubleValue();
    }

}
