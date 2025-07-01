package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPendentesDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasPorDataDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.ConsultasProximosDiasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.operacional.MedicoSemAgendamentoDto;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.services.RelatorioOperacionalService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioOperacionalServiceImpl implements RelatorioOperacionalService {

    private final ConsultaRepository consultaRepository;
    private final MedicoRepository medicoRepository;


    public List<ConsultasPorDataDto> consultasPorData(LocalDate data) {
        LocalDate dataFinal = obterDataValidaOuAtual(data);

        log.info("Buscando consultas marcadas para {}", dataFinal);

        return consultaRepository.buscarConsultasPorData(dataFinal)
                .stream()
                .map(registro -> {
                    Long id = (Long) registro[0];
                    LocalTime horario = (LocalTime) registro[1];
                    String nomeMedico = (String) registro[2];
                    String nomePaciente = (String) registro[3];
                    StatusConsulta status = (StatusConsulta) registro[4];

                    return new ConsultasPorDataDto(id, horario, nomeMedico, nomePaciente, status);
                })
                .toList();
    }

    public List<ConsultasProximosDiasDto> consultasProximosDias() {
        LocalDate hoje = LocalDate.now();
        LocalDate limite = hoje.plusDays(7);

        log.info("Buscando consultas entre {} e {}", hoje, limite);

        return consultaRepository.buscarConsultasProximosDias(hoje, limite)
                .stream()
                .map(registro -> {
                    Long id = (Long) registro[0];
                    LocalDate data = (LocalDate) registro[1];
                    String nomeMedico = (String) registro[2];
                    String nomePaciente = (String) registro[3];
                    StatusConsulta status = (StatusConsulta) registro[4];

                    return new ConsultasProximosDiasDto(id, data, nomeMedico, nomePaciente, status);
                })
                .toList();
    }

    public List<ConsultasPendentesDto> consultasPendentes() {
        log.info("Buscando consultas agendadas no passado (pendentes)");

        return consultaRepository.buscarConsultasPendentes()
                .stream()
                .map(registro -> {
                    Long id = (Long) registro[0];
                    LocalDate data = (LocalDate) registro[1];
                    String nomeMedico = (String) registro[2];
                    String nomePaciente = (String) registro[3];

                    return new ConsultasPendentesDto(id, data, nomeMedico, nomePaciente);
                })
                .toList();
    }

    public List<MedicoSemAgendamentoDto> medicosSemAgendamento(Integer ano, Integer mes) {
        int mesFinal = obterMesValidoOuAtual(mes);
        int anoFinal = obterAnoValidoOuAtual(ano);

        log.info("Buscando médicos sem agendamento no mês {}/{}", mesFinal, anoFinal);

        return medicoRepository.buscarMedicosSemAgendamentoNoMes(anoFinal, mesFinal)
                .stream()
                .map(registro -> {
                    Long id = (Long) registro[0];
                    String nome = (String) registro[1];
                    Especialidade especialidade = (Especialidade) registro[2];

                    return new MedicoSemAgendamentoDto(id, nome, especialidade);
                })
                .toList();
    }


    private LocalDate obterDataValidaOuAtual(LocalDate data) {
        if (data == null) {
            LocalDate hoje = LocalDate.now();
            log.debug("Data não informada. Usando data atual: {}", hoje);
            return hoje;
        }

        LocalDate limiteInferior = LocalDate.of(2000, 1, 1);
        LocalDate limiteSuperior = LocalDate.now().plusYears(5);

        if (data.isBefore(limiteInferior) || data.isAfter(limiteSuperior)) {
            log.warn("Data inválida recebida: {}. Deve estar entre {} e {}.", data, limiteInferior, limiteSuperior);
            throw new IllegalArgumentException("Data inválida: " + data + ". Deve estar entre " + limiteInferior + " e " + limiteSuperior + ".");
        }

        return data;
    }

    private int obterMesValidoOuAtual(Integer mes) {
        if (mes == null) return LocalDate.now().getMonthValue();

        if (mes < 1 || mes > 12) {
            log.warn("Mês inválido recebido: {}. Deve estar entre 1 e 12.", mes);
            throw new IllegalArgumentException("Mês inválido: " + mes + ". Valor deve estar entre 1 e 12.");
        }
        return mes;
    }

    private int obterAnoValidoOuAtual(Integer ano) {
        int anoAtual = LocalDate.now().getYear();
        if (ano == null) return anoAtual;

        final int ANO_MIN = 2000;
        final int ANO_MAX = anoAtual + 10;
        if (ano < ANO_MIN || ano > ANO_MAX) {
            log.warn("Ano inválido recebido: {}. Deve estar entre {} e {}.", ano, ANO_MIN, ANO_MAX);
            throw new IllegalArgumentException("Ano inválido: " + ano + ". Deve estar entre " + ANO_MIN + " e " + ANO_MAX + ".");
        }
        return ano;
    }

}
