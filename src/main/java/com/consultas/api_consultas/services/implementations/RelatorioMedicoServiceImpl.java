package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.medicos.*;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.services.RelatorioMedicoService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.time.Year;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioMedicoServiceImpl implements RelatorioMedicoService {

    private final ConsultaRepository consultaRepository;
    private final MedicoRepository medicoRepository;


    @Override
    public List<ConsultasRealizadasPorMedicoDto> consultasRealizadasPorMedico() {
        log.info("Gerando relatório de consultas realizadas por médico");

        return consultaRepository.contarConsultasRealizadasPorMedico()
                .stream()
                .map(obj -> new ConsultasRealizadasPorMedicoDto(
                        (Long) obj[0],
                        (String) obj[1],
                        ((Number) obj[2]).longValue()
                ))
                .toList();
    }

    @Override
    public List<MedicosComMaisConsultasNoMesDto> medicosComMaisConsultasNoMes(int mes, int ano) {
        int anoMaximo = Year.now().getValue() + 5;

        if (mes < 1 || mes > 12) {
            log.warn("Mês inválido recebido no relatório: {}", mes);
            throw new IllegalArgumentException("Mês inválido: " + mes + ". O valor deve estar entre 1 e 12.");
        }

        if (ano < 2000 || ano > anoMaximo) {
            log.warn("Ano inválido recebido no relatório: {}", ano);
            throw new IllegalArgumentException("Ano inválido: " + ano + ". O valor deve estar entre 2000 e " + anoMaximo + ".");
        }

        log.info("Gerando relatório de médicos com mais consultas no mês: {}/{}", mes, ano);

        return consultaRepository.medicosComMaisConsultasNoMes(mes, ano)
                .stream()
                .map(obj -> new MedicosComMaisConsultasNoMesDto(
                        (Long) obj[0],
                        (String) obj[1],
                        ((Number) obj[2]).longValue()
                ))
                .toList();
    }

    @Override
    public List<MedicosPorEspecialidadeDto> medicosPorEspecialidade() {
        log.info("Gerando relatório de médicos por especialidade");

        return medicoRepository.contarMedicosPorEspecialidade()
                .stream()
                .map(obj -> new MedicosPorEspecialidadeDto(
                        (Especialidade) obj[0],
                        ((Number) obj[1]).longValue()
                ))
                .toList();
    }

    @Override
    public List<TaxaCancelamentoPorMedicoDto> taxaCancelamentoPorMedico() {
        log.info("Gerando relatório de taxa de cancelamentos por médico");

        DecimalFormat df = new DecimalFormat("0.00'%'" );

        return consultaRepository.calcularTaxaCancelamentoPorMedico()
                .stream()
                .map(obj -> new TaxaCancelamentoPorMedicoDto(
                        (Long) obj[0],
                        (String) obj[1],
                        df.format(((Number) obj[2]).doubleValue())
                ))
                .toList();
    }

    @Override
    public List<FaturamentoPorMedicoDto> faturamentoPorMedico() {
        log.info("Gerando relatório de faturamento por médico");

        return consultaRepository.calcularFaturamentoPorMedico()
                .stream()
                .map(obj -> new FaturamentoPorMedicoDto(
                        (Long) obj[0],
                        (String) obj[1],
                        (BigDecimal) obj[2]
                ))
                .toList();
    }

}
