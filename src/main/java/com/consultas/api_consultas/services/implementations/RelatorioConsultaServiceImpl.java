package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.consultas.*;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.RelatorioConsultaService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioConsultaServiceImpl implements RelatorioConsultaService {

    private final ConsultaRepository repository;


    @Override
    public ConsultasPorStatusDto gerarRelatorioConsultasPorStatus() {
        log.info("Gerando relatório de consultas por status");

        List<Object[]> resultados = repository.contarConsultasPorStatus();

        long agendadas = 0;
        long canceladas = 0;
        long realizadas = 0;

        for (Object[] resultado : resultados) {
            String status = (String) resultado[0];
            long total = ((Number) resultado[1]).longValue();

            switch (status) {
                case "AGENDADA" -> agendadas = total;
                case "CANCELADA" -> canceladas = total;
                case "REALIZADA" -> realizadas = total;
                default -> log.warn("Status desconhecido encontrado no relatório: {}", status);
            }
        }

        return new ConsultasPorStatusDto(agendadas, canceladas, realizadas);
    }

    @Override
    public List<ConsultasPorMesDto> consultasPorMes() {
        log.info("Gerando relatório de consultas por mês");

        List<Object[]> resultados = repository.contarConsultasPorMes();

        return resultados.stream()
                .map(obj -> new ConsultasPorMesDto(
                        ((Number) obj[0]).intValue(),
                        ((Number) obj[1]).longValue()
                ))
                .toList();
    }

    @Override
    public List<ConsultasPorAnoDto> consultasPorAno() {
        log.info("Gerando relatório de consultas por ano");

        List<Object[]> resultados = repository.contarConsultasPorAno();

        return resultados.stream()
                .map(obj -> new ConsultasPorAnoDto(
                        ((Number) obj[0]).intValue(),
                        ((Number) obj[1]).longValue()
                ))
                .toList();
    }

    @Override
    public List<ConsultasPorEspecialidadeDto> consultasPorEspecialidade() {
        log.info("Gerando relatório de consultas por especialidade");

        return repository.contarConsultasPorEspecialidade()
                .stream()
                .map(obj -> new ConsultasPorEspecialidadeDto((Especialidade) obj[0], ((Number) obj[1]).longValue()))
                .toList();
    }

    @Override
    public List<ConsultaResumoDto> consultasPorPaciente(Long id) {
        log.info("Listando consultas do paciente id={}", id);

        return repository.findByPacienteId(id).stream()
                .map(ConsultaResumoDto::new)
                .toList();
    }

    @Override
    public List<ConsultaResumoDto> consultasPorMedico(Long id) {
        log.info("Listando consultas do médico id={}", id);

        return repository.findByMedicoId(id).stream()
                .map(ConsultaResumoDto::new)
                .toList();
    }

    @Override
    public List<ConsultaResumoDto> consultasPorPeriodo(LocalDate inicio, LocalDate fim) {
        log.info("Buscando consultas no período de {} até {}", inicio, fim);

        return repository.findByDataAtendimentoBetween(inicio, fim).stream()
                .map(ConsultaResumoDto::new)
                .toList();
    }

}
