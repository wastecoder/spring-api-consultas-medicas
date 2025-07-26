package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.pacientes.*;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Pessoa;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.services.RelatorioPacienteService;
import com.consultas.api_consultas.utils.SecurityUtil;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioPacienteServiceImpl implements RelatorioPacienteService {

    private final ConsultaRepository consultaRepository;
    private final PacienteRepository pacienteRepository;
    private final PacienteService pacienteService;
    private final SecurityUtil securityUtil;


    @Override
    public List<HistoricoConsultaPacienteDto> historicoPorPaciente(Long id) {
        log.info("Gerando histórico de consultas do paciente id={}", id);

        Paciente paciente = pacienteService.buscarPorId(id);
        if (!securityUtil.canAccessPatient(paciente)) {
            throw new AccessDeniedException("Usuário não tem permissão para acessar os dados desse paciente");
        }

        return consultaRepository.buscarHistoricoPorPaciente(id)
                .stream()
                .map(obj -> {
                    Long idConsulta = (Long) obj[0];
                    LocalDate dataAtendimento = (LocalDate) obj[1];
                    LocalTime horarioAtendimento = (LocalTime) obj[2];
                    StatusConsulta status = (StatusConsulta) obj[3];
                    Pessoa medico = (Pessoa) obj[4];

                    return new HistoricoConsultaPacienteDto(idConsulta, dataAtendimento, horarioAtendimento, status, medico);
                })
                .toList();
    }

    @Override
    public List<CancelamentosPorPacienteDto> cancelamentosPorPaciente() {
        log.info("Gerando relatório de cancelamentos por paciente");

        return consultaRepository.contarCancelamentosPorPaciente()
                .stream()
                .map(obj -> {
                    Long idPaciente = (Long) obj[0];
                    String nomePaciente = (String) obj[1];
                    long totalCancelamentos = ((Number) obj[2]).longValue();

                    return new CancelamentosPorPacienteDto(idPaciente, nomePaciente, totalCancelamentos);
                })
                .toList();
    }

    @Override
    public List<PacientesComMaisConsultasDto> pacientesComMaisConsultasPorPeriodo(LocalDate inicio, LocalDate fim) {
        log.info("Buscando pacientes com mais consultas entre {} e {}", inicio, fim);

        return consultaRepository.pacientesComMaisConsultasPorPeriodo(inicio, fim)
                .stream()
                .map(obj -> {
                    Long idPaciente = (Long) obj[0];
                    String nomePaciente = (String) obj[1];
                    long totalConsultas = ((Number) obj[2]).longValue();

                    return new PacientesComMaisConsultasDto(idPaciente, nomePaciente, totalConsultas);
                })
                .toList();
    }

    @Override
    public List<DistribuicaoPacientesPorSexoDto> distribuicaoPorSexo() {
        log.info("Gerando distribuição de pacientes por sexo");

        return pacienteRepository.distribuicaoPorSexo()
                .stream()
                .map(obj -> {
                    Sexo sexo = (Sexo) obj[0];
                    Long totalPacientes = ((Number) obj[1]).longValue();

                    return new DistribuicaoPacientesPorSexoDto(sexo, totalPacientes);
                })
                .toList();
    }

    @Override
    public List<DistribuicaoPacientesPorFaixaEtariaDto> distribuicaoPorFaixaEtaria() {
        log.info("Gerando distribuição de pacientes por faixa etária");

        return pacienteRepository.distribuicaoPorFaixaEtaria()
                .stream()
                .map(obj -> {
                    String faixaEtaria = (String) obj[0];
                    long totalPacientes = ((Number) obj[1]).longValue();

                    return new DistribuicaoPacientesPorFaixaEtariaDto(faixaEtaria, totalPacientes);
                })
                .toList();
    }

}
