package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.ConsultaService;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.PacienteService;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Service
@AllArgsConstructor
@Slf4j
public class ConsultaServiceImpl implements ConsultaService {

    private final ConsultaRepository repository;
    private final MedicoService medicoService;
    private final PacienteService pacienteService;


    @Override
    public Consulta salvar(Consulta consultaNova) {
        log.info("Salvando nova consulta para médico ID {} e paciente ID {}",
                consultaNova.getMedico().getId(), consultaNova.getPaciente().getId());

        validarRelacionamentos(consultaNova);
        return repository.save(consultaNova);
    }

    @Override
    public List<Consulta> buscarTodos() {
        log.info("Buscando todas as consultas");

        return repository.findAll();
    }

    @Override
    public Consulta buscarPorId(Long id) {
        log.info("Buscando consulta por ID: {}", id);

        return repository.findById(id)
                .orElseThrow(() -> {
                    log.warn("Consulta com ID [{}] não encontrada", id);
                    return new EntityNotFoundException("Consulta com ID [" + id + "] não encontrada");
                });
    }

    @Override
    public List<Consulta> buscarConsultas(Long medicoId, Long pacienteId, LocalDate dataAtendimento, StatusConsulta statusConsulta) {
        log.info("Buscando consultas com filtros - Médico ID: {}, Paciente ID: {}, Data: {}, Status: {}",
                medicoId, pacienteId, dataAtendimento, statusConsulta);

        boolean temMedico = medicoId != null;
        boolean temPaciente = pacienteId != null;
        boolean temDataAtendimento = dataAtendimento != null;
        StatusConsulta status = (statusConsulta != null) ? statusConsulta : StatusConsulta.AGENDADA;

        Sort ordernarPorMaisRecente = Sort.by("dataAtendimento").ascending();

        // Prioridade 1: data atendimento + status
        if (temDataAtendimento) {
            log.debug("Filtro aplicado: dataAtendimento + status");
            return repository.findByDataAtendimentoAndStatus(dataAtendimento, status, ordernarPorMaisRecente);
        }

        // Prioridade 2: medico + paciente + status
        if (temMedico && temPaciente) {
            log.debug("Filtro aplicado: medico + paciente + status");
            Medico medico = medicoService.buscarPorId(medicoId);
            Paciente paciente = pacienteService.buscarPorId(pacienteId);
            return repository.findByMedicoAndPacienteAndStatus(medico, paciente, status, ordernarPorMaisRecente);
        }

        // Prioridade 3: medico + status
        if (temMedico) {
            log.debug("Filtro aplicado: medico + status");
            Medico medico = medicoService.buscarPorId(medicoId);
            return repository.findByMedicoAndStatus(medico, status, ordernarPorMaisRecente);
        }

        // Prioridade 4: paciente + status
        if (temPaciente) {
            log.debug("Filtro aplicado: paciente + status");
            Paciente paciente = pacienteService.buscarPorId(pacienteId);
            return repository.findByPacienteAndStatus(paciente, status, ordernarPorMaisRecente);
        }

        log.debug("Filtro aplicado: apenas status ou nenhum filtro");
        return repository.findByStatus(status, ordernarPorMaisRecente);
    }

    @Override
    public Consulta atualizar(Long id, Consulta consultaAtualizada) {
        log.info("Atualizando consulta ID: {}", id);

        Consulta consultaExistente = this.buscarPorId(id);
        validarRelacionamentos(consultaAtualizada);

        consultaExistente.setDataAtendimento(consultaAtualizada.getDataAtendimento());
        consultaExistente.setHorarioAtendimento(consultaAtualizada.getHorarioAtendimento());
        consultaExistente.setDuracaoEmMinutos(consultaAtualizada.getDuracaoEmMinutos());
        consultaExistente.setPreco(consultaAtualizada.getPreco());
        consultaExistente.setMotivo(consultaAtualizada.getMotivo());
        consultaExistente.setStatus(consultaAtualizada.getStatus());
        consultaExistente.setMedico(consultaAtualizada.getMedico());
        consultaExistente.setPaciente(consultaAtualizada.getPaciente());

        Consulta consultaSalva = repository.save(consultaExistente);
        log.info("Consulta ID {} atualizada com sucesso", id);
        return consultaSalva;
    }

    @Override
    public void removerPorId(Long id) {
        log.info("Removendo consulta ID: {}", id);

        this.buscarPorId(id); // lança exceção se não encontrar
        repository.deleteById(id);
        log.info("Consulta ID {} removida com sucesso", id);
    }

    private void validarRelacionamentos(Consulta consulta) {
        Long medicoId = consulta.getMedico().getId();
        Long pacienteId = consulta.getPaciente().getId();

        log.debug("Validando relacionamentos - Médico ID: {}, Paciente ID: {}", medicoId, pacienteId);

        medicoService.buscarPorId(medicoId);
        pacienteService.buscarPorId(pacienteId);
    }

}
