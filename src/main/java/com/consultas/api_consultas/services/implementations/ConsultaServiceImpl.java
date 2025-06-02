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
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Service
@AllArgsConstructor
public class ConsultaServiceImpl implements ConsultaService {

    private final ConsultaRepository repository;
    private final MedicoService medicoService;
    private final PacienteService pacienteService;


    @Override
    public Consulta salvar(Consulta consultaNova) {
        validarRelacionamentos(consultaNova);
        return repository.save(consultaNova);
    }

    @Override
    public List<Consulta> buscarTodos() {
        return repository.findAll();
    }

    @Override
    public Consulta buscarPorId(Long id) {
        return repository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Consulta com ID [" + id + "] não encontrada"));
    }

    @Override
    public List<Consulta> buscarConsultas(Long medicoId, Long pacienteId, LocalDate dataAtendimento, StatusConsulta statusConsulta) {
        boolean temMedico = medicoId != null;
        boolean temPaciente = pacienteId != null;
        boolean temDataAtendimento = dataAtendimento != null;
        StatusConsulta status = (statusConsulta != null) ? statusConsulta : StatusConsulta.AGENDADA;

        Sort ordernarPorMaisRecente = Sort.by("dataAtendimento").ascending();

        // Prioridade 1: data atendimento + status
        if (temDataAtendimento) {
            return repository.findByDataAtendimentoAndStatus(dataAtendimento, status, ordernarPorMaisRecente);
        }

        // Prioridade 2: medico + paciente + status
        if (temMedico && temPaciente) {
            Medico medico = medicoService.buscarPorId(medicoId);
            Paciente paciente = pacienteService.buscarPorId(pacienteId);
            return repository.findByMedicoAndPacienteAndStatus(medico, paciente, status, ordernarPorMaisRecente);
        }

        // Prioridade 3: medico + status
        if (temMedico) {
            Medico medico = medicoService.buscarPorId(medicoId);
            return repository.findByMedicoAndStatus(medico, status, ordernarPorMaisRecente);
        }

        // Prioridade 4: paciente + status
        if (temPaciente) {
            Paciente paciente = pacienteService.buscarPorId(pacienteId);
            return repository.findByPacienteAndStatus(paciente, status, ordernarPorMaisRecente);
        }

        // Prioridade 5: status ou nenhum filtro
        return repository.findByStatus(status, ordernarPorMaisRecente);
    }

    @Override
    public Consulta atualizar(Long id, Consulta consultaAtualizada) {
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

        repository.save(consultaExistente);
        return this.buscarPorId(id);        //Se retornar o save() dará LazyException
    }

    @Override
    public void removerPorId(Long id) {
        this.buscarPorId(id);           // lança EntityNotFoundException se não existir
        repository.deleteById(id);
    }

    private void validarRelacionamentos(Consulta consulta) {
        Long medicoId = consulta.getMedico().getId();
        medicoService.buscarPorId(medicoId);

        Long pacienteId = consulta.getPaciente().getId();
        pacienteService.buscarPorId(pacienteId);
    }

}
