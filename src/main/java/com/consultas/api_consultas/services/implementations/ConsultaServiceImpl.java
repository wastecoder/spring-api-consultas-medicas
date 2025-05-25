package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.ConsultaService;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class ConsultaServiceImpl implements ConsultaService {

    private final ConsultaRepository repository;
    private final MedicoRepository medicoRepository;
    private final PacienteRepository pacienteRepository;


    @Override
    public Consulta salvar(Consulta consultaNova) {
        validarRelacionamentos(consultaNova);
        return repository.save(consultaNova);
    }

    @Override
    public List<Consulta> buscarTodos() {
        return repository.findAllWithMedicoAndPaciente();
    }

    @Override
    public Consulta buscarPorId(Long id) {
        return repository.findByIdWithMedicoAndPaciente(id)
                .orElseThrow(() -> new EntityNotFoundException("Consulta com ID [" + id + "] não encontrada"));
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
        if (!medicoRepository.existsById(medicoId)) {
            throw new EntityNotFoundException("Médico com ID [" + medicoId + "] não encontrado.");
        }

        Long pacienteId = consulta.getPaciente().getId();
        if (!pacienteRepository.existsById(pacienteId)) {
            throw new EntityNotFoundException("Paciente com ID [" + pacienteId + "] não encontrado.");
        }
    }

}
