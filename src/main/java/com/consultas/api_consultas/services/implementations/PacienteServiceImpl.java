package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.PacienteService;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class PacienteServiceImpl implements PacienteService {

    private final PacienteRepository repository;


    @Override
    public Paciente salvar(Paciente pacienteNovo) {
        return repository.save(pacienteNovo);
    }

    @Override
    public List<Paciente> buscarTodos() {
        return repository.findAll();
    }

    @Override
    public Paciente buscarPorId(Long id) {
        return repository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Paciente com ID [" + id + "] não encontrado"));
    }

    @Override
    public Paciente atualizar(Long id, Paciente pacienteAtualizado) {
        Paciente pacienteExistente = buscarPorId(id);

        pacienteExistente.setNome(pacienteAtualizado.getNome());
        pacienteExistente.setEmail(pacienteAtualizado.getEmail());
        pacienteExistente.setTelefone(pacienteAtualizado.getTelefone());
        pacienteExistente.setAtivo(pacienteAtualizado.getAtivo());

        pacienteExistente.setCpf(pacienteAtualizado.getCpf());
        pacienteExistente.setDataNascimento(pacienteAtualizado.getDataNascimento());

        return repository.save(pacienteExistente);
    }

    @Override
    public void removerPorId(Long id) {
        repository.deleteById(id);
    }

}
