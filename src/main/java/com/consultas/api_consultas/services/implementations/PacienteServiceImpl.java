package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.PacienteService;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Sort;
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
    public List<Paciente> buscarPacientes(String nome, String cpf, Sexo sexo, Boolean ativo) {
        boolean nomeInformado = nome != null && !nome.trim().isEmpty();
        boolean cpfInformado = cpf != null && !cpf.trim().isEmpty();
        boolean sexoInformado = sexo != null;
        boolean filtroAtivo = (ativo != null) ? ativo : true;

        Sort ordenarPorNome = Sort.by("nome").ascending();

        // Prioridade 1: ativo + nome
        if (nomeInformado) {
            return repository.findByNomeContainingIgnoreCaseAndAtivo(nome, filtroAtivo, ordenarPorNome);
        }

        // Prioridade 2: CPF
        if (cpfInformado) {
            return repository.findByCpf(cpf)
                    .map(List::of).orElseGet(List::of);
        }

        // Prioridade 3: sexo
        if (sexoInformado) {
            return repository.findBySexoAndAtivo(sexo, filtroAtivo, ordenarPorNome);
        }

        // Prioridade 4: ativo ou nenhum filtro
        return repository.findByAtivo(filtroAtivo, ordenarPorNome);
    }

    @Override
    public Paciente atualizar(Long id, Paciente pacienteAtualizado) {
        Paciente pacienteExistente = buscarPorId(id);

        pacienteExistente.setNome(pacienteAtualizado.getNome());
        pacienteExistente.setEmail(pacienteAtualizado.getEmail());
        pacienteExistente.setTelefone(pacienteAtualizado.getTelefone());
//        pacienteExistente.setAtivo(pacienteExistente.getAtivo());           //Não altera o ativo ao editar

        pacienteExistente.setCpf(pacienteAtualizado.getCpf());
        pacienteExistente.setDataNascimento(pacienteAtualizado.getDataNascimento());

        return repository.save(pacienteExistente);
    }

    @Override
    public void removerPorId(Long id) {
        Paciente pacienteExistente = this.buscarPorId(id);
        if (Boolean.TRUE.equals(pacienteExistente.getAtivo())) {
            throw new BusinessRuleException("Paciente deve estar inativo para ser excluído.");
        }
        repository.deleteById(id);
    }


    @Override
    public void inativarPorId(Long id) {
        Paciente pacienteExistente = this.buscarPorId(id);

        if (Boolean.TRUE.equals(pacienteExistente.getAtivo())) {
            pacienteExistente.setAtivo(false);
            repository.save(pacienteExistente);
        }
    }

    @Override
    public void ativarPorId(Long id) {
        Paciente pacienteExistente = this.buscarPorId(id);

        if (Boolean.FALSE.equals(pacienteExistente.getAtivo())) {
            pacienteExistente.setAtivo(true);
            repository.save(pacienteExistente);
        }
    }

}
