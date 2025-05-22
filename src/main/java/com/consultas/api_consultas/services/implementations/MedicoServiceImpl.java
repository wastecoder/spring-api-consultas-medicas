package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.services.MedicoService;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class MedicoServiceImpl implements MedicoService {

    private final MedicoRepository repository;


    @Override
    public Medico salvar(Medico medicoNovo) {
        return repository.save(medicoNovo);
    }

    @Override
    public List<Medico> buscarTodos() {
        return repository.findAll();
    }

    @Override
    public Medico buscarPorId(Long id) {
        return repository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Médico com ID [" + id + "] não encontrado"));
    }

    @Override
    public Medico atualizar(Long id, Medico medicoAtualizado) {
        Medico medicoExistente = this.buscarPorId(id);

        medicoExistente.setNome(medicoAtualizado.getNome());
        medicoExistente.setEmail(medicoAtualizado.getEmail());
        medicoExistente.setTelefone(medicoAtualizado.getTelefone());
        medicoExistente.setAtivo(medicoAtualizado.getAtivo());

        medicoExistente.setCrmSigla(medicoAtualizado.getCrmSigla());
        medicoExistente.setCrmDigitos(medicoAtualizado.getCrmDigitos());
        medicoExistente.setEspecialidade(medicoAtualizado.getEspecialidade());

        return repository.save(medicoExistente);
    }

    @Override
    public void removerPorId(Long id) {
        repository.deleteById(id);
    }

}
