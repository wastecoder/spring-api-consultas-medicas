package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.ConsultaService;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
public class ConsultaServiceImpl implements ConsultaService {

    private final ConsultaRepository repository;


    @Override
    public Consulta salvar(Consulta consultaNova) {
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
    public Consulta atualizar(Long id, Consulta consultaAtualizada) {
        Consulta consultaExistente = this.buscarPorId(id);

        consultaExistente.setDataAtendimento(consultaAtualizada.getDataAtendimento());
        consultaExistente.setPreco(consultaAtualizada.getPreco());
        consultaExistente.setMotivo(consultaAtualizada.getMotivo());
        consultaExistente.setStatus(consultaAtualizada.getStatus());
        consultaExistente.setMedico(consultaAtualizada.getMedico());
        consultaExistente.setPaciente(consultaAtualizada.getPaciente());

        return repository.save(consultaExistente);
    }

    @Override
    public void removerPorId(Long id) {
        repository.deleteById(id);
    }

}
