package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
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
    public List<Medico> buscarMedicos(String nome, SiglaCrm crmSigla, String crmDigitos, Boolean ativo) {
        boolean nomeInformado = nome != null && !nome.trim().isEmpty();
        boolean crmInformado = crmSigla != null && crmDigitos != null && !crmDigitos.trim().isEmpty();
        boolean filtroAtivo = (ativo != null) ? ativo : true;

        // Prioridade 1: ativo + nome
        if (nomeInformado) {
            return repository.findByNomeContainingIgnoreCaseAndAtivo(nome, filtroAtivo);
        }

        // Prioridade 2: ativo + CRM (Sigla + Dígitos)
        if (crmInformado) {
            return repository.findByCrmSiglaAndCrmDigitosAndAtivo(crmSigla, crmDigitos, filtroAtivo);
        }

        // Prioridade 3: ativo ou nenhum filtro
        return repository.findByAtivo(filtroAtivo);
    }

    @Override
    public Medico atualizar(Long id, Medico medicoAtualizado) {
        Medico medicoExistente = this.buscarPorId(id);

        medicoExistente.setNome(medicoAtualizado.getNome());
        medicoExistente.setEmail(medicoAtualizado.getEmail());
        medicoExistente.setTelefone(medicoAtualizado.getTelefone());
        medicoExistente.setAtivo(medicoExistente.getAtivo());           //Não altera o ativo ao editar - para isso, use os endpoints de ativar e inativar

        medicoExistente.setCrmSigla(medicoAtualizado.getCrmSigla());
        medicoExistente.setCrmDigitos(medicoAtualizado.getCrmDigitos());
        medicoExistente.setEspecialidade(medicoAtualizado.getEspecialidade());

        return repository.save(medicoExistente);
    }

    @Override
    public void removerPorId(Long id) {
        Medico medicoExistente = this.buscarPorId(id);
        if (Boolean.TRUE.equals(medicoExistente.getAtivo())) {
            throw new BusinessRuleException("Médico deve estar inativo para ser excluído.");
        }
        repository.deleteById(id);
    }

    @Override
    public void inativarPorId(Long id) {
        Medico medicoExistente = this.buscarPorId(id);

        if (Boolean.TRUE.equals(medicoExistente.getAtivo())) {
            medicoExistente.setAtivo(false);
            repository.save(medicoExistente);
        }
    }

    @Override
    public void ativarPorId(Long id) {
        Medico medicoExistente = this.buscarPorId(id);

        if (Boolean.FALSE.equals(medicoExistente.getAtivo())) {
            medicoExistente.setAtivo(true);
            repository.save(medicoExistente);
        }
    }

}
