package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.rules.MedicoRules;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
@Slf4j
public class MedicoServiceImpl implements MedicoService {

    private final MedicoRepository repository;
    private final MedicoRules medicoRules;


    @Override
    public Medico salvar(Medico medicoNovo) {
        log.info("Salvando novo médico: {}", medicoNovo.getNome());

        return repository.save(medicoNovo);
    }

    @Override
    public List<Medico> buscarTodos() {
        log.info("Buscando todos os médicos");

        return repository.findAll();
    }

    @Override
    public Medico buscarPorId(Long id) {
        log.info("Buscando médico por ID: {}", id);

        return repository.findById(id)
                .orElseThrow(() -> {
                    log.warn("Médico com ID [{}] não encontrado", id);
                    return new EntityNotFoundException("Médico com ID [" + id + "] não encontrado");
                });
    }

    @Override
    public List<Medico> buscarMedicos(String nome, SiglaCrm crmSigla, String crmDigitos, Boolean ativo) {
        log.info("Buscando médicos com filtros - Nome: {}, CRM: {} {}, Ativo: {}", nome, crmSigla, crmDigitos, ativo);

        boolean nomeInformado = nome != null && !nome.trim().isEmpty();
        boolean crmInformado = crmSigla != null && crmDigitos != null && !crmDigitos.trim().isEmpty();
        boolean filtroAtivo = (ativo != null) ? ativo : true;

        Sort ordenarPorNome = Sort.by("nome").ascending();

        // Prioridade 1: ativo + nome
        if (nomeInformado) {
            log.debug("Filtro aplicado: nome + ativo");
            return repository.findByNomeContainingIgnoreCaseAndAtivo(nome, filtroAtivo, ordenarPorNome);
        }

        // Prioridade 2: ativo + CRM (Sigla + Dígitos)
        if (crmInformado) {
            log.debug("Filtro aplicado: CRM completo");
            return repository.findByCrmSiglaAndCrmDigitos(crmSigla, crmDigitos)
                    .map(List::of).orElseGet(List::of);
        }

        // Prioridade 3: ativo ou nenhum filtro
        log.debug("Filtro aplicado: apenas ativo ou nenhum filtro");
        return repository.findByAtivo(filtroAtivo, ordenarPorNome);
    }

    @Override
    public Medico atualizar(Long id, Medico medicoAtualizado) {
        log.info("Atualizando médico ID: {}", id);

        Medico medicoExistente = this.buscarPorId(id);

        medicoExistente.setNome(medicoAtualizado.getNome());
        medicoExistente.setEmail(medicoAtualizado.getEmail());
        medicoExistente.setTelefone(medicoAtualizado.getTelefone());
//        medicoExistente.setAtivo(medicoExistente.getAtivo());           //Não altera o ativo ao editar - para isso, use os endpoints de ativar e inativar

        medicoExistente.setCrmSigla(medicoAtualizado.getCrmSigla());
        medicoExistente.setCrmDigitos(medicoAtualizado.getCrmDigitos());
        medicoExistente.setEspecialidade(medicoAtualizado.getEspecialidade());

        Medico medicoSalvo = repository.save(medicoExistente);
        log.info("Médico ID {} atualizado com sucesso", id);
        return medicoSalvo;
    }

    @Override
    public void removerPorId(Long id) {
        log.info("Solicitação para excluir médico ID: {}", id);

        Medico medicoExistente = this.buscarPorId(id);
        if (Boolean.TRUE.equals(medicoExistente.getAtivo())) {
            log.warn("Exclusão negada: médico ID {} está ativo", id);
            throw new BusinessRuleException("Médico deve estar inativo para ser excluído.");
        }
        repository.deleteById(id);
        log.info("Médico ID {} excluído com sucesso", id);
    }

    @Override
    public void inativarPorId(Long id) {
        log.info("Inativando médico ID: {}", id);

        Medico medicoExistente = this.buscarPorId(id);
        medicoRules.verificarSeNaoTemConsultasFuturas(medicoExistente);

        if (Boolean.TRUE.equals(medicoExistente.getAtivo())) {
            medicoExistente.setAtivo(false);
            repository.save(medicoExistente);
            log.info("Médico ID {} inativado com sucesso", id);
        } else {
            log.debug("Médico ID {} já está inativo", id);
        }
    }

    @Override
    public void ativarPorId(Long id) {
        log.info("Ativando médico ID: {}", id);

        Medico medicoExistente = this.buscarPorId(id);
        if (Boolean.FALSE.equals(medicoExistente.getAtivo())) {
            medicoExistente.setAtivo(true);
            repository.save(medicoExistente);
            log.info("Médico ID {} ativado com sucesso", id);
        } else {
            log.debug("Médico ID {} já está ativo", id);
        }
    }

    @Override
    public Medico buscarPorUsuario(Usuario usuario) {
        return repository.findByUsuario(usuario)
                .orElseThrow(() -> new EntityNotFoundException("Médico associado ao usuário não encontrado"));
    }

}
