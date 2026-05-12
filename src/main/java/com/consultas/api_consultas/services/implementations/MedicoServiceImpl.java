package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.mappers.MedicoMapper;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.repositories.specifications.MedicoSpecifications;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.rules.MedicoRules;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
@AllArgsConstructor
@Slf4j
public class MedicoServiceImpl implements MedicoService {

    private final MedicoRepository repository;
    private final MedicoRules medicoRules;
    private final MedicoMapper medicoMapper;


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

    private static final Map<String, String[]> CAMPOS_ORDENACAO = Map.of(
            "nome",          new String[] { "nome" },
            "crm",           new String[] { "crmSigla", "crmDigitos" },
            "especialidade", new String[] { "especialidade" }
    );

    @Override
    public PageResponse<MedicoResposta> buscarMedicos(
            int pagina,
            int tamanho,
            String nome,
            SiglaCrm crmSigla,
            String crmDigitos,
            Boolean ativo,
            String ordenarPor,
            String direcao
    ) {
        log.info("Buscando médicos - Página: {}, Tamanho: {}, Nome: {}, CRM: {} {}, Ativo: {}, OrdenarPor: {}, Direção: {}",
                pagina, tamanho, nome, crmSigla, crmDigitos, ativo, ordenarPor, direcao);

        Sort sort = construirOrdenacao(ordenarPor, direcao);
        Pageable pageable = PageRequest.of(pagina, tamanho, sort);
        Specification<Medico> spec = Specification
                .where(MedicoSpecifications.comAtivo(ativo))
                .and(MedicoSpecifications.comNomeContendo(nome))
                .and(MedicoSpecifications.comCrm(crmSigla, crmDigitos));

        return PageResponse.from(repository.findAll(spec, pageable).map(medicoMapper::paraResposta));
    }

    private Sort construirOrdenacao(String ordenarPor, String direcao) {
        String[] campos = CAMPOS_ORDENACAO.get(ordenarPor);
        if (campos == null) {
            throw new BusinessRuleException("Campo de ordenação inválido: " + ordenarPor);
        }
        Sort.Direction direction;
        try {
            direction = Sort.Direction.fromString(direcao);
        } catch (IllegalArgumentException e) {
            throw new BusinessRuleException("Direção de ordenação inválida: " + direcao);
        }
        return Sort.by(direction, campos);
    }

    /**
     * Atualiza os dados cadastrais do médico.
     * O campo {@code ativo} não é modificado aqui — use os endpoints de ativar/inativar.
     */
    @Override
    public Medico atualizar(Long id, MedicoRequisicao requisicao) {
        log.info("Atualizando médico ID: {}", id);

        Medico medicoExistente = this.buscarPorId(id);
        medicoMapper.aplicarAtualizacao(requisicao, medicoExistente);

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
    public Medico buscarPorUsernameEAtivo(String username, Boolean ativo) {
        return repository.findByUsuarioUsernameAndAtivo(username, ativo)
                .orElseThrow(() -> new EntityNotFoundException("Médico associado ao usuário não encontrado"));
    }

}
