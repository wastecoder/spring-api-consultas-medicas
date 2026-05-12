package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.PacienteRequisicao;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.mappers.PacienteMapper;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.repositories.specifications.PacienteSpecifications;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.services.rules.PacienteRules;
import com.consultas.api_consultas.utils.SecurityUtil;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@AllArgsConstructor
@Slf4j
public class PacienteServiceImpl implements PacienteService {

    private final PacienteRepository repository;
    private final PacienteRules pacienteRules;
    private final SecurityUtil securityUtil;
    private final PacienteMapper pacienteMapper;


    @Override
    public Paciente salvar(Paciente pacienteNovo) {
        log.info("Salvando novo paciente: {}", pacienteNovo.getNome());

        return repository.save(pacienteNovo);
    }

    @Override
    public List<Paciente> buscarTodos() {
        log.info("Buscando todos os pacientes");

        return repository.findAll();
    }

    @Override
    public Paciente buscarPorId(Long id) {
        log.info("Buscando paciente por ID: {}", id);

        Paciente paciente = repository.findById(id)
                .orElseThrow(() -> {
                    log.warn("Paciente com ID [{}] não encontrado", id);
                    return new EntityNotFoundException("Paciente com ID [" + id + "] não encontrado");
                });


        if (!securityUtil.canAccessPatient(paciente)) {
            throw new AccessDeniedException("Você não tem permissão para acessar este paciente");
        }

        return paciente;
    }

    @Override
    public PageResponse<PacienteResposta> buscarPacientes(int pagina, int tamanho, String nome, String cpf, Sexo sexo, Boolean ativo) {
        log.info("Buscando pacientes - Página: {}, Tamanho: {}, Nome: {}, CPF: {}, Sexo: {}, Ativo: {}",
                pagina, tamanho, nome, cpf, sexo, ativo);

        Pageable pageable = PageRequest.of(pagina, tamanho, Sort.by(Sort.Direction.ASC, "nome"));
        Specification<Paciente> spec = Specification
                .where(PacienteSpecifications.comAtivo(ativo))
                .and(PacienteSpecifications.comNomeContendo(nome))
                .and(PacienteSpecifications.comCpf(cpf))
                .and(PacienteSpecifications.comSexo(sexo));

        return PageResponse.from(repository.findAll(spec, pageable).map(pacienteMapper::paraResposta));
    }

    /**
     * Atualiza os dados cadastrais do paciente.
     * O campo {@code ativo} não é modificado aqui — use os endpoints de ativar/inativar.
     */
    @Override
    public Paciente atualizar(Long id, PacienteRequisicao requisicao) {
        log.info("Atualizando paciente ID: {}", id);

        Paciente pacienteExistente = buscarPorId(id);
        pacienteMapper.aplicarAtualizacao(requisicao, pacienteExistente);

        Paciente pacienteSalvo = repository.save(pacienteExistente);
        log.info("Paciente ID {} atualizado com sucesso", id);
        return pacienteSalvo;
    }

    @Override
    public void removerPorId(Long id) {
        log.info("Solicitação para excluir paciente ID: {}", id);

        Paciente pacienteExistente = this.buscarPorId(id);
        if (Boolean.TRUE.equals(pacienteExistente.getAtivo())) {
            log.warn("Exclusão negada: paciente ID {} está ativo", id);
            throw new BusinessRuleException("Paciente deve estar inativo para ser excluído.");
        }
        repository.deleteById(id);
        log.info("Paciente ID {} excluído com sucesso", id);
    }

    @Override
    public void inativarPorId(Long id) {
        log.info("Inativando paciente ID: {}", id);

        Paciente pacienteExistente = this.buscarPorId(id);
        pacienteRules.verificarSeNaoTemConsultasFuturas(pacienteExistente);

        if (Boolean.TRUE.equals(pacienteExistente.getAtivo())) {
            pacienteExistente.setAtivo(false);
            repository.save(pacienteExistente);
            log.info("Paciente ID {} inativado com sucesso", id);
        } else {
            log.debug("Paciente ID {} já está inativo", id);
        }
    }

    @Override
    public void ativarPorId(Long id) {
        log.info("Ativando paciente ID: {}", id);

        Paciente pacienteExistente = this.buscarPorId(id);
        if (Boolean.FALSE.equals(pacienteExistente.getAtivo())) {
            pacienteExistente.setAtivo(true);
            repository.save(pacienteExistente);
            log.info("Paciente ID {} ativado com sucesso", id);
        } else {
            log.debug("Paciente ID {} já está ativo", id);
        }
    }

    @Override
    public Paciente buscarPorUsernameEAtivo(String username, Boolean ativo) {
        return repository.findByUsuarioUsernameAndAtivo(username, ativo)
                .orElseThrow(() -> new EntityNotFoundException("Paciente associado ao usuário não encontrado"));
    }

}
