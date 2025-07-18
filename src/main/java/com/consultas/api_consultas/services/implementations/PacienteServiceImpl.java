package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.services.rules.PacienteRules;
import com.consultas.api_consultas.utils.SecurityUtil;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
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
    public List<Paciente> buscarPacientes(String nome, String cpf, Sexo sexo, Boolean ativo) {
        log.info("Buscando pacientes com filtros - Nome: {}, CPF: {}, Sexo: {}, Ativo: {}", nome, cpf, sexo, ativo);

        boolean nomeInformado = nome != null && !nome.trim().isEmpty();
        boolean cpfInformado = cpf != null && !cpf.trim().isEmpty();
        boolean sexoInformado = sexo != null;
        boolean filtroAtivo = (ativo != null) ? ativo : true;

        Sort ordenarPorNome = Sort.by("nome").ascending();

        // Prioridade 1: ativo + nome
        if (nomeInformado) {
            log.debug("Filtro aplicado: nome + ativo");
            return repository.findByNomeContainingIgnoreCaseAndAtivo(nome, filtroAtivo, ordenarPorNome);
        }

        // Prioridade 2: CPF
        if (cpfInformado) {
            log.debug("Filtro aplicado: CPF");
            return repository.findByCpf(cpf)
                    .map(List::of).orElseGet(List::of);
        }

        // Prioridade 3: sexo
        if (sexoInformado) {
            log.debug("Filtro aplicado: sexo + ativo");
            return repository.findBySexoAndAtivo(sexo, filtroAtivo, ordenarPorNome);
        }

        log.debug("Filtro aplicado: apenas ativo ou nenhum filtro");
        return repository.findByAtivo(filtroAtivo, ordenarPorNome);
    }

    @Override
    public Paciente atualizar(Long id, Paciente pacienteAtualizado) {
        log.info("Atualizando paciente ID: {}", id);
        Paciente pacienteExistente = buscarPorId(id);

        pacienteExistente.setNome(pacienteAtualizado.getNome());
        pacienteExistente.setEmail(pacienteAtualizado.getEmail());
        pacienteExistente.setTelefone(pacienteAtualizado.getTelefone());
//        pacienteExistente.setAtivo(pacienteExistente.getAtivo());           //Não altera o ativo ao editar

        pacienteExistente.setCpf(pacienteAtualizado.getCpf());
        pacienteExistente.setDataNascimento(pacienteAtualizado.getDataNascimento());

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
    public Paciente buscarPorUsuario(Usuario usuario) {
        return repository.findByUsuario(usuario)
                .orElseThrow(() -> new EntityNotFoundException("Paciente associado ao usuário não encontrado"));
    }

}
