package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.services.UsuarioService;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class UsuarioServiceImpl implements UsuarioService {

    private final UsuarioRepository usuarioRepository;
    private final MedicoService medicoService;
    private final PacienteService pacienteService;
    private final PasswordEncoder passwordEncoder;


    @Override
    @Transactional
    public UsuarioResposta salvar(UsuarioCadastroDto requisicao) {
        log.info("Salvando novo usuário: {}", requisicao.getUsername());

        validarUsernameDuplicado(requisicao.getUsername(), null);
        validarRegrasDeAssociacao(requisicao);

        Usuario usuario = new Usuario();
        usuario.setUsername(requisicao.getUsername());
        usuario.setSenha(passwordEncoder.encode(requisicao.getSenha()));
        usuario.setFuncao(requisicao.getFuncao());
        usuario.setAtivo(true);

        usuario = usuarioRepository.save(usuario);

        log.info(">>> Usuário salvo no banco de dados: {}", usuario);

        associarUsuarioAoMedicoOuPaciente(requisicao, usuario);

        return new UsuarioResposta(usuario);
    }

    @Override
    public List<Usuario> buscarTodos() {
        log.info("Buscando todos os usuários");

        return usuarioRepository.findAll();
    }

    @Override
    public Usuario buscarPorId(Long id) {
        log.info("Buscando usuário por ID: {}", id);

        return usuarioRepository.findById(id)
                .orElseThrow(() -> {
                    log.warn("Usuário com ID [{}] não encontrado", id);
                    return new EntityNotFoundException("Usuário com ID [" + id + "] não encontrado");
                });
    }

    @Override
    public Usuario atualizar(Long id, UsuarioAtualizacaoDto req) {
        log.info("Atualizando usuário ID: {}", id);

        Usuario existente = buscarPorId(id);

        validarUsernameDuplicado(req.getUsername(), id);

        existente.setUsername(req.getUsername());

        if (req.getSenha() != null && !req.getSenha().isBlank()) {
            existente.setSenha(passwordEncoder.encode(req.getSenha()));
        }

        Usuario salvo = usuarioRepository.save(existente);
        log.info("Usuário ID {} atualizado com sucesso", id);

        return salvo;
    }

    @Override
    public void removerPorId(Long id) {
        log.info("Excluindo usuário ID {}", id);

        Usuario usuario = buscarPorId(id);

        if (Boolean.TRUE.equals(usuario.getAtivo())) {
            log.warn("Exclusão negada: usuário ID {} está ativo", id);
            throw new BusinessRuleException("Usuário deve estar inativo para ser excluído.");
        }

        desassociarUsuarioDePessoa(usuario);

        usuarioRepository.deleteById(id);
        log.info("Usuário ID {} excluído com sucesso", id);
    }

    @Override
    public void inativarPorId(Long id) {
        log.info("Inativando usuário ID: {}", id);

        Usuario usuario = buscarPorId(id);
        if (Boolean.TRUE.equals(usuario.getAtivo())) {
            usuario.setAtivo(false);
            usuarioRepository.save(usuario);
            log.info("Usuário ID {} inativado com sucesso", id);
        } else {
            log.debug("Usuário ID {} já está inativo", id);
        }
    }

    @Override
    public void ativarPorId(Long id) {
        log.info("Ativando usuário ID: {}", id);

        Usuario usuario = buscarPorId(id);
        if (Boolean.FALSE.equals(usuario.getAtivo())) {
            usuario.setAtivo(true);
            usuarioRepository.save(usuario);
            log.info("Usuário ID {} ativado com sucesso", id);
        } else {
            log.debug("Usuário ID {} já está ativo", id);
        }
    }


    private void validarRegrasDeAssociacao(UsuarioCadastroDto req) {
        if (req.getFuncao() == Funcao.RECEPCIONISTA && req.getIdAssociado() != null) {
            throw new BusinessRuleException("Recepcionista não pode estar associado a médico ou paciente.");
        }

        if ((req.getFuncao() == Funcao.MEDICO || req.getFuncao() == Funcao.PACIENTE) && req.getIdAssociado() == null) {
            throw new BusinessRuleException("Função " + req.getFuncao() + " exige ID de associação.");
        }
    }

    private void associarUsuarioAoMedicoOuPaciente(UsuarioCadastroDto req, Usuario usuario) {
        if (req.getFuncao() == Funcao.MEDICO) {
            Medico medico = medicoService.buscarPorId(req.getIdAssociado());


            if (medico.getUsuario() != null) {
                throw new BusinessRuleException("Este médico já está associado a um usuário.");
            }

            medico.setUsuario(usuario);
            medicoService.salvar(medico);

            log.info(">>> Médico salvo no banco de dados: {}", medico);
        }

        if (req.getFuncao() == Funcao.PACIENTE) {
            Paciente paciente = pacienteService.buscarPorId(req.getIdAssociado());

            if (paciente.getUsuario() != null) {
                throw new BusinessRuleException("Este paciente já está associado a um usuário.");
            }

            paciente.setUsuario(usuario);
            pacienteService.salvar(paciente);

            log.info(">>> Paciente salvo no banco de dados: {}", paciente);
        }
    }

    private void desassociarUsuarioDePessoa(Usuario usuario) {
        if (usuario.getFuncao() == Funcao.MEDICO) {
            Medico medico = medicoService.buscarPorUsuario(usuario);
            medico.setUsuario(null);
            medicoService.salvar(medico);
        }

        if (usuario.getFuncao() == Funcao.PACIENTE) {
            Paciente paciente = pacienteService.buscarPorUsuario(usuario);
            paciente.setUsuario(null);
            pacienteService.salvar(paciente);
        }
    }

    private void validarUsernameDuplicado(String username, Long idAtual) {
        boolean existe;

        if (idAtual == null) {
            existe = usuarioRepository.existsByUsername(username);
        } else {
            existe = usuarioRepository.existsByUsernameAndIdNot(username, idAtual);
        }

        if (existe) {
            throw new BusinessRuleException("Nome de usuário já está em uso: " + username);
        }
    }

}
