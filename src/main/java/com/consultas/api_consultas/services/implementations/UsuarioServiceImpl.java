package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.UsuarioService;
import com.consultas.api_consultas.services.rules.UsuarioRules;
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
    private final PasswordEncoder passwordEncoder;
    private final UsuarioRules usuarioRules;


    @Override
    @Transactional
    public UsuarioResposta salvar(UsuarioCadastroDto requisicao) {
        log.info("Salvando novo usuário: {}", requisicao.getUsername());

        usuarioRules.validarUsernameDuplicado(requisicao.getUsername(), null);
        usuarioRules.validarRegrasDeAssociacao(requisicao);

        Usuario usuario = new Usuario();
        usuario.setUsername(requisicao.getUsername());
        usuario.setSenha(passwordEncoder.encode(requisicao.getSenha()));
        usuario.setFuncao(requisicao.getFuncao());
        usuario.setAtivo(true);

        usuario = usuarioRepository.save(usuario);

        usuarioRules.associarUsuarioAoMedicoOuPaciente(requisicao, usuario);

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

        usuarioRules.validarUsernameDuplicado(req.getUsername(), id);

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

        usuarioRules.desassociarUsuarioDePessoa(usuario);
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

}
