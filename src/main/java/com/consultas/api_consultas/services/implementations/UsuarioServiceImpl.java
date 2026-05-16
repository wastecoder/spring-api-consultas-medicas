package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.mappers.UsuarioMapper;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.repositories.specifications.UsuarioSpecifications;
import com.consultas.api_consultas.services.UsuarioService;
import com.consultas.api_consultas.services.rules.UsuarioRules;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class UsuarioServiceImpl implements UsuarioService {

    private final UsuarioRepository usuarioRepository;
    private final PasswordEncoder passwordEncoder;
    private final UsuarioRules usuarioRules;
    private final UsuarioMapper usuarioMapper;


    @Override
    @Transactional
    public UsuarioResposta salvar(UsuarioCadastroDto requisicao) {
        log.info("Salvando novo usuário: {}", requisicao.getUsername());

        usuarioRules.validarUsernameDuplicado(requisicao.getUsername(), null);
        usuarioRules.validarEmailDuplicado(requisicao.getEmail(), null);
        usuarioRules.validarRegrasDeAssociacao(requisicao);

        Usuario usuario = usuarioMapper.paraEntidade(requisicao);
        usuario.setSenha(passwordEncoder.encode(requisicao.getSenha()));
        usuario.setAtivo(true);

        usuario = usuarioRepository.save(usuario);

        usuarioRules.associarUsuarioAoMedicoOuPaciente(requisicao, usuario);

        return usuarioMapper.paraResposta(usuario);
    }

    private static final Map<String, String[]> CAMPOS_ORDENACAO = Map.of(
            "username", new String[] { "username" },
            "email",    new String[] { "email" },
            "funcao",   new String[] { "funcao" }
    );

    @Override
    public PageResponse<UsuarioResposta> buscarUsuarios(
            int pagina,
            int tamanho,
            String username,
            Funcao funcao,
            Boolean ativo,
            String ordenarPor,
            String direcao
    ) {
        log.info("Buscando usuários - Página: {}, Tamanho: {}, Username: {}, Funcao: {}, Ativo: {}, OrdenarPor: {}, Direção: {}",
                pagina, tamanho, username, funcao, ativo, ordenarPor, direcao);

        Sort sort = construirOrdenacao(ordenarPor, direcao);
        Pageable pageable = PageRequest.of(pagina, tamanho, sort);
        Specification<Usuario> spec = Specification
                .where(UsuarioSpecifications.comAtivo(ativo))
                .and(UsuarioSpecifications.comUsernameContendo(username))
                .and(UsuarioSpecifications.comFuncao(funcao));

        return PageResponse.from(usuarioRepository.findAll(spec, pageable).map(usuarioMapper::paraResposta));
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

    @Override
    public Usuario buscarPorId(Long id) {
        log.info("Buscando usuário por ID: {}", id);

        return usuarioRepository.findById(id)
                .orElseThrow(() -> {
                    String mensagem = String.format("Usuário com ID [%d] não encontrado", id);
                    log.warn(mensagem);
                    return new EntityNotFoundException(mensagem);
                });
    }

    @Override
    public Usuario atualizar(Long id, UsuarioAtualizacaoDto req) {
        log.info("Atualizando usuário ID: {}", id);

        Usuario existente = buscarPorId(id);

        usuarioRules.validarUsernameDuplicado(req.getUsername(), id);
        usuarioRules.validarEmailDuplicado(req.getEmail(), id);

        usuarioMapper.aplicarAtualizacao(req, existente);

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
