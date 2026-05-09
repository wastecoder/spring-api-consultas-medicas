package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;

public interface UsuarioService {

    UsuarioResposta salvar(UsuarioCadastroDto requisicao);

    PageResponse<UsuarioResposta> buscarTodos(int pagina, int tamanho);

    Usuario buscarPorId(Long id);

    Usuario atualizar(Long id, UsuarioAtualizacaoDto req);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

}
