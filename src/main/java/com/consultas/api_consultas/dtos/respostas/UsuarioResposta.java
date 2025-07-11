package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import lombok.Getter;

@Getter
public class UsuarioResposta {

    private final Long id;

    private final String username;

    private final Funcao funcao;

    private final boolean ativo;

    public UsuarioResposta(Usuario usuario) {
        this.id = usuario.getId();
        this.username = usuario.getUsername();
        this.funcao = usuario.getFuncao();
        this.ativo = usuario.getAtivo();
    }

}
