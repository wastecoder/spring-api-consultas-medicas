package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;

@Getter
public class UsuarioResposta {

    private final Long id;

    private final String username;

    private final String email;

    private final Funcao funcao;

    private final boolean ativo;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AuditoriaResposta auditoria;

    public UsuarioResposta(Usuario usuario) {
        this.id = usuario.getId();
        this.username = usuario.getUsername();
        this.email = usuario.getEmail();
        this.funcao = usuario.getFuncao();
        this.ativo = usuario.getAtivo();
    }

    public static UsuarioResposta entidadeParaDtoComAuditoria(Usuario usuario) {
        UsuarioResposta dto = new UsuarioResposta(usuario);
        dto.auditoria = AuditoriaResposta.de(usuario);
        return dto;
    }

}
