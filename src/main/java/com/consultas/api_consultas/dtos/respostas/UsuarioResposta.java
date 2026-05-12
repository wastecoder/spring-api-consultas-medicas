package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.enums.Funcao;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class UsuarioResposta {

    private Long id;

    private String username;

    private String email;

    private Funcao funcao;

    private boolean ativo;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AuditoriaResposta auditoria;

}
