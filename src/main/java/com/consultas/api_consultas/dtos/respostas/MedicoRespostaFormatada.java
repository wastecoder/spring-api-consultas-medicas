package com.consultas.api_consultas.dtos.respostas;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class MedicoRespostaFormatada {

    private Long id;

    private String nome;

    private String email;

    private String crm;

    private String especialidade;

    private String telefone;

    private String ativo;

}
