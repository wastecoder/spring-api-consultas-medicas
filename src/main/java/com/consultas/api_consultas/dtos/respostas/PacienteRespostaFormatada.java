package com.consultas.api_consultas.dtos.respostas;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PacienteRespostaFormatada {

    private Long id;

    private String nome;

    private String email;

    private String cpf;

    private String sexo;

    private String dataNascimento;

    private String telefone;

    private String ativo;

}
