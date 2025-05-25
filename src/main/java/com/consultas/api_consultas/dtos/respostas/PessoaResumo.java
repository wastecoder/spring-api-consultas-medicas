package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Pessoa;
import lombok.Getter;

@Getter
public class PessoaResumo {

    private Long id;
    private String nome;

    public PessoaResumo(Pessoa medico) {
        this.id = medico.getId();
        this.nome = medico.getNome();
    }

}
