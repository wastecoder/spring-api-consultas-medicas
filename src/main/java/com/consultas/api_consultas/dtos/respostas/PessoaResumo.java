package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Pessoa;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class PessoaResumo {

    private final Long id;
    private final String nome;

    public PessoaResumo(Pessoa pessoa) {
        this(pessoa.getId(), pessoa.getNome());
    }

}
