package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Pessoa;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class PessoaResumo {

    private Long id;
    private String nome;

    public PessoaResumo(Pessoa pessoa) {
        this(pessoa.getId(), pessoa.getNome());
    }

    // Usado pelo CSV/PDF do export — Jackson continua serializando id+nome via getters.
    @Override
    public String toString() {
        return nome;
    }

}
