package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Paciente;
import lombok.Getter;

import java.time.LocalDate;

@Getter
public class PacienteResposta {

    private Long id;

    private String nome;

    private String email;

    private String cpf;

    private LocalDate dataNascimento;

    private String telefone;

    private Boolean ativo;


    public PacienteResposta(Paciente paciente) {
        this.id = paciente.getId();
        this.nome = paciente.getNome();
        this.email = paciente.getEmail();
        this.cpf = paciente.getEmail();
        this.dataNascimento = paciente.getDataNascimento();
        this.telefone = paciente.getTelefone();
        this.ativo = paciente.getAtivo();
    }

}
