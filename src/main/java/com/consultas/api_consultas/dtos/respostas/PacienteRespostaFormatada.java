package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.utils.FormatoUtils;
import lombok.Getter;

@Getter
public class PacienteRespostaFormatada {

    private Long id;

    private String nome;

    private String email;

    private String cpf;

    private String dataNascimento;

    private String telefone;

    private String ativo;


    public PacienteRespostaFormatada(Paciente paciente) {
        this.id = paciente.getId();
        this.nome = paciente.getNome();
        this.email = paciente.getEmail();
        this.cpf = FormatoUtils.formatarCpf(paciente.getCpf());
        this.dataNascimento = FormatoUtils.formatarData(paciente.getDataNascimento());
        this.telefone = FormatoUtils.formatarTelefone(paciente.getTelefone());
        this.ativo = FormatoUtils.formatarStatusAtivo(paciente.getAtivo());
    }

}
