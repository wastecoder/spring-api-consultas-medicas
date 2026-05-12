package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;

import java.time.LocalDate;

@Getter
public class PacienteResposta {

    private Long id;

    private String nome;

    private String email;

    private String cpf;

    private Sexo sexo;

    private LocalDate dataNascimento;

    private String telefone;

    private Boolean ativo;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AuditoriaResposta auditoria;


    public PacienteResposta(Paciente paciente) {
        this.id = paciente.getId();
        this.nome = paciente.getNome();
        this.email = paciente.getEmail();
        this.cpf = paciente.getCpf();
        this.sexo = paciente.getSexo();
        this.dataNascimento = paciente.getDataNascimento();
        this.telefone = paciente.getTelefone();
        this.ativo = paciente.getAtivo();
    }

    public static PacienteResposta entidadeParaDtoComAuditoria(Paciente paciente) {
        PacienteResposta dto = new PacienteResposta(paciente);
        dto.auditoria = AuditoriaResposta.de(paciente);
        return dto;
    }

}
