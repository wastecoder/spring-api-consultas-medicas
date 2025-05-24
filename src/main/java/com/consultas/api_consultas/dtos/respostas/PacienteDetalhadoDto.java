package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Paciente;
import lombok.Getter;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

@Getter
public class PacienteDetalhadoDto {

    private Long id;

    private String nome;

    private String email;

    private String cpf;

    private String dataNascimento;

    private String telefone;

    private Boolean ativo;

    public PacienteDetalhadoDto(Paciente paciente) {
        this.id = paciente.getId();
        this.nome = paciente.getNome();
        this.email = paciente.getEmail();
        this.cpf = formatarCpf(paciente.getCpf());
        this.dataNascimento = formatarData(paciente.getDataNascimento());
        this.telefone = formatarTelefone(paciente.getTelefone());
        this.ativo = paciente.getAtivo();
    }

    private String formatarCpf(String cpf) {
        return String.format("%s.%s.%s-%s",
                cpf.substring(0, 3),
                cpf.substring(3, 6),
                cpf.substring(6, 9),
                cpf.substring(9, 11));
    }

    private String formatarData(LocalDate data) {
        return data.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"));
    }

    private String formatarTelefone(String telefone) {
        if (telefone.length() == 11) {
            return String.format("(%s) %s-%s", telefone.substring(0, 2), telefone.substring(2, 7), telefone.substring(7));
        } else if (telefone.length() == 10) {
            return String.format("(%s) %s-%s", telefone.substring(0, 2), telefone.substring(2, 6), telefone.substring(6));
        } else {
            return telefone;
        }
    }

}
