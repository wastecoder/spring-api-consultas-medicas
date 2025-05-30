package com.consultas.api_consultas.dtos.requisicoes;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Getter;

import java.time.LocalDate;

@Getter
public class PacienteRequisicao {

    @NotBlank(message = "Nome é obrigatório")
    @Size(min = 3, max = 100, message = "Nome deve ter entre 3 a 100 caracteres")
    private String nome;

    @NotBlank(message = "E-mail é obrigatório")
    @Size(min = 5, max = 50, message = "E-mail deve ter entre 5 a 50 caracteres")
    @Pattern(
            regexp = "^[\\w._%+-]+@[\\w.-]+\\.[a-zA-Z]{2,}$",
            message = "E-mail deve ser válido"
    )
    private String email;

    @NotBlank(message = "CPF é obrigatório")
    @Size(min = 11, max = 11, message = "CPF deve ter 11 números")
    @Pattern(
            regexp = "\\d{11}",
            message = "CPF deve conter apenas números com 11 dígitos"
    )
    private String cpf;

    @NotNull(message = "Sexo é obrigatório")
    private Sexo sexo;

    @NotNull(message = "Data de nascimento é obrigatória")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate dataNascimento;

    @NotBlank(message = "Telefone é obrigatório")
    @Size(min = 10, max = 11, message = "Telefone deve ter entre 10 a 11 números")
    @Pattern(
            regexp = "\\d{10,11}",
            message = "Telefone deve conter apenas números com 10 ou 11 dígitos"
    )
    private String telefone;

    public Paciente dtoParaPaciente() {
        Paciente paciente = new Paciente();
        paciente.setNome(this.getNome());
        paciente.setEmail(this.getEmail());
        paciente.setCpf(this.getCpf());
        paciente.setSexo(this.getSexo());
        paciente.setDataNascimento(this.getDataNascimento());
        paciente.setTelefone(this.getTelefone());
        return paciente;
    }

}
