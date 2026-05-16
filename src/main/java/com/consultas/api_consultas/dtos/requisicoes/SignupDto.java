package com.consultas.api_consultas.dtos.requisicoes;

import com.consultas.api_consultas.enums.Sexo;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PastOrPresent;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

import java.time.LocalDate;

// Cadastro self-service de paciente: cria Usuario(funcao=PACIENTE) + Paciente
// numa unica chamada publica e devolve LoginResponse pronto para auto-login.
// Limites herdados de LoginDTO (username/senha) e PacienteRequisicao (dados pessoais);
// email respeita o limite de 50 chars da entidade Paciente (Pessoa.email).
public record SignupDto(

        @NotBlank(message = "Nome é obrigatório")
        @Size(min = 3, max = 100, message = "Nome deve ter entre 3 a 100 caracteres")
        String nome,

        @NotBlank(message = "CPF é obrigatório")
        @Size(min = 11, max = 11, message = "CPF deve ter 11 números")
        @Pattern(regexp = "\\d{11}", message = "CPF deve conter apenas números com 11 dígitos")
        String cpf,

        @NotNull(message = "Sexo é obrigatório")
        Sexo sexo,

        @NotNull(message = "Data de nascimento é obrigatória")
        @PastOrPresent(message = "Data de nascimento não pode ser no futuro")
        @JsonFormat(pattern = "yyyy-MM-dd")
        LocalDate dataNascimento,

        @NotBlank(message = "Telefone é obrigatório")
        @Size(min = 10, max = 11, message = "Telefone deve ter entre 10 a 11 números")
        @Pattern(regexp = "\\d{10,11}", message = "Telefone deve conter apenas números com 10 ou 11 dígitos")
        String telefone,

        @NotBlank(message = "E-mail é obrigatório")
        @Email(message = "E-mail deve ser válido")
        @Size(min = 5, max = 50, message = "E-mail deve ter entre 5 a 50 caracteres")
        String email,

        @NotBlank(message = "Usuário não pode ser vazio")
        @Size(min = 5, max = 30, message = "Usuário deve ter entre 5 a 30 caracteres")
        String username,

        @NotBlank(message = "Senha não pode ser vazia")
        @Size(min = 5, max = 100, message = "Senha deve ter entre 5 e 100 caracteres")
        String senha

) {}
