package com.consultas.api_consultas.dtos.requisicoes;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record ForgotPasswordDto(
        @NotBlank(message = "Email é obrigatório")
        @Email(message = "Email inválido")
        @Size(max = 100, message = "Email deve ter no máximo 100 caracteres")
        String email
) {}
