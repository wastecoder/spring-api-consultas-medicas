package com.consultas.api_consultas.dtos.requisicoes;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record ResetPasswordDto(
        @NotBlank(message = "Token é obrigatório")
        @Size(min = 36, max = 36, message = "Token inválido")
        String token,

        @NotBlank(message = "Nova senha é obrigatória")
        @Size(min = 5, max = 100, message = "Nova senha deve ter entre 5 e 100 caracteres")
        String novaSenha
) {}
