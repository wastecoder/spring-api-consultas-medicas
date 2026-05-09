package com.consultas.api_consultas.dtos.requisicoes;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record LoginDTO(
        @NotBlank(message = "Username é obrigatório")
        @Size(min = 5, max = 30, message = "Username deve ter entre 5 e 30 caracteres")
        String username,

        @NotBlank(message = "Senha é obrigatória")
        @Size(min = 5, max = 100, message = "Senha deve ter entre 5 e 100 caracteres")
        String senha
) {}
