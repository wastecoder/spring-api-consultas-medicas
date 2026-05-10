package com.consultas.api_consultas.dtos.requisicoes;

import jakarta.validation.constraints.NotBlank;

public record RefreshTokenRequestDTO(
        @NotBlank(message = "Refresh token é obrigatório")
        String refreshToken
) {}
