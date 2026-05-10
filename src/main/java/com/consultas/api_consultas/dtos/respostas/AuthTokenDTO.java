package com.consultas.api_consultas.dtos.respostas;

public record AuthTokenDTO(
        String accessToken,
        String refreshToken,
        long expiresIn,
        String tokenType
) {}
