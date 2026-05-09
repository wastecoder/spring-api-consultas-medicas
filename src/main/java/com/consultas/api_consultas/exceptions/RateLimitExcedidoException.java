package com.consultas.api_consultas.exceptions;

import lombok.Getter;

@Getter
public class RateLimitExcedidoException extends RuntimeException {

    private final long retryAfterSegundos;

    public RateLimitExcedidoException(String message, long retryAfterSegundos) {
        super(message);
        this.retryAfterSegundos = retryAfterSegundos;
    }
}
