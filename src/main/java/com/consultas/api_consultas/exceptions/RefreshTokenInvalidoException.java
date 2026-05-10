package com.consultas.api_consultas.exceptions;

public class RefreshTokenInvalidoException extends RuntimeException {
    public RefreshTokenInvalidoException(String message) {
        super(message);
    }
}
