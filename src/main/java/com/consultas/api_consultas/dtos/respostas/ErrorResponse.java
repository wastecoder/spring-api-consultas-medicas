package com.consultas.api_consultas.dtos.respostas;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import org.springframework.http.HttpStatus;

import java.time.OffsetDateTime;

@Getter
public class ErrorResponse {
    private final String message;

    private final HttpStatus httpStatus;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ssXXX")
    private final OffsetDateTime timestamp;

    public ErrorResponse(String message, HttpStatus httpStatus, OffsetDateTime timestamp) {
        this.message = message;
        this.httpStatus = httpStatus;
        this.timestamp = timestamp;
    }
}
