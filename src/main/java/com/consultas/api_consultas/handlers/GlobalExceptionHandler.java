package com.consultas.api_consultas.handlers;

import com.consultas.api_consultas.dtos.respostas.ErrorResponse;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.time.OffsetDateTime;
import java.util.stream.Collectors;

@Slf4j
@ControllerAdvice
public class GlobalExceptionHandler {

    // Ocorre quando uma exceção inesperada não tratada cai no sistema
    // Ex: Qualquer erro que não tenha um tratamento específico neste Handler
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleUncaughtException(final Exception ex) {
        var status = HttpStatus.INTERNAL_SERVER_ERROR;

        log.error("Erro inesperado não tratado", ex);

        ErrorResponse exception = new ErrorResponse(
                "Ocorreu um erro inesperado. Por favor, tente novamente mais tarde.",
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(exception, status);
    }

    // Ocorre quando um ou mais campos da requisição falham na validação (@Valid)
    // Ex: um campo obrigatório está ausente ou um valor inválido foi enviado
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidationException(MethodArgumentNotValidException ex) {
        var status = HttpStatus.BAD_REQUEST;

        String mensagens = ex.getBindingResult()
                .getFieldErrors()
                .stream()
                .map(error -> error.getField() + ": " + error.getDefaultMessage())
                .collect(Collectors.joining("; "));

        log.warn("Erro de validação: {}", mensagens);

        ErrorResponse exception = new ErrorResponse(
                mensagens,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(exception, status);
    }

    // Ocorre quando uma regra de negócio é violada explicitamente no código
    // Ex: tentativa de excluir um médico ainda ativo
    @ExceptionHandler(BusinessRuleException.class)
    public ResponseEntity<ErrorResponse> handleBusinessRule(final BusinessRuleException ex) {
        var status = HttpStatus.BAD_REQUEST;

        log.warn("Violação de regra de negócio: {}", ex.getMessage());

        ErrorResponse exception = new ErrorResponse(
                ex.getMessage(),
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(exception, status);
    }

}
