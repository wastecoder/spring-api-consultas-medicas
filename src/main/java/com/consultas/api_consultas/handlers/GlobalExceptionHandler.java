package com.consultas.api_consultas.handlers;

import com.consultas.api_consultas.dtos.respostas.ErrorResponse;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import jakarta.persistence.EntityNotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
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

    // Ocorre quando a entidade requisitada não existe no banco de dados
    // Ex: buscar um médico por ID que não está cadastrado
    @ExceptionHandler(EntityNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleEntityNotFound(final EntityNotFoundException ex) {
        var status = HttpStatus.NOT_FOUND;

        log.warn("Recurso não encontrado: {}", ex.getMessage());

        ErrorResponse exception = new ErrorResponse(
                ex.getMessage(),
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(exception, status);
    }

    // Ocorre quando é enviado um valor inválido para um campo enum
    // Ex: envio de "A" para o campo crmSigla, que espera um valor como "SP", "RJ", etc.
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<ErrorResponse> handleInvalidEnumValue(HttpMessageNotReadableException ex) {
        var status = HttpStatus.BAD_REQUEST;

        String mensagem = "Valor inválido fornecido para um dos campos. Verifique se os valores estão corretos.";

        if (ex.getCause() instanceof InvalidFormatException invalid) {
            var campo = invalid.getPath().get(0).getFieldName();
            mensagem = "Valor inválido para o campo [" + campo + "].";
        }

        log.warn("Enum inválido: {}", mensagem);

        ErrorResponse exception = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(exception, status);
    }

    // Ocorre quando há violação de integridade no banco, como duplicidade em campos únicos
    // Ex: tentativa de cadastrar um médico com crmSigla e crmDigitos já existentes
    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ErrorResponse> handleDatabaseConstraintViolation(DataIntegrityViolationException ex) {
        var status = HttpStatus.CONFLICT;

        String mensagem = "Violação de dados únicos. Verifique se já existe um registro com os mesmos valores.";

        if (ex.getMostSpecificCause().getMessage().contains("uk_medico_crm")) {
            mensagem = "Já existe um médico cadastrado com esse CRM (sigla e dígitos).";
        }

        log.warn("Constraint violada: {}", mensagem);

        ErrorResponse exception = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(exception, status);
    }

}
