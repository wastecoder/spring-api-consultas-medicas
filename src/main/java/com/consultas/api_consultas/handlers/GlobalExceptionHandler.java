package com.consultas.api_consultas.handlers;

import com.consultas.api_consultas.dtos.respostas.ErrorResponse;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.exceptions.PasswordResetTokenInvalidoException;
import com.consultas.api_consultas.exceptions.RateLimitExcedidoException;
import com.consultas.api_consultas.exceptions.RefreshTokenInvalidoException;
import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import jakarta.persistence.EntityNotFoundException;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.transaction.TransactionSystemException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import java.time.OffsetDateTime;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Slf4j
@ControllerAdvice
public class GlobalExceptionHandler {

    // Ocorre quando uma exceção inesperada não tratada cai no sistema
    // Ex: Qualquer erro que não tenha um tratamento específico neste Handler
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleUncaughtException(final Exception exception) {
        var status = HttpStatus.INTERNAL_SERVER_ERROR;

        log.error("Erro inesperado não tratado", exception);

        ErrorResponse response = new ErrorResponse(
                "Ocorreu um erro inesperado. Por favor, tente novamente mais tarde.",
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando um ou mais campos da requisição falham na validação (@Valid)
    // Ex: um campo obrigatório está ausente ou um valor inválido foi enviado
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<ErrorResponse> handleValidationException(MethodArgumentNotValidException exception) {
        var status = HttpStatus.BAD_REQUEST;

        String mensagens = exception.getBindingResult()
                .getFieldErrors()
                .stream()
                .map(error -> error.getField() + ": " + error.getDefaultMessage())
                .collect(Collectors.joining("; "));

        log.warn("Erro de validação: {}", mensagens);

        ErrorResponse response = new ErrorResponse(
                mensagens,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando uma regra de negócio é violada explicitamente no código
    // Ex: tentativa de excluir um médico ainda ativo
    @ExceptionHandler(BusinessRuleException.class)
    public ResponseEntity<ErrorResponse> handleBusinessRule(final BusinessRuleException exception) {
        var status = HttpStatus.BAD_REQUEST;

        log.warn("Violação de regra de negócio: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                exception.getMessage(),
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando a entidade requisitada não existe no banco de dados
    // Ex: buscar um médico por ID que não está cadastrado
    @ExceptionHandler(EntityNotFoundException.class)
    public ResponseEntity<ErrorResponse> handleEntityNotFound(final EntityNotFoundException exception) {
        var status = HttpStatus.NOT_FOUND;

        log.warn("Recurso não encontrado: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                exception.getMessage(),
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando o tipo de dado enviado não é compatível com o esperado pelo endpoint
    // Ex: envio de uma string "abc" para um campo que espera um número, como Long ou Integer
    // Essa exceção é lançada apenas em parâmetros de rota ou query (PathVariable ou RequestParam), não em dados do corpo (RequestBody)
    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ResponseEntity<ErrorResponse> handleTypeMismatch(final MethodArgumentTypeMismatchException exception) {
        var status = HttpStatus.BAD_REQUEST;

        String campo = exception.getPropertyName();
        String tipoEsperado = exception.getRequiredType() != null ? exception.getRequiredType().getSimpleName() : "desconhecido";

        String mensagem = String.format("Valor inválido para o campo [%s]. Tipo esperado: [%s].", campo, tipoEsperado);

        log.warn("Tipo de dado incompatível: campo [{}], valor [{}], tipo esperado [{}]", campo, exception.getValue(), tipoEsperado);

        ErrorResponse response = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando é enviado um valor inválido para um campo enum
    // Ex: envio de "A" para o campo crmSigla, que espera um valor como "SP", "RJ", etc.
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<ErrorResponse> handleInvalidEnumValue(HttpMessageNotReadableException exception) {
        var status = HttpStatus.BAD_REQUEST;

        String mensagem = "Valor inválido fornecido para um dos campos. Verifique se os valores estão corretos.";

        if (exception.getCause() instanceof InvalidFormatException invalid) {
            var campo = invalid.getPath().get(0).getFieldName();
            mensagem = "Valor inválido para o campo [" + campo + "].";
        }

        log.warn("Enum inválido: {}", mensagem);

        ErrorResponse response = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando há violação de integridade no banco, como duplicidade em campos únicos
    // Ex: tentativa de cadastrar um médico com crmSigla e crmDigitos já existentes
    @ExceptionHandler(DataIntegrityViolationException.class)
    public ResponseEntity<ErrorResponse> handleDatabaseConstraintViolation(DataIntegrityViolationException exception) {
        var status = HttpStatus.CONFLICT;

        String mensagem = "Violação de dados únicos. Verifique se já existe um registro com os mesmos valores.";
        String causa = exception.getMostSpecificCause().getMessage();

        if (causa.contains("uk_medico_crm")) {
            mensagem = "Já existe um médico cadastrado com esse CRM (sigla e dígitos).";
        } else if (causa.contains("uk_medico_email")) {
            mensagem = "Já existe um médico cadastrado com esse e-mail.";
        } else if (causa.contains("uk_paciente_email")) {
            mensagem = "Já existe um paciente cadastrado com esse e-mail.";
        } else if (causa.contains("uk_paciente_cpf")) {
            mensagem = "Já existe um paciente cadastrado com esse CPF.";
        }

        log.warn("Constraint violada: {}", mensagem);

        ErrorResponse response = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando é lançado um IllegalArgumentException no código, geralmente por parâmetros inválidos em métodos de serviço
    // Ex: fornecimento de um mês fora do intervalo 1-12 em filtros de relatórios
    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<ErrorResponse> handleIllegalArgument(final IllegalArgumentException exception) {
        var status = HttpStatus.BAD_REQUEST;
        String mensagem = exception.getMessage();

        log.warn("Argumento inválido: {}", mensagem);

        ErrorResponse response = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando um usuário autenticado tenta acessar um recurso para o qual não possui permissão
    // Ex: paciente tentando editar os dados de outro paciente
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ErrorResponse> handleAccessDeniedException(AccessDeniedException exception) {
        var status = HttpStatus.FORBIDDEN;
        String mensagem = exception.getMessage();

        log.warn("Acesso negado: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando o cliente excede o limite de tentativas de login no /auth/login
    // Acompanhada do header Retry-After indicando em quantos segundos a janela renova
    @ExceptionHandler(RateLimitExcedidoException.class)
    public ResponseEntity<ErrorResponse> handleRateLimitExcedido(RateLimitExcedidoException exception) {
        var status = HttpStatus.TOO_MANY_REQUESTS;

        log.warn("Rate limit excedido: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                exception.getMessage(),
                status,
                OffsetDateTime.now()
        );

        return ResponseEntity
                .status(status)
                .header(HttpHeaders.RETRY_AFTER, String.valueOf(exception.getRetryAfterSegundos()))
                .body(response);
    }

    // Ocorre quando as credenciais para login fornecidas são inválidas
    // Ex: usuário ou senha incorretos no /auth/login
    @ExceptionHandler(BadCredentialsException.class)
    public ResponseEntity<ErrorResponse> handleBadCredentialsException(BadCredentialsException exception) {
        var status = HttpStatus.UNAUTHORIZED;
        String mensagem = exception.getMessage();

        log.warn("Falha na autenticação: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                mensagem,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando o usuário existe e a senha confere, mas a conta está inativa (UserDetails.isEnabled() = false)
    // Mensagem explícita para que o front saiba diferenciar de credenciais inválidas
    @ExceptionHandler(DisabledException.class)
    public ResponseEntity<ErrorResponse> handleDisabledException(DisabledException exception) {
        var status = HttpStatus.UNAUTHORIZED;

        log.warn("Tentativa de login de usuário inativo");

        ErrorResponse response = new ErrorResponse(
                "Usuário inativo. Contate o administrador.",
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando o token de redefinição enviado em /auth/reset-password é inválido, já usado ou expirou
    // A mensagem é deliberadamente genérica para não revelar o motivo exato (oracle de existência)
    @ExceptionHandler(PasswordResetTokenInvalidoException.class)
    public ResponseEntity<ErrorResponse> handlePasswordResetTokenInvalido(PasswordResetTokenInvalidoException exception) {
        var status = HttpStatus.UNAUTHORIZED;

        log.warn("Token de redefinição inválido: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                "Token de redefinição inválido ou expirado.",
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando o refresh token enviado em /auth/refresh está ausente do banco, expirado ou pertence a usuário inativo
    // O refresh é credencial de autenticação — falha aqui retorna 401, mesmo padrão do BadCredentialsException
    @ExceptionHandler(RefreshTokenInvalidoException.class)
    public ResponseEntity<ErrorResponse> handleRefreshTokenInvalido(RefreshTokenInvalidoException exception) {
        var status = HttpStatus.UNAUTHORIZED;

        log.warn("Refresh token inválido: {}", exception.getMessage());

        ErrorResponse response = new ErrorResponse(
                exception.getMessage(),
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando uma constraint do Bean Validation falha em parâmetros de método
    // Ex: @PathVariable ou @RequestParam anotados com @Min/@Max/@Pattern em controller marcado com @Validated
    // Diferente do MethodArgumentNotValidException, que cobre apenas @Valid em @RequestBody
    @ExceptionHandler(ConstraintViolationException.class)
    public ResponseEntity<ErrorResponse> handleConstraintViolation(ConstraintViolationException exception) {
        var status = HttpStatus.BAD_REQUEST;

        String mensagens = formatarViolacoes(exception.getConstraintViolations());

        log.warn("Violação de constraint em parâmetro: {}", mensagens);

        ErrorResponse response = new ErrorResponse(
                mensagens,
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Ocorre quando o commit da transação falha (wrapper do Spring para erros no flush/commit)
    // Causa mais comum: ConstraintViolationException disparada pelo Hibernate ao validar entidades no flush
    // Quando a causa raiz é validação, devolve 400 com os campos; caso contrário trata como erro de sistema (500)
    @ExceptionHandler(TransactionSystemException.class)
    public ResponseEntity<ErrorResponse> handleTransactionSystemException(TransactionSystemException exception) {
        Throwable causa = exception.getRootCause();

        if (causa instanceof ConstraintViolationException cve) {
            var status = HttpStatus.BAD_REQUEST;
            String mensagens = formatarViolacoes(cve.getConstraintViolations());

            log.warn("Violação de constraint detectada no commit da transação: {}", mensagens);

            ErrorResponse response = new ErrorResponse(
                    mensagens,
                    status,
                    OffsetDateTime.now()
            );

            return new ResponseEntity<>(response, status);
        }

        var status = HttpStatus.INTERNAL_SERVER_ERROR;

        log.error("Erro de sistema ao confirmar transação", exception);

        ErrorResponse response = new ErrorResponse(
                "Erro ao processar a transação. Por favor, tente novamente mais tarde.",
                status,
                OffsetDateTime.now()
        );

        return new ResponseEntity<>(response, status);
    }

    // Formata um conjunto de ConstraintViolation no padrão "campo: mensagem; campo: mensagem"
    // Reaproveitado por handleConstraintViolation e handleTransactionSystemException (causa de validação)
    private String formatarViolacoes(Set<ConstraintViolation<?>> violacoes) {
        if (violacoes == null || violacoes.isEmpty()) {
            return "Erro de validação.";
        }

        return violacoes.stream()
                .map(violacao -> nomeDoCampo(violacao) + ": " + violacao.getMessage())
                .collect(Collectors.joining("; "));
    }

    // Extrai o último nó do propertyPath, evitando o prefixo do método (ex: "buscarMedicoPorId.id" -> "id")
    private String nomeDoCampo(ConstraintViolation<?> violacao) {
        return StreamSupport.stream(violacao.getPropertyPath().spliterator(), false)
                .reduce((primeiro, ultimo) -> ultimo)
                .map(node -> node.getName())
                .orElse(violacao.getPropertyPath().toString());
    }

}
