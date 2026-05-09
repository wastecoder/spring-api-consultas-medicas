package com.consultas.api_consultas.handlers;

import com.consultas.api_consultas.dtos.respostas.ErrorResponse;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import jakarta.validation.Path;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.TransactionSystemException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class GlobalExceptionHandlerTest {

    private GlobalExceptionHandler handler;

    @BeforeEach
    void setUp() {
        handler = new GlobalExceptionHandler();
    }


    @Test
    @DisplayName("ConstraintViolation: deve retornar 400 com mensagem formatada como 'campo: msg'")
    void deveRetornar400ComMensagemFormatadaParaConstraintViolation() {
        Set<ConstraintViolation<?>> violacoes = new LinkedHashSet<>();
        violacoes.add(violacao("buscarMedicoPorId.id", "deve ser maior ou igual a 1"));

        ConstraintViolationException exception = new ConstraintViolationException(violacoes);

        ResponseEntity<ErrorResponse> response = handler.handleConstraintViolation(exception);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(response.getBody()).isNotNull();
        assertThat(response.getBody().getHttpStatus()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(response.getBody().getMessage()).isEqualTo("id: deve ser maior ou igual a 1");
    }

    @Test
    @DisplayName("ConstraintViolation: deve concatenar múltiplas violações com '; '")
    void deveConcatenarMultiplasViolacoesComPontoEVirgula() {
        Set<ConstraintViolation<?>> violacoes = new LinkedHashSet<>();
        violacoes.add(violacao("listarMedicosComMaisConsultasNoMes.mes", "deve ser menor ou igual a 12"));
        violacoes.add(violacao("listarMedicosComMaisConsultasNoMes.ano", "deve ser maior ou igual a 2000"));

        ConstraintViolationException exception = new ConstraintViolationException(violacoes);

        ResponseEntity<ErrorResponse> response = handler.handleConstraintViolation(exception);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(response.getBody()).isNotNull();
        assertThat(response.getBody().getMessage())
                .contains("mes: deve ser menor ou igual a 12")
                .contains("ano: deve ser maior ou igual a 2000")
                .contains("; ");
    }

    @Test
    @DisplayName("TransactionSystemException: causa raiz ConstraintViolation deve retornar 400 com violações formatadas")
    void deveRetornar400QuandoCausaRaizForConstraintViolationException() {
        Set<ConstraintViolation<?>> violacoes = new LinkedHashSet<>();
        violacoes.add(violacao("medico.crm", "não pode ser nulo"));

        ConstraintViolationException causa = new ConstraintViolationException(violacoes);
        TransactionSystemException tse = new TransactionSystemException(
                "Could not commit JPA transaction",
                causa
        );

        ResponseEntity<ErrorResponse> response = handler.handleTransactionSystemException(tse);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(response.getBody()).isNotNull();
        assertThat(response.getBody().getHttpStatus()).isEqualTo(HttpStatus.BAD_REQUEST);
        assertThat(response.getBody().getMessage()).isEqualTo("crm: não pode ser nulo");
    }

    @Test
    @DisplayName("TransactionSystemException: causa raiz não-validação deve retornar 500 genérico")
    void deveRetornar500QuandoCausaRaizNaoForConstraintViolation() {
        TransactionSystemException tse = new TransactionSystemException(
                "Could not commit JPA transaction",
                new RuntimeException("lock timeout no banco")
        );

        ResponseEntity<ErrorResponse> response = handler.handleTransactionSystemException(tse);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
        assertThat(response.getBody()).isNotNull();
        assertThat(response.getBody().getHttpStatus()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
        assertThat(response.getBody().getMessage())
                .isEqualTo("Erro ao processar a transação. Por favor, tente novamente mais tarde.");
    }


    // Mock de ConstraintViolation com propertyPath formado por nós (último é o nome do campo)
    private ConstraintViolation<?> violacao(String propertyPath, String mensagem) {
        Path path = pathDe(propertyPath);
        ConstraintViolation<?> violacao = mock(ConstraintViolation.class);
        when(violacao.getMessage()).thenReturn(mensagem);
        when(violacao.getPropertyPath()).thenReturn(path);
        return violacao;
    }

    private Path pathDe(String propertyPath) {
        String[] partes = propertyPath.split("\\.");
        List<Path.Node> nodes = new ArrayList<>(partes.length);
        for (String parte : partes) {
            nodes.add(node(parte));
        }
        Path path = mock(Path.class);
        when(path.spliterator()).thenAnswer(invocation -> nodes.spliterator());
        return path;
    }

    private Path.Node node(String nome) {
        Path.Node node = mock(Path.Node.class);
        when(node.getName()).thenReturn(nome);
        return node;
    }
}
