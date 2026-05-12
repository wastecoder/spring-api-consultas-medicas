package com.consultas.api_consultas.export;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class TabelaDadosTest {

    record DtoExemplo(Long idMedico, String nome, BigDecimal totalFaturado) {}

    @Test
    void deListaTraduzHeadersConhecidos() {
        TabelaDados t = TabelaDados.deLista(
                List.of(new DtoExemplo(1L, "Dr. Souza", new BigDecimal("1500.00"))),
                DtoExemplo.class
        );
        assertEquals(List.of("ID Médico", "Nome", "Total Faturado"), t.headers());
    }

    @Test
    void deListaUsaCamelCaseQuandoSemTraducao() {
        record DtoX(String campoExoticoCamelCase) {}
        TabelaDados t = TabelaDados.deLista(List.of(new DtoX("v")), DtoX.class);
        assertEquals("Campo Exotico Camel Case", t.headers().get(0));
    }

    @Test
    void deListaFormataValoresPtBr() {
        TabelaDados t = TabelaDados.deLista(
                List.of(new DtoExemplo(1L, "Dr. Souza", new BigDecimal("1500.00"))),
                DtoExemplo.class
        );
        List<String> linha = t.linhas().get(0);
        assertEquals("1", linha.get(0));
        assertEquals("Dr. Souza", linha.get(1));
        assertEquals("1.500,00", linha.get(2));
    }

    @Test
    void deListaMarcaColunasNumericas() {
        TabelaDados t = TabelaDados.deLista(
                List.of(new DtoExemplo(1L, "x", BigDecimal.ZERO)),
                DtoExemplo.class
        );
        assertTrue(t.colunasNumericas()[0]);   // Long
        assertEquals(false, t.colunasNumericas()[1]); // String
        assertTrue(t.colunasNumericas()[2]);   // BigDecimal
    }

    @Test
    void deObjetoProduzTabelaChaveValor() {
        TabelaDados t = TabelaDados.deObjeto(new DtoExemplo(42L, "Dra. Lima", new BigDecimal("250.5")));
        assertEquals(List.of("Campo", "Valor"), t.headers());
        assertEquals(3, t.linhas().size());
        assertEquals(List.of("ID Médico", "42"), t.linhas().get(0));
        assertEquals(List.of("Nome", "Dra. Lima"), t.linhas().get(1));
        assertEquals(List.of("Total Faturado", "250,50"), t.linhas().get(2));
    }

    @Test
    void rejeitaTipoNaoRecord() {
        assertThrows(IllegalArgumentException.class,
                () -> TabelaDados.deLista(List.of("x"), String.class));
    }
}
