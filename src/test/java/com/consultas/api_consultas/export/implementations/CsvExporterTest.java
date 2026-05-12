package com.consultas.api_consultas.export.implementations;

import com.consultas.api_consultas.export.TabelaDados;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CsvExporterTest {

    record Linha(Long id, String nome, BigDecimal valor) {}

    private final CsvExporter exporter = new CsvExporter();

    @Test
    void csvComecaComBomUtf8() {
        TabelaDados t = TabelaDados.deLista(
                List.of(new Linha(1L, "x", BigDecimal.ONE)),
                Linha.class
        );
        byte[] bytes = exporter.exportar(t);
        assertEquals((byte) 0xEF, bytes[0]);
        assertEquals((byte) 0xBB, bytes[1]);
        assertEquals((byte) 0xBF, bytes[2]);
    }

    @Test
    void csvUsaPontoEVirgulaComoSeparador() {
        TabelaDados t = TabelaDados.deLista(
                List.of(new Linha(1L, "Dr. Souza", new BigDecimal("1500.00"))),
                Linha.class
        );
        String csv = new String(exporter.exportar(t), StandardCharsets.UTF_8);
        assertTrue(csv.contains("ID;Nome;"));
        assertTrue(csv.contains("1;Dr. Souza;1.500,00"));
    }

    @Test
    void preservaAcentosComBom() {
        record Acento(String descricao) {}
        TabelaDados t = TabelaDados.deLista(
                List.of(new Acento("avaliação médica")),
                Acento.class
        );
        String csv = new String(exporter.exportar(t), StandardCharsets.UTF_8);
        assertTrue(csv.contains("avaliação médica"));
    }
}
