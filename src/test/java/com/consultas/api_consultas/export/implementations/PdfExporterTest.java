package com.consultas.api_consultas.export.implementations;

import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.TabelaDados;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.parser.PdfTextExtractor;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PdfExporterTest {

    record Linha(Long id, String nome, BigDecimal valor) {}

    private final PdfExporter exporter = new PdfExporter();

    @Test
    void geraPdfComMagicHeader() {
        TabelaDados t = TabelaDados.deLista(
                List.of(new Linha(1L, "Dr. Souza", new BigDecimal("1500.00"))),
                Linha.class
        );
        byte[] bytes = exporter.exportar(t, ExportContext.of("Faturamento"));
        assertEquals('%', bytes[0]);
        assertEquals('P', bytes[1]);
        assertEquals('D', bytes[2]);
        assertEquals('F', bytes[3]);
    }

    @Test
    void pdfContemTituloEConteudo() throws IOException {
        TabelaDados t = TabelaDados.deLista(
                List.of(new Linha(1L, "Dr. Souza", new BigDecimal("1500.00"))),
                Linha.class
        );
        byte[] bytes = exporter.exportar(t, ExportContext.of("Faturamento Mensal"));
        String texto = extrairTexto(bytes);
        assertTrue(texto.contains("Faturamento Mensal"), "deve conter título");
        assertTrue(texto.contains("Dr. Souza"), "deve conter dado da tabela");
        assertTrue(texto.contains("Página"), "deve conter rodapé com 'Página'");
    }

    @Test
    void pdfMostraParametrosNoCabecalho() throws IOException {
        TabelaDados t = TabelaDados.deLista(List.of(new Linha(1L, "x", BigDecimal.ZERO)), Linha.class);
        ExportContext ctx = ExportContext.of("Relatório com Período",
                Map.of("Período", "01/01/2025 a 31/12/2025"));
        byte[] bytes = exporter.exportar(t, ctx);
        String texto = extrairTexto(bytes);
        assertTrue(texto.contains("Período: 01/01/2025 a 31/12/2025"));
    }

    @Test
    void pdfNaoVazioMesmoSemLinhas() {
        TabelaDados t = TabelaDados.deLista(List.<Linha>of(), Linha.class);
        byte[] bytes = exporter.exportar(t, ExportContext.of("Vazio"));
        assertNotEquals(0, bytes.length);
    }

    private String extrairTexto(byte[] bytes) throws IOException {
        PdfReader reader = new PdfReader(bytes);
        PdfTextExtractor ext = new PdfTextExtractor(reader);
        StringBuilder sb = new StringBuilder();
        for (int p = 1; p <= reader.getNumberOfPages(); p++) {
            sb.append(ext.getTextFromPage(p)).append('\n');
        }
        reader.close();
        return sb.toString();
    }
}
