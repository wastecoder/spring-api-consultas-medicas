package com.consultas.api_consultas.export.implementations;

import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RelatorioExportServiceImplTest {

    record Linha(Long id, String nome, BigDecimal valor) {}
    record Agregado(double perDia, double perSemana, double perMes) {}

    private final RelatorioExportServiceImpl service =
            new RelatorioExportServiceImpl(new CsvExporter(), new PdfExporter());

    @Test
    void csvDevolveContentTypeCorretoEAttachment() {
        ResponseEntity<byte[]> resp = service.exportarLista(
                List.of(new Linha(1L, "x", BigDecimal.ONE)),
                Linha.class,
                FormatoExportacao.CSV,
                ExportContext.of("Faturamento Mensal")
        );
        assertEquals(200, resp.getStatusCode().value());
        HttpHeaders h = resp.getHeaders();
        assertNotNull(h.getContentType());
        assertTrue(h.getContentType().toString().contains("text/csv"));
        String disp = h.getFirst(HttpHeaders.CONTENT_DISPOSITION);
        assertNotNull(disp);
        assertTrue(disp.contains("attachment"));
        assertTrue(disp.contains("faturamento-mensal-"));
        assertTrue(disp.endsWith(".csv\""));
    }

    @Test
    void pdfDevolveApplicationPdf() {
        ResponseEntity<byte[]> resp = service.exportarLista(
                List.of(new Linha(1L, "x", BigDecimal.ONE)),
                Linha.class,
                FormatoExportacao.PDF,
                ExportContext.of("Relatório X")
        );
        assertEquals("application/pdf", resp.getHeaders().getContentType().toString());
        assertTrue(resp.getHeaders().getFirst(HttpHeaders.CONTENT_DISPOSITION).endsWith(".pdf\""));
    }

    @Test
    void exportarObjetoFuncionaParaSingleDto() {
        ResponseEntity<byte[]> resp = service.exportarObjeto(
                new Agregado(1.0, 7.0, 30.0),
                FormatoExportacao.CSV,
                ExportContext.of("Média de Consultas")
        );
        String csv = new String(resp.getBody());
        assertTrue(csv.contains("Campo;Valor"));
        assertTrue(csv.contains("Por Dia;1,00"));
    }

    @Test
    void rejeitaJsonComoFormatoDeExportacao() {
        assertThrows(IllegalArgumentException.class, () ->
                service.exportarObjeto(new Agregado(1, 7, 30), FormatoExportacao.JSON,
                        ExportContext.of("X"))
        );
    }
}
