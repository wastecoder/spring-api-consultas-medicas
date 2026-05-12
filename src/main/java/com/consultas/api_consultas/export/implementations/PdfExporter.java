package com.consultas.api_consultas.export.implementations;

import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.TabelaDados;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfTemplate;
import com.lowagie.text.pdf.PdfWriter;
import org.springframework.stereotype.Component;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.time.format.DateTimeFormatter;

@Component
public class PdfExporter {

    private static final DateTimeFormatter TIMESTAMP_HUMANO = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss");
    private static final Color COR_HEADER = new Color(220, 220, 220);
    private static final Font FONTE_TITULO = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 14);
    private static final Font FONTE_META = FontFactory.getFont(FontFactory.HELVETICA, 9, Color.DARK_GRAY);
    private static final Font FONTE_HEADER_TABELA = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 10);
    private static final Font FONTE_CELULA = FontFactory.getFont(FontFactory.HELVETICA, 9);
    private static final Font FONTE_RODAPE = FontFactory.getFont(FontFactory.HELVETICA, 8, Color.DARK_GRAY);

    public byte[] exportar(TabelaDados tabela, ExportContext ctx) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        Document doc = new Document(PageSize.A4, 36, 36, 54, 54);
        try {
            PdfWriter writer = PdfWriter.getInstance(doc, out);
            writer.setPageEvent(new RodapeEvent());
            doc.open();

            // Cabeçalho: título + metadados
            Paragraph titulo = new Paragraph(ctx.tituloRelatorio(), FONTE_TITULO);
            titulo.setSpacingAfter(6);
            doc.add(titulo);

            Paragraph geradoEm = new Paragraph(
                    "Gerado em " + ctx.geradoEm().format(TIMESTAMP_HUMANO), FONTE_META);
            doc.add(geradoEm);

            for (var entry : ctx.parametrosOrdenados().entrySet()) {
                doc.add(new Paragraph(entry.getKey() + ": " + entry.getValue(), FONTE_META));
            }

            // Espaço antes da tabela
            doc.add(new Paragraph(" "));

            // Tabela
            PdfPTable pdfTable = new PdfPTable(tabela.headers().size());
            pdfTable.setWidthPercentage(100);
            pdfTable.setHeaderRows(1);

            for (String h : tabela.headers()) {
                PdfPCell cell = new PdfPCell(new Phrase(h, FONTE_HEADER_TABELA));
                cell.setBackgroundColor(COR_HEADER);
                cell.setPadding(5);
                pdfTable.addCell(cell);
            }

            if (tabela.linhas().isEmpty()) {
                PdfPCell vazio = new PdfPCell(new Phrase("Sem dados.", FONTE_CELULA));
                vazio.setColspan(tabela.headers().size());
                vazio.setHorizontalAlignment(Element.ALIGN_CENTER);
                vazio.setPadding(8);
                pdfTable.addCell(vazio);
            } else {
                for (var linha : tabela.linhas()) {
                    for (int i = 0; i < linha.size(); i++) {
                        PdfPCell cell = new PdfPCell(new Phrase(linha.get(i), FONTE_CELULA));
                        cell.setPadding(4);
                        if (tabela.colunasNumericas()[i]) {
                            cell.setHorizontalAlignment(Element.ALIGN_RIGHT);
                        }
                        pdfTable.addCell(cell);
                    }
                }
            }
            doc.add(pdfTable);
            doc.close();
        } catch (DocumentException e) {
            throw new IllegalStateException("Falha ao gerar PDF", e);
        }
        return out.toByteArray();
    }

    /** Escreve "API de Consultas Médicas — Página X de Y" centralizado no rodapé de cada página. */
    private static class RodapeEvent extends PdfPageEventHelper {
        private PdfTemplate totalPagesPlaceholder;
        private static final Font FONTE = FONTE_RODAPE;

        @Override
        public void onOpenDocument(PdfWriter writer, Document document) {
            totalPagesPlaceholder = writer.getDirectContent().createTemplate(30, 12);
        }

        @Override
        public void onEndPage(PdfWriter writer, Document document) {
            PdfContentByte cb = writer.getDirectContent();
            int paginaAtual = writer.getPageNumber();
            String esquerda = "API de Consultas Médicas";
            String meio = "Página " + paginaAtual + " de ";

            float y = document.bottom() - 20;
            float xCentro = (document.left() + document.right()) / 2f;

            cb.beginText();
            cb.setFontAndSize(FONTE.getBaseFont(), FONTE.getSize());
            cb.showTextAligned(Element.ALIGN_LEFT, esquerda, document.left(), y, 0);
            float meioWidth = FONTE.getBaseFont().getWidthPoint(meio, FONTE.getSize());
            cb.showTextAligned(Element.ALIGN_LEFT, meio, xCentro - (meioWidth / 2f), y, 0);
            cb.endText();

            cb.addTemplate(totalPagesPlaceholder, xCentro - (meioWidth / 2f) + meioWidth, y);
        }

        @Override
        public void onCloseDocument(PdfWriter writer, Document document) {
            totalPagesPlaceholder.beginText();
            totalPagesPlaceholder.setFontAndSize(FONTE.getBaseFont(), FONTE.getSize());
            totalPagesPlaceholder.showText(String.valueOf(writer.getPageNumber()));
            totalPagesPlaceholder.endText();
        }
    }
}
