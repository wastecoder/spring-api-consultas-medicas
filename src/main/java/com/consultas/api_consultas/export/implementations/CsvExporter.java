package com.consultas.api_consultas.export.implementations;

import com.consultas.api_consultas.export.TabelaDados;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UncheckedIOException;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

@Component
public class CsvExporter {

    // BOM UTF-8: sem isso o Excel no Windows abre "ç/ã" como mojibake.
    private static final byte[] BOM_UTF8 = { (byte) 0xEF, (byte) 0xBB, (byte) 0xBF };

    private static final CSVFormat FORMATO = CSVFormat.EXCEL.builder()
            .setDelimiter(';')
            .build();

    public byte[] exportar(TabelaDados tabela) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try {
            out.write(BOM_UTF8);
            try (OutputStreamWriter w = new OutputStreamWriter(out, StandardCharsets.UTF_8);
                 CSVPrinter printer = new CSVPrinter(w, FORMATO)) {
                printer.printRecord(tabela.headers());
                for (var linha : tabela.linhas()) {
                    printer.printRecord(linha);
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException("Falha ao gerar CSV", e);
        }
        return out.toByteArray();
    }
}
