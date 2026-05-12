package com.consultas.api_consultas.export.implementations;

import com.consultas.api_consultas.enums.FormatoExportacao;
import com.consultas.api_consultas.export.ExportContext;
import com.consultas.api_consultas.export.RelatorioExportService;
import com.consultas.api_consultas.export.TabelaDados;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ContentDisposition;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RelatorioExportServiceImpl implements RelatorioExportService {

    private final CsvExporter csvExporter;
    private final PdfExporter pdfExporter;

    @Override
    public ResponseEntity<byte[]> exportarLista(List<?> dados, Class<?> tipoDto,
                                                FormatoExportacao formato, ExportContext ctx) {
        TabelaDados tabela = TabelaDados.deLista(dados, tipoDto);
        return montarResposta(tabela, formato, ctx);
    }

    @Override
    public ResponseEntity<byte[]> exportarObjeto(Object dto, FormatoExportacao formato, ExportContext ctx) {
        TabelaDados tabela = TabelaDados.deObjeto(dto);
        return montarResposta(tabela, formato, ctx);
    }

    private ResponseEntity<byte[]> montarResposta(TabelaDados tabela, FormatoExportacao formato, ExportContext ctx) {
        byte[] corpo = switch (formato) {
            case CSV -> csvExporter.exportar(tabela);
            case PDF -> pdfExporter.exportar(tabela, ctx);
            case JSON -> throw new IllegalArgumentException(
                    "Formato JSON não é tratado pelo RelatorioExportService — controller deve devolver o DTO direto.");
        };

        String nomeArquivo = ctx.slugArquivo() + "." + formato.extensao();
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.parseMediaType(formato.contentType()));
        headers.setContentDisposition(ContentDisposition.attachment().filename(nomeArquivo).build());
        headers.setContentLength(corpo.length);
        return new ResponseEntity<>(corpo, headers, 200);
    }
}
