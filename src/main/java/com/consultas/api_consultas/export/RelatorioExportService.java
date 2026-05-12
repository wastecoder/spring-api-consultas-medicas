package com.consultas.api_consultas.export;

import com.consultas.api_consultas.enums.FormatoExportacao;
import org.springframework.http.ResponseEntity;

import java.util.List;

/**
 * Serviço responsável por converter resultado de relatório em arquivo (CSV/PDF).
 * Os controllers chamam estes métodos só quando o formato pedido é diferente de JSON.
 */
public interface RelatorioExportService {

    /** Para endpoints que retornam lista — cada elemento vira uma linha. */
    ResponseEntity<byte[]> exportarLista(List<?> dados, Class<?> tipoDto,
                                         FormatoExportacao formato, ExportContext ctx);

    /** Para endpoints que retornam um único objeto agregado — vira tabela chave→valor. */
    ResponseEntity<byte[]> exportarObjeto(Object dto,
                                          FormatoExportacao formato, ExportContext ctx);
}
