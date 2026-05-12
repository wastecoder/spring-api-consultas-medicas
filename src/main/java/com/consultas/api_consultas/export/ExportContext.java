package com.consultas.api_consultas.export;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.text.Normalizer;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Metadados de uma exportação:
 * - título humano (usado no PDF e no nome do arquivo);
 * - parâmetros usados (ex: "Período: 01/01/2025 a 31/12/2025") — viram linhas no cabeçalho do PDF;
 * - timestamp de geração — usado no PDF e no nome do arquivo.
 */
public record ExportContext(
        String tituloRelatorio,
        Map<String, String> parametros,
        LocalDateTime geradoEm
) {

    private static final DateTimeFormatter SLUG_TS = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss");

    public static ExportContext of(String titulo) {
        return new ExportContext(titulo, Map.of(), LocalDateTime.now());
    }

    public static ExportContext of(String titulo, Map<String, String> parametros) {
        return new ExportContext(titulo, parametros, LocalDateTime.now());
    }

    /** Ordem dos parâmetros é preservada para o PDF (LinkedHashMap). */
    public Map<String, String> parametrosOrdenados() {
        if (parametros == null || parametros.isEmpty()) return Map.of();
        return new LinkedHashMap<>(parametros);
    }

    /** Slug do nome do arquivo: "faturamento-mensal-20260511-143022". */
    public String slugArquivo() {
        return slugify(tituloRelatorio) + "-" + geradoEm.format(SLUG_TS);
    }

    private static String slugify(String in) {
        String norm = Normalizer.normalize(in, Normalizer.Form.NFD)
                .replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
        return norm.toLowerCase()
                .replaceAll("[^a-z0-9]+", "-")
                .replaceAll("(^-|-$)", "");
    }
}
