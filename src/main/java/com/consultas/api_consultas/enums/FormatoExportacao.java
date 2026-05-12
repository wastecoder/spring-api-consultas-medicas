package com.consultas.api_consultas.enums;

public enum FormatoExportacao {

    JSON("application/json", "json"),
    CSV("text/csv; charset=UTF-8", "csv"),
    PDF("application/pdf", "pdf");

    private final String contentType;
    private final String extensao;

    FormatoExportacao(String contentType, String extensao) {
        this.contentType = contentType;
        this.extensao = extensao;
    }

    public String contentType() {
        return contentType;
    }

    public String extensao() {
        return extensao;
    }

    public boolean isJson() {
        return this == JSON;
    }
}
