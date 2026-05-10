package com.consultas.api_consultas.constants;

import java.time.LocalDate;

public final class AppConstants {

    private AppConstants() {}

    // JWT
    public static final long JWT_EXPIRACAO_SEGUNDOS = 3600L;
    public static final long JWT_REFRESH_EXPIRACAO_SEGUNDOS = 604800L; // 7 dias

    // Consulta
    public static final long DURACAO_CONSULTA_MAX_MINUTOS = 480L;

    // Relatório de produtividade
    public static final double MINUTOS_POR_DIA = 1440.0;

    // Relatório financeiro — limites de datas aceitas
    public static final LocalDate RELATORIO_DATA_LIMITE_INFERIOR = LocalDate.of(2000, 1, 1);
    public static final int RELATORIO_DATA_LIMITE_SUPERIOR_ANOS_FUTURO = 5;

    // Paginação default
    public static final String PAGINACAO_PAGINA_DEFAULT = "0";
    public static final String PAGINACAO_TAMANHO_DEFAULT = "5";
}
