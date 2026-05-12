package com.consultas.api_consultas.export;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.text.NumberFormat;
import java.util.Locale;

/**
 * Formatação pt-BR centralizada para os exporters (CSV e PDF).
 * Sem isso, BigDecimal sairia "1234.56" e datas em ISO, deixando a planilha/PDF feio.
 */
public final class FormatadorValor {

    @SuppressWarnings("deprecation") // Locale.of só existe a partir do JDK 19; projeto é 17.
    private static final Locale BR = new Locale("pt", "BR");
    private static final DateTimeFormatter DATA = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    private static final DateTimeFormatter HORA = DateTimeFormatter.ofPattern("HH:mm");
    private static final DateTimeFormatter DATA_HORA = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");

    private FormatadorValor() {}

    public static String formatar(Object valor) {
        if (valor == null) return "";
        if (valor instanceof BigDecimal bd) return numero().format(bd);
        if (valor instanceof Double d) return numero().format(d);
        if (valor instanceof Float f) return numero().format(f);
        if (valor instanceof LocalDate ld) return ld.format(DATA);
        if (valor instanceof LocalTime lt) return lt.format(HORA);
        if (valor instanceof LocalDateTime ldt) return ldt.format(DATA_HORA);
        if (valor instanceof OffsetDateTime odt) return odt.format(DATA_HORA);
        if (valor instanceof Enum<?> e) return e.name();
        return valor.toString();
    }

    public static boolean ehNumerico(Class<?> tipo) {
        if (tipo == null) return false;
        return Number.class.isAssignableFrom(tipo)
                || tipo == int.class || tipo == long.class
                || tipo == double.class || tipo == float.class
                || tipo == short.class || tipo == byte.class;
    }

    private static NumberFormat numero() {
        NumberFormat nf = NumberFormat.getNumberInstance(BR);
        nf.setMinimumFractionDigits(2);
        nf.setMaximumFractionDigits(2);
        return nf;
    }
}
