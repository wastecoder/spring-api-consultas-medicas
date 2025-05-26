package com.consultas.api_consultas.utils;

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class FormatoUtils {

    private FormatoUtils() {} // Impede instância

    private static final DateTimeFormatter DATA_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    private static final DateTimeFormatter HORA_FORMATTER = DateTimeFormatter.ofPattern("HH:mm");
    private static final DateTimeFormatter DATA_HORA_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm");


    public static String formatarData(LocalDate data) {
        return data.format(DATA_FORMATTER);
    }

    public static String formatarHora(LocalTime hora) {
        return hora.format(HORA_FORMATTER);
    }

    public static String formatarDataHora(LocalDateTime dataHora) {
        return dataHora.format(DATA_HORA_FORMATTER);
    }

    public static String formatarTelefone(String telefone) {
        if (telefone.length() == 11) {
            return String.format("(%s) %s-%s",
                    telefone.substring(0, 2),
                    telefone.substring(2, 7),
                    telefone.substring(7));
        } else if (telefone.length() == 10) {
            return String.format("(%s) %s-%s",
                    telefone.substring(0, 2),
                    telefone.substring(2, 6),
                    telefone.substring(6));
        } else {
            return telefone;
        }
    }

    public static String formatarCpf(String cpf) {
        return String.format("%s.%s.%s-%s",
                cpf.substring(0, 3),
                cpf.substring(3, 6),
                cpf.substring(6, 9),
                cpf.substring(9, 11));
    }

    public static String formatarPreco(BigDecimal preco) {
        Locale localePtBr = new Locale.Builder()
                .setLanguage("pt")
                .setRegion("BR")
                .build();
        NumberFormat formatoMoeda = NumberFormat
                .getCurrencyInstance(localePtBr);
        return formatoMoeda.format(preco);
    }

    public static String formatarStatusAtivo(Boolean ativo) {
        return Boolean.TRUE.equals(ativo) ? "ativo" : "inativo";
    }

}
