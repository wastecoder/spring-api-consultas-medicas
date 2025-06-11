package com.consultas.api_consultas.utils;

import java.time.Duration;
import java.time.LocalTime;

// Classe utilitária responsável por conversões de entrada (input parsing),
// como transformar Strings ou números em tipos do domínio (ex: LocalTime, Duration).
public class ConversorEntradaUtils {

    private ConversorEntradaUtils() {}

    public static LocalTime converterStringParaLocalTime(String horario) {
        return LocalTime.parse(horario);
    }

    public static Duration converterIntegerParaDuration(Integer minutos) {
        return Duration.ofMinutes(minutos);
    }

}
