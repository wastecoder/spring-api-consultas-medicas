package com.consultas.api_consultas.export;

import com.consultas.api_consultas.enums.Sexo;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FormatadorValorTest {

    @Test
    void nullViraStringVazia() {
        assertEquals("", FormatadorValor.formatar(null));
    }

    @Test
    void bigDecimalUsaFormatoPtBr() {
        assertEquals("1.234,56", FormatadorValor.formatar(new BigDecimal("1234.56")));
    }

    @Test
    void doubleUsaFormatoPtBr() {
        assertEquals("3,14", FormatadorValor.formatar(3.14));
    }

    @Test
    void localDateUsaDdMmYyyy() {
        assertEquals("11/05/2026", FormatadorValor.formatar(LocalDate.of(2026, 5, 11)));
    }

    @Test
    void localTimeUsaHhMm() {
        assertEquals("14:30", FormatadorValor.formatar(LocalTime.of(14, 30)));
    }

    @Test
    void localDateTimeUsaDataHora() {
        assertEquals("11/05/2026 14:30",
                FormatadorValor.formatar(LocalDateTime.of(2026, 5, 11, 14, 30)));
    }

    @Test
    void enumUsaName() {
        assertEquals("MASCULINO", FormatadorValor.formatar(Sexo.MASCULINO));
    }

    @Test
    void stringPassaInalterada() {
        assertEquals("texto", FormatadorValor.formatar("texto"));
    }

    @Test
    void detectaTiposNumericos() {
        assertTrue(FormatadorValor.ehNumerico(BigDecimal.class));
        assertTrue(FormatadorValor.ehNumerico(long.class));
        assertTrue(FormatadorValor.ehNumerico(Integer.class));
        assertTrue(FormatadorValor.ehNumerico(double.class));
    }
}
