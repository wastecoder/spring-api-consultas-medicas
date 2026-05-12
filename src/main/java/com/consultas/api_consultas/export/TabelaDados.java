package com.consultas.api_consultas.export;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Representação tabular intermediária consumida pelos exporters.
 * Construída por reflection sobre records (campo → coluna).
 */
public record TabelaDados(List<String> headers, List<List<String>> linhas, boolean[] colunasNumericas) {

    /** Tradução manual de nomes comuns; o fallback é camelCase → "Camel Case". */
    private static final Map<String, String> HEADERS_PT = Map.ofEntries(
            Map.entry("id", "ID"),
            Map.entry("idMedico", "ID Médico"),
            Map.entry("idPaciente", "ID Paciente"),
            Map.entry("idConsulta", "ID Consulta"),
            Map.entry("nome", "Nome"),
            Map.entry("nomeMedico", "Médico"),
            Map.entry("nomePaciente", "Paciente"),
            Map.entry("total", "Total"),
            Map.entry("totalFaturado", "Total Faturado"),
            Map.entry("totalPerdido", "Total Perdido"),
            Map.entry("totalPacientes", "Total de Pacientes"),
            Map.entry("totalConsultas", "Total de Consultas"),
            Map.entry("ano", "Ano"),
            Map.entry("mes", "Mês"),
            Map.entry("dia", "Dia"),
            Map.entry("data", "Data"),
            Map.entry("dataConsulta", "Data da Consulta"),
            Map.entry("horarioConsulta", "Horário"),
            Map.entry("statusConsulta", "Status"),
            Map.entry("status", "Status"),
            Map.entry("especialidade", "Especialidade"),
            Map.entry("especialidadeMedica", "Especialidade"),
            Map.entry("sexo", "Sexo"),
            Map.entry("faixaEtaria", "Faixa Etária"),
            Map.entry("taxa", "Taxa"),
            Map.entry("taxaComparecimento", "Taxa de Comparecimento"),
            Map.entry("tempoMedio", "Tempo Médio (min)"),
            Map.entry("tempoMedioDias", "Tempo Médio de Espera (dias)"),
            Map.entry("perDia", "Por Dia"),
            Map.entry("perSemana", "Por Semana"),
            Map.entry("perMes", "Por Mês"),
            Map.entry("agendada", "Agendadas"),
            Map.entry("cancelada", "Canceladas"),
            Map.entry("realizada", "Realizadas")
    );

    /** Constrói a tabela a partir de uma lista de records (mesma classe). */
    public static TabelaDados deLista(List<?> dados, Class<?> tipoDto) {
        if (!tipoDto.isRecord()) {
            throw new IllegalArgumentException(
                    "TabelaDados.deLista só aceita records — recebido: " + tipoDto.getName());
        }
        RecordComponent[] componentes = tipoDto.getRecordComponents();
        List<String> headers = new ArrayList<>(componentes.length);
        boolean[] numericas = new boolean[componentes.length];
        for (int i = 0; i < componentes.length; i++) {
            headers.add(traduzirHeader(componentes[i].getName()));
            numericas[i] = FormatadorValor.ehNumerico(componentes[i].getType());
        }
        List<List<String>> linhas = new ArrayList<>(dados.size());
        for (Object item : dados) {
            List<String> linha = new ArrayList<>(componentes.length);
            for (RecordComponent c : componentes) {
                linha.add(FormatadorValor.formatar(invocar(c.getAccessor(), item)));
            }
            linhas.add(linha);
        }
        return new TabelaDados(headers, linhas, numericas);
    }

    /** Constrói tabela chave→valor (2 colunas) a partir de um único record. */
    public static TabelaDados deObjeto(Object dto) {
        Class<?> tipo = dto.getClass();
        if (!tipo.isRecord()) {
            throw new IllegalArgumentException(
                    "TabelaDados.deObjeto só aceita records — recebido: " + tipo.getName());
        }
        RecordComponent[] componentes = tipo.getRecordComponents();
        List<String> headers = List.of("Campo", "Valor");
        boolean[] numericas = { false, true };
        List<List<String>> linhas = new ArrayList<>(componentes.length);
        for (RecordComponent c : componentes) {
            linhas.add(List.of(
                    traduzirHeader(c.getName()),
                    FormatadorValor.formatar(invocar(c.getAccessor(), dto))
            ));
        }
        return new TabelaDados(headers, linhas, numericas);
    }

    private static String traduzirHeader(String nomeCampo) {
        String traducao = HEADERS_PT.get(nomeCampo);
        if (traducao != null) return traducao;
        // camelCase fallback: "totalFaturado" → "Total Faturado"
        String espacado = nomeCampo.replaceAll("([a-z])([A-Z])", "$1 $2");
        return Character.toUpperCase(espacado.charAt(0)) + espacado.substring(1);
    }

    private static Object invocar(Method accessor, Object alvo) {
        try {
            // Records aninhados em classes package-private (comum em testes) exigem setAccessible
            accessor.setAccessible(true);
            return accessor.invoke(alvo);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new IllegalStateException("Falha ao ler campo " + accessor.getName(), e);
        }
    }
}
