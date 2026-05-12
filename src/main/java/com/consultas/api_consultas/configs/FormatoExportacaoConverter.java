package com.consultas.api_consultas.configs;

import com.consultas.api_consultas.enums.FormatoExportacao;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * Aceita ?formato=csv, CSV, Csv etc. — sem isso o Spring exige match exato com o nome do enum (JSON/CSV/PDF).
 * É um @Component (não @Bean lambda) por dois motivos:
 *   1. Lambdas perdem os parâmetros genéricos em runtime, e o Spring 6 reclama em mvcConversionService;
 *   2. @WebMvcTest auto-detecta Converters por tipo, então fica disponível nos testes sem @Import.
 */
@Component
public class FormatoExportacaoConverter implements Converter<String, FormatoExportacao> {

    @Override
    public FormatoExportacao convert(String source) {
        return FormatoExportacao.valueOf(source.toUpperCase());
    }
}
