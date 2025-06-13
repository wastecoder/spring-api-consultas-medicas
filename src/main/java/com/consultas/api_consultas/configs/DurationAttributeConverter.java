package com.consultas.api_consultas.configs;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;

import java.time.Duration;

@Converter(autoApply = true)
public class DurationAttributeConverter implements AttributeConverter<Duration, Long> {

    @Override
    public Long convertToDatabaseColumn(Duration duration) {
        return (duration != null) ? duration.toMinutes() : null;
    }

    @Override
    public Duration convertToEntityAttribute(Long minutes) {
        return (minutes != null) ? Duration.ofMinutes(minutes) : null;
    }

}
