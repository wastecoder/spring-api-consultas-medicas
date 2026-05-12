package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.AuditableEntity;

import java.time.Instant;

public record AuditoriaResposta(
        String createdBy,
        Instant createdDate,
        String lastModifiedBy,
        Instant lastModifiedDate
) {

    public static AuditoriaResposta de(AuditableEntity entidade) {
        return new AuditoriaResposta(
                entidade.getCreatedBy(),
                entidade.getCreatedDate(),
                entidade.getLastModifiedBy(),
                entidade.getLastModifiedDate()
        );
    }

}
