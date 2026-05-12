package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class MedicoResposta {

    private Long id;

    private String nome;

    private String email;

    private SiglaCrm crmSigla;

    private String crmDigitos;

    private Especialidade especialidade;

    private String telefone;

    private Boolean ativo;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AuditoriaResposta auditoria;

}
