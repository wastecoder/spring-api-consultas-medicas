package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.enums.Sexo;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
public class PacienteResposta {

    private Long id;

    private String nome;

    private String email;

    private String cpf;

    private Sexo sexo;

    private LocalDate dataNascimento;

    private String telefone;

    private Boolean ativo;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AuditoriaResposta auditoria;

}
