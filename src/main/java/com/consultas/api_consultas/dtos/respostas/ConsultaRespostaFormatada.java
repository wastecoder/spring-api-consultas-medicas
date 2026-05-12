package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.enums.StatusConsulta;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ConsultaRespostaFormatada {

    private Long id;

    private String dataAtendimento;

    @JsonFormat(pattern = "HH:mm")
    private String horarioAtendimento;

    private Long duracaoEmMinutos;

    private String dataAgendamento;

    private String preco;

    private String motivo;

    private StatusConsulta status;

    private PessoaResumo medico;

    private PessoaResumo paciente;

}
