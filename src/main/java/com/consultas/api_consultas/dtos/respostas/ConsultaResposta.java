package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.enums.StatusConsulta;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

@Getter
@Setter
@NoArgsConstructor
public class ConsultaResposta {

    private Long id;

    private LocalDate dataAtendimento;

    @JsonFormat(pattern = "HH:mm")
    private LocalTime horarioAtendimento;

    private Long duracaoEmMinutos;

    private LocalDateTime dataAgendamento;

    private BigDecimal preco;

    private String motivo;

    private StatusConsulta status;

    private PessoaResumo medico;

    private PessoaResumo paciente;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private AuditoriaResposta auditoria;

}
