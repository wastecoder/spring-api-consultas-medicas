package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

@Getter
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


    public ConsultaResposta(Consulta consulta) {
        this.id = consulta.getId();
        this.dataAtendimento = consulta.getDataAtendimento();
        this.horarioAtendimento = consulta.getHorarioAtendimento();
        this.duracaoEmMinutos = consulta.getDuracaoEmMinutos().toMinutes();
        this.dataAgendamento = consulta.getDataAgendamento();
        this.preco = consulta.getPreco();
        this.motivo = consulta.getMotivo();
        this.status = consulta.getStatus();

        medico = new PessoaResumo(consulta.getMedico());
        paciente = new PessoaResumo(consulta.getPaciente());
    }

}
