package com.consultas.api_consultas.dtos.requisicoes;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.validation.constraints.*;
import lombok.Getter;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;

@Getter
public class ConsultaRequisicao {

    @NotNull(message = "Data de atendimento é obrigatória")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate dataAtendimento;

    @NotNull(message = "Horário de atendimento é obrigatório")
    @Pattern(
            regexp = "^([01]\\d|2[0-3]):[0-5]\\d$",
            message = "Horário deve estar no formato HH:mm (00:00 até 23:59)"
    )
    private String horarioAtendimento;

    @NotNull(message = "Duração da consulta é obrigatória")
    @Min(value = 1, message = "Duração deve ser positiva")
    @Max(value = 480, message = "Duração máxima de 480min (8h)")
    private Integer duracaoEmMinutos;

    @NotNull(message = "Preço da consulta é obrigatório")
    @PositiveOrZero(message = "Preço da consulta deve ser 0 ou positivo")
    private BigDecimal preco;

    @NotBlank(message = "Motivo da consulta é obrigatório")
    @Size(max = 200, message = "Motivo da consulta deve ter no máximo 200 caracteres")
    private String motivo;

    @NotNull(message = "Status da consulta é obrigatório")
    private StatusConsulta status;

    @NotNull(message = "ID do médico é obrigatório")
    @Positive(message = "ID do médico deve ser maior do que 0")
    private Long medicoId;

    @NotNull(message = "ID do paciente é obrigatório")
    @Positive(message = "ID do paciente deve ser maior do que 0")
    private Long pacienteId;


    public LocalTime converterHorarioParaLocalTime() {
        return LocalTime.parse(this.horarioAtendimento);
    }

    public Duration converterDuracaoParaDuration() {
        return Duration.ofMinutes(this.duracaoEmMinutos);
    }

    public Consulta dtoParaConsulta() {
        Consulta consulta = new Consulta();
        consulta.setDataAtendimento(this.getDataAtendimento());
        consulta.setHorarioAtendimento(this.converterHorarioParaLocalTime());
        consulta.setDuracaoEmMinutos(this.converterDuracaoParaDuration());
        consulta.setPreco(this.getPreco());
        consulta.setMotivo(this.getMotivo());
        consulta.setStatus(this.getStatus());

        Medico medico = new Medico();
        medico.setId(this.getMedicoId());

        Paciente paciente = new Paciente();
        paciente.setId(this.getPacienteId());

        consulta.setMedico(medico);
        consulta.setPaciente(paciente);

        return consulta;
    }

}
