package com.consultas.api_consultas.dtos.respostas.relatorios.consultas;

import com.consultas.api_consultas.dtos.respostas.PessoaResumo;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;

import java.time.LocalDate;
import java.time.LocalTime;

public record ConsultaResumoDto(
        Long id,
        LocalDate dataAtendimento,
        LocalTime horarioAtendimento,
        StatusConsulta status,
        PessoaResumo medico,
        PessoaResumo paciente
) {

    public ConsultaResumoDto(Consulta c) {
        this(
                c.getId(),
                c.getDataAtendimento(),
                c.getHorarioAtendimento(),
                c.getStatus(),
                new PessoaResumo(c.getMedico()),
                new PessoaResumo(c.getPaciente())
        );
    }

}
