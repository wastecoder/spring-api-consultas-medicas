package com.consultas.api_consultas.dtos.respostas.relatorios.pacientes;

import com.consultas.api_consultas.dtos.respostas.PessoaResumo;
import com.consultas.api_consultas.entities.Pessoa;
import com.consultas.api_consultas.enums.StatusConsulta;

import java.time.LocalDate;
import java.time.LocalTime;

public record HistoricoConsultaPacienteDto(
        Long idConsulta,
        LocalDate dataAtendimento,
        LocalTime horarioAtendimento,
        StatusConsulta status,
        PessoaResumo medico
) {

    public HistoricoConsultaPacienteDto(
            Long id,
            LocalDate data,
            LocalTime horario,
            StatusConsulta status,
            Pessoa medico
    ) {

        this(
                id, data, horario, status,
                new PessoaResumo(medico)
        );

    }

}
