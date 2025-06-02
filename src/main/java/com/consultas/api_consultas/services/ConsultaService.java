package com.consultas.api_consultas.services;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;

import java.time.LocalDate;
import java.util.List;

public interface ConsultaService {

    Consulta salvar(Consulta consultaNova);

    List<Consulta> buscarTodos();

    Consulta buscarPorId(Long id);

    List<Consulta> buscarConsultas(Long medicoId, Long pacienteId, LocalDate dataAtendimento, StatusConsulta statusConsulta);

    Consulta atualizar(Long id, Consulta consultaAtualizada);

    void removerPorId(Long id);

}
