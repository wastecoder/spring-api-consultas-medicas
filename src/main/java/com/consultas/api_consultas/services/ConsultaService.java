package com.consultas.api_consultas.services;

import com.consultas.api_consultas.entities.Consulta;

import java.util.List;

public interface ConsultaService {

    Consulta salvar(Consulta consultaNova);

    List<Consulta> buscarTodos();

    Consulta buscarPorId(Long id);

    Consulta atualizar(Long id, Consulta consultaAtualizada);

    void removerPorId(Long id);

}
