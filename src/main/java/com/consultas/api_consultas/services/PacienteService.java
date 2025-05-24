package com.consultas.api_consultas.services;

import com.consultas.api_consultas.entities.Paciente;

import java.util.List;

public interface PacienteService {

    Paciente salvar(Paciente pacienteNovo);

    List<Paciente> buscarTodos();

    Paciente buscarPorId(Long id);

    Paciente atualizar(Long id, Paciente pacienteAtualizado);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

}
