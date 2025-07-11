package com.consultas.api_consultas.services;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Sexo;

import java.util.List;

public interface PacienteService {

    Paciente salvar(Paciente pacienteNovo);

    List<Paciente> buscarTodos();

    Paciente buscarPorId(Long id);

    List<Paciente> buscarPacientes(String nome, String cpf, Sexo sexo, Boolean ativo);

    Paciente atualizar(Long id, Paciente pacienteAtualizado);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

    Paciente buscarPorUsuario(Usuario usuario);

}
