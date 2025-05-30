package com.consultas.api_consultas.services;

import com.consultas.api_consultas.entities.Medico;

import java.util.List;

public interface MedicoService {

    Medico salvar(Medico medicoNovo);

    List<Medico> buscarTodos();

    Medico buscarPorId(Long id);

    List<Medico> buscarPorAtivo(Boolean ativo);

    Medico atualizar(Long id, Medico medicoAtualizado);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

}
