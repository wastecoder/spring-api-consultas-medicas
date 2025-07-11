package com.consultas.api_consultas.services;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.SiglaCrm;

import java.util.List;

public interface MedicoService {

    Medico salvar(Medico medicoNovo);

    List<Medico> buscarTodos();

    Medico buscarPorId(Long id);

    List<Medico> buscarMedicos(String nome, SiglaCrm crmSigla, String crmDigitos, Boolean ativo);

    Medico atualizar(Long id, Medico medicoAtualizado);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

    Medico buscarPorUsuario(Usuario usuario);

}
