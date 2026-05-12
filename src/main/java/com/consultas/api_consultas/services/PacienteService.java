package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.PacienteRequisicao;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;

import java.util.List;

public interface PacienteService {

    Paciente salvar(Paciente pacienteNovo);

    List<Paciente> buscarTodos();

    Paciente buscarPorId(Long id);

    PageResponse<PacienteResposta> buscarPacientes(int pagina, int tamanho, String nome, String cpf, Sexo sexo, Boolean ativo);

    Paciente atualizar(Long id, PacienteRequisicao requisicao);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

    Paciente buscarPorUsernameEAtivo(String username, Boolean ativo);

}
