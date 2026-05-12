package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;

import java.util.List;

public interface MedicoService {

    Medico salvar(Medico medicoNovo);

    List<Medico> buscarTodos();

    Medico buscarPorId(Long id);

    PageResponse<MedicoResposta> buscarMedicos(int pagina, int tamanho, String nome, SiglaCrm crmSigla, String crmDigitos, Boolean ativo);

    Medico atualizar(Long id, MedicoRequisicao requisicao);

    void removerPorId(Long id);

    void inativarPorId(Long id);

    void ativarPorId(Long id);

    Medico buscarPorUsernameEAtivo(String username, Boolean ativo);

}
