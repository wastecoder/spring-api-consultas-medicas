package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;

import java.time.LocalDate;
import java.util.List;

public interface ConsultaService {

    Consulta salvar(Consulta consultaNova);

    List<Consulta> buscarTodos();

    Consulta buscarPorId(Long id);

    PageResponse<ConsultaResposta> buscarConsultas(int pagina, int tamanho, Long medicoId, Long pacienteId, LocalDate dataAtendimento, StatusConsulta statusConsulta, String ordenarPor, String direcao);

    Consulta atualizar(Long id, ConsultaAtualizacaoDto requisicao);

    Consulta realizar(Long id);

    void removerPorId(Long id);

}
