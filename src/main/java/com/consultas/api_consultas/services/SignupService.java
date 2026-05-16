package com.consultas.api_consultas.services;

import com.consultas.api_consultas.dtos.requisicoes.SignupDto;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;

public interface SignupService {

    AuthTokenDTO cadastrarPaciente(SignupDto dto);

}
