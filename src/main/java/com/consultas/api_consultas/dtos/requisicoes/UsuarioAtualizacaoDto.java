package com.consultas.api_consultas.dtos.requisicoes;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.Getter;

@Getter
public class UsuarioAtualizacaoDto {

    @NotBlank(message = "Usuário não pode ser vazio")
    @Size(min = 5, max = 30, message = "Usuário deve ter entre 5 a 30 caracteres")
    private String username;

    // Opcional: Pode ocultar esse campo para não atualizar
    @Size(min = 5, max = 100, message = "Senha deve ter entre 5 e 100 caracteres")
    private String senha;

}
