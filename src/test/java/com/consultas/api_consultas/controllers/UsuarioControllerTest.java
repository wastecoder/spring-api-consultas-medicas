package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.UsuarioService;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.EntityNotFoundException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(UsuarioController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class})
@ActiveProfiles("test")
class UsuarioControllerTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private UsuarioService usuarioService;

    private UsuarioCadastroDto cadastroValido() {
        return UsuarioCadastroDto.builder()
                .username("usuario_teste")
                .email("usuario_teste@example.com")
                .senha("senha123")
                .funcao(Funcao.RECEPCIONISTA)
                .idAssociado(null)
                .build();
    }

    private UsuarioResposta respostaSalva(Long id) {
        Usuario u = Usuario.builder()
                .id(id)
                .username("usuario_teste")
                .email("usuario_teste@example.com")
                .senha("hash-bcrypt")
                .funcao(Funcao.RECEPCIONISTA)
                .ativo(true)
                .build();
        return new UsuarioResposta(u);
    }


    @Nested
    @DisplayName("POST /usuarios")
    class Cadastrar {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 201 quando admin cria usuário")
        void deveCadastrarComSucesso() throws Exception {
            when(usuarioService.salvar(any(UsuarioCadastroDto.class))).thenReturn(respostaSalva(1L));

            mvc.perform(post("/usuarios")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(cadastroValido())))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.id").value(1))
                    .andExpect(jsonPath("$.username").value("usuario_teste"))
                    .andExpect(jsonPath("$.funcao").value("RECEPCIONISTA"));
        }

        @Test
        @WithMockUser(roles = "MEDICO")
        @DisplayName("Deve retornar 403 quando médico tenta criar usuário")
        void deveRetornar403QuandoRoleSemPermissao() throws Exception {
            mvc.perform(post("/usuarios")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(cadastroValido())))
                    .andExpect(status().isForbidden());
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 400 quando username é muito curto")
        void deveRetornar400QuandoBodyInvalido() throws Exception {
            UsuarioCadastroDto invalido = UsuarioCadastroDto.builder()
                    .username("ab") // menor que 5
                    .senha("123")   // menor que 5
                    .funcao(null)
                    .build();

            mvc.perform(post("/usuarios")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(invalido)))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").exists());
        }
    }


    @Nested
    @DisplayName("GET /usuarios")
    class Listar {

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 200 e a página de usuários")
        void deveListarComSucesso() throws Exception {
            PageResponse<UsuarioResposta> page = new PageResponse<>(
                    java.util.List.of(respostaSalva(1L)),
                    0, 5, 1L, 1, true, true, false, false
            );
            when(usuarioService.buscarTodos(0, 5)).thenReturn(page);

            mvc.perform(get("/usuarios"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.content[0].id").value(1));
        }
    }


    @Nested
    @DisplayName("GET /usuarios/{id}")
    class BuscarPorId {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 200 quando usuário existe")
        void deveBuscar() throws Exception {
            Usuario usuario = Usuario.builder()
                    .id(7L).username("usuario_teste").senha("hash")
                    .funcao(Funcao.RECEPCIONISTA).ativo(true).build();
            when(usuarioService.buscarPorId(7L)).thenReturn(usuario);

            mvc.perform(get("/usuarios/{id}", 7L))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(7));
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 404 quando usuário não existe")
        void deveRetornar404() throws Exception {
            when(usuarioService.buscarPorId(99L))
                    .thenThrow(new EntityNotFoundException("Usuário não encontrado"));

            mvc.perform(get("/usuarios/{id}", 99L))
                    .andExpect(status().isNotFound());
        }
    }


    @Nested
    @DisplayName("PUT /usuarios/{id}")
    class Editar {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 200 quando admin edita")
        void deveEditar() throws Exception {
            Usuario usuario = Usuario.builder()
                    .id(3L).username("novo_user").senha("hash")
                    .funcao(Funcao.RECEPCIONISTA).ativo(true).build();
            when(usuarioService.atualizar(eq(3L), any(UsuarioAtualizacaoDto.class))).thenReturn(usuario);

            UsuarioAtualizacaoDto req = UsuarioAtualizacaoDto.builder()
                    .username("novo_user").email("novo_user@example.com").senha("senha123").build();

            mvc.perform(put("/usuarios/{id}", 3L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(req)))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.username").value("novo_user"));
        }

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 403 quando recepcionista tenta editar (somente ADMIN)")
        void deveRetornar403QuandoRecepcionistaEdita() throws Exception {
            UsuarioAtualizacaoDto req = UsuarioAtualizacaoDto.builder()
                    .username("novo_user").email("novo_user@example.com").build();

            mvc.perform(put("/usuarios/{id}", 3L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(req)))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("DELETE /usuarios/{id}")
    class Excluir {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 quando admin exclui")
        void deveExcluir() throws Exception {
            mvc.perform(delete("/usuarios/{id}", 5L))
                    .andExpect(status().isNoContent());
            verify(usuarioService).removerPorId(5L);
        }

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 403 quando recepcionista tenta excluir (somente ADMIN)")
        void deveRetornar403() throws Exception {
            mvc.perform(delete("/usuarios/{id}", 5L))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("PATCH /usuarios/inativar e /ativar")
    class AtivarInativar {

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 204 ao inativar")
        void deveInativar() throws Exception {
            mvc.perform(patch("/usuarios/inativar/{id}", 2L))
                    .andExpect(status().isNoContent());
            verify(usuarioService).inativarPorId(2L);
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 ao ativar")
        void deveAtivar() throws Exception {
            mvc.perform(patch("/usuarios/ativar/{id}", 2L))
                    .andExpect(status().isNoContent());
            verify(usuarioService).ativarPorId(2L);
        }
    }


    @Test
    @DisplayName("Deve retornar 401 quando requisição não está autenticada")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/usuarios"))
                .andExpect(status().isUnauthorized());
    }
}
