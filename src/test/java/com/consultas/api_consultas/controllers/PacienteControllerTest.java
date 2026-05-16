package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.PacienteRequisicao;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.mappers.PacienteMapper;
import com.consultas.api_consultas.mappers.PacienteMapperImpl;
import com.consultas.api_consultas.services.PacienteService;
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

import java.time.LocalDate;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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

@WebMvcTest(PacienteController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class, PacienteMapperImpl.class})
@ActiveProfiles("test")
class PacienteControllerTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private PacienteMapper pacienteMapper;

    @MockBean
    private PacienteService pacienteService;

    private Map<String, Object> corpoValido() {
        Map<String, Object> corpo = new LinkedHashMap<>();
        corpo.put("nome", "João da Silva");
        corpo.put("email", "joao@email.com");
        corpo.put("cpf", "12345678901");
        corpo.put("sexo", Sexo.MASCULINO.name());
        corpo.put("dataNascimento", LocalDate.of(1990, 5, 20).toString());
        corpo.put("telefone", "11987654321");
        return corpo;
    }

    private Paciente pacienteSalvo(Long id) {
        Paciente p = new Paciente("João da Silva", "joao@email.com", "11987654321",
                "12345678901", Sexo.MASCULINO, LocalDate.of(1990, 5, 20));
        p.setId(id);
        p.setAtivo(true);
        return p;
    }


    @Nested
    @DisplayName("POST /pacientes")
    class Cadastrar {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 201 quando admin cadastra paciente")
        void deveCadastrarComSucesso() throws Exception {
            when(pacienteService.salvar(any(Paciente.class))).thenReturn(pacienteSalvo(1L));

            mvc.perform(post("/pacientes")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoValido())))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.id").value(1))
                    .andExpect(jsonPath("$.cpf").value("12345678901"))
                    .andExpect(jsonPath("$.sexo").value("MASCULINO"));
        }

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 403 quando paciente tenta cadastrar (somente ADMIN/RECEPCIONISTA)")
        void deveRetornar403QuandoRoleSemPermissao() throws Exception {
            mvc.perform(post("/pacientes")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoValido())))
                    .andExpect(status().isForbidden());
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 400 quando o corpo da requisição é inválido")
        void deveRetornar400QuandoBodyInvalido() throws Exception {
            String corpoInvalido = "{}"; // todos os campos obrigatórios ausentes

            mvc.perform(post("/pacientes")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(corpoInvalido))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").exists());
        }
    }


    @Nested
    @DisplayName("GET /pacientes")
    class Listar {

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 200 e a página de pacientes")
        void deveListarComSucesso() throws Exception {
            PageResponse<PacienteResposta> page = new PageResponse<>(
                    List.of(pacienteMapper.paraResposta(pacienteSalvo(1L))),
                    0, 5, 1L, 1, true, true, false, false
            );
            when(pacienteService.buscarPacientes(eq(0), eq(5), any(), any(), any(), any(), any(), any())).thenReturn(page);

            mvc.perform(get("/pacientes"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.content[0].id").value(1));
        }

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 403 quando paciente tenta listar todos (somente ADMIN/RECEPCIONISTA)")
        void deveRetornar403QuandoPacienteListaTodos() throws Exception {
            mvc.perform(get("/pacientes"))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("GET /pacientes/{id}")
    class BuscarPorId {

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 200 quando paciente busca pelo próprio id (controller libera; validação de mesmo paciente fica no service)")
        void devePermitirPacienteBuscarPorId() throws Exception {
            when(pacienteService.buscarPorId(7L)).thenReturn(pacienteSalvo(7L));

            mvc.perform(get("/pacientes/{id}", 7L))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(7));
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 404 quando paciente não existe")
        void deveRetornar404QuandoNaoEncontrado() throws Exception {
            when(pacienteService.buscarPorId(99L))
                    .thenThrow(new EntityNotFoundException("Paciente não encontrado"));

            mvc.perform(get("/pacientes/{id}", 99L))
                    .andExpect(status().isNotFound());
        }
    }


    @Nested
    @DisplayName("PUT /pacientes/{id}")
    class Editar {

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 200 quando paciente edita os próprios dados")
        void deveEditarComoPaciente() throws Exception {
            when(pacienteService.atualizar(eq(7L), any(PacienteRequisicao.class))).thenReturn(pacienteSalvo(7L));

            mvc.perform(put("/pacientes/{id}", 7L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoValido())))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(7));
        }

        @Test
        @WithMockUser(roles = "MEDICO")
        @DisplayName("Deve retornar 403 quando médico tenta editar paciente")
        void deveRetornar403QuandoMedicoEdita() throws Exception {
            mvc.perform(put("/pacientes/{id}", 7L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoValido())))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("DELETE /pacientes/{id}")
    class Excluir {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 quando admin exclui")
        void deveExcluir() throws Exception {
            mvc.perform(delete("/pacientes/{id}", 5L))
                    .andExpect(status().isNoContent());
            verify(pacienteService).removerPorId(5L);
        }

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 403 quando recepcionista tenta excluir (somente ADMIN)")
        void deveRetornar403QuandoRecepcionistaExclui() throws Exception {
            mvc.perform(delete("/pacientes/{id}", 5L))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("PATCH /pacientes/inativar e /ativar")
    class AtivarInativar {

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 204 ao inativar")
        void deveInativar() throws Exception {
            mvc.perform(patch("/pacientes/inativar/{id}", 2L))
                    .andExpect(status().isNoContent());
            verify(pacienteService).inativarPorId(2L);
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 ao ativar")
        void deveAtivar() throws Exception {
            mvc.perform(patch("/pacientes/ativar/{id}", 2L))
                    .andExpect(status().isNoContent());
            verify(pacienteService).ativarPorId(2L);
        }
    }


    @Test
    @DisplayName("Deve retornar 401 quando requisição não está autenticada")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/pacientes"))
                .andExpect(status().isUnauthorized());
    }
}
