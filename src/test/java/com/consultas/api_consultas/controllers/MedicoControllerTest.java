package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.services.MedicoService;
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

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(MedicoController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class})
@ActiveProfiles("test")
class MedicoControllerTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private MedicoService medicoService;

    private MedicoRequisicao requisicaoValida() {
        MedicoRequisicao req = new MedicoRequisicao();
        req.setNome("Dra. Ana Souza");
        req.setEmail("ana@clinica.com");
        req.setCrmSigla(SiglaCrm.SP);
        req.setCrmDigitos("123456");
        req.setEspecialidade(Especialidade.CARDIOLOGIA);
        req.setTelefone("11987654321");
        return req;
    }

    private Medico medicoSalvo(Long id) {
        Medico m = new Medico("Dra. Ana Souza", "ana@clinica.com", "11987654321",
                SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA);
        m.setId(id);
        m.setAtivo(true);
        return m;
    }


    @Nested
    @DisplayName("POST /medicos")
    class Cadastrar {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 201 e o médico salvo quando a requisição é válida")
        void deveCadastrarComSucesso() throws Exception {
            when(medicoService.salvar(any(Medico.class))).thenReturn(medicoSalvo(10L));

            mvc.perform(post("/medicos")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(requisicaoValida())))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.id").value(10))
                    .andExpect(jsonPath("$.nome").value("Dra. Ana Souza"))
                    .andExpect(jsonPath("$.crmSigla").value("SP"))
                    .andExpect(jsonPath("$.especialidade").value("CARDIOLOGIA"))
                    .andExpect(jsonPath("$.ativo").value(true));
        }

        @Test
        @WithMockUser(roles = "MEDICO")
        @DisplayName("Deve retornar 403 quando role não autorizada tenta cadastrar")
        void deveRetornar403QuandoRoleSemPermissao() throws Exception {
            mvc.perform(post("/medicos")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(requisicaoValida())))
                    .andExpect(status().isForbidden())
                    .andExpect(jsonPath("$.message").value("Você não tem permissão para realizar esta operação."));
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 400 quando o corpo da requisição é inválido")
        void deveRetornar400QuandoBodyInvalido() throws Exception {
            MedicoRequisicao invalido = new MedicoRequisicao();
            invalido.setNome(""); // viola @NotBlank/@Size
            invalido.setEmail("nao-eh-email");
            invalido.setCrmDigitos("abc");
            invalido.setTelefone("123");

            mvc.perform(post("/medicos")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(invalido)))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").exists());
        }
    }


    @Nested
    @DisplayName("GET /medicos")
    class Listar {

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 200 e a página de médicos")
        void deveListarComSucesso() throws Exception {
            PageResponse<MedicoResposta> page = new PageResponse<>(
                    List.of(new MedicoResposta(medicoSalvo(1L))),
                    0, 5, 1L, 1, true, true, false, false
            );
            when(medicoService.buscarMedicos(eq(0), eq(5), any(), any(), any(), any())).thenReturn(page);

            mvc.perform(get("/medicos"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.content[0].id").value(1))
                    .andExpect(jsonPath("$.totalElements").value(1));
        }
    }


    @Nested
    @DisplayName("GET /medicos/{id}")
    class BuscarPorId {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 200 e o médico encontrado")
        void deveBuscarPorIdComSucesso() throws Exception {
            when(medicoService.buscarPorId(7L)).thenReturn(medicoSalvo(7L));

            mvc.perform(get("/medicos/{id}", 7L))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(7))
                    .andExpect(jsonPath("$.email").value("ana@clinica.com"));
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 404 quando o médico não existe")
        void deveRetornar404QuandoNaoEncontrado() throws Exception {
            when(medicoService.buscarPorId(99L))
                    .thenThrow(new EntityNotFoundException("Médico não encontrado"));

            mvc.perform(get("/medicos/{id}", 99L))
                    .andExpect(status().isNotFound())
                    .andExpect(jsonPath("$.message").value("Médico não encontrado"));
        }
    }


    @Nested
    @DisplayName("PUT /medicos/{id}")
    class Editar {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 200 quando admin edita médico")
        void deveEditarComSucesso() throws Exception {
            when(medicoService.atualizar(eq(3L), any(Medico.class))).thenReturn(medicoSalvo(3L));

            mvc.perform(put("/medicos/{id}", 3L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(requisicaoValida())))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(3));
        }

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 403 quando recepcionista tenta editar (somente ADMIN)")
        void deveRetornar403QuandoRecepcionistaEdita() throws Exception {
            mvc.perform(put("/medicos/{id}", 3L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(requisicaoValida())))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("DELETE /medicos/{id}")
    class Excluir {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 quando admin exclui médico")
        void deveExcluirComSucesso() throws Exception {
            mvc.perform(delete("/medicos/{id}", 5L))
                    .andExpect(status().isNoContent());
            verify(medicoService).removerPorId(5L);
        }

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 403 quando recepcionista tenta excluir (somente ADMIN)")
        void deveRetornar403QuandoRecepcionistaExclui() throws Exception {
            mvc.perform(delete("/medicos/{id}", 5L))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("PATCH /medicos/inativar/{id} e /ativar/{id}")
    class AtivarInativar {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 ao inativar")
        void deveInativar() throws Exception {
            mvc.perform(patch("/medicos/inativar/{id}", 2L))
                    .andExpect(status().isNoContent());
            verify(medicoService).inativarPorId(2L);
        }

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 204 ao ativar (recepcionista também pode)")
        void deveAtivar() throws Exception {
            mvc.perform(patch("/medicos/ativar/{id}", 2L))
                    .andExpect(status().isNoContent());
            verify(medicoService).ativarPorId(2L);
        }
    }


    @Test
    @DisplayName("Deve retornar 401 quando requisição não está autenticada")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/medicos"))
                .andExpect(status().isUnauthorized());
    }
}
