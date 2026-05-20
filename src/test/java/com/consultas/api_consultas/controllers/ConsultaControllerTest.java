package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.configs.TestSecurityConfig;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.handlers.GlobalExceptionHandler;
import com.consultas.api_consultas.mappers.ConsultaMapper;
import com.consultas.api_consultas.mappers.ConsultaMapperImpl;
import com.consultas.api_consultas.services.ConsultaService;
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

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(ConsultaController.class)
@Import({TestSecurityConfig.class, GlobalExceptionHandler.class, ConsultaMapperImpl.class})
@ActiveProfiles("test")
class ConsultaControllerTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private ConsultaMapper consultaMapper;

    @MockBean
    private ConsultaService consultaService;

    private Map<String, Object> corpoCadastroValido() {
        Map<String, Object> corpo = new LinkedHashMap<>();
        corpo.put("dataAtendimento", LocalDate.now().plusDays(7).toString());
        corpo.put("horarioAtendimento", "10:00");
        corpo.put("duracaoEmMinutos", 30);
        corpo.put("preco", 150.00);
        corpo.put("motivo", "Consulta de rotina");
        corpo.put("medicoId", 1);
        corpo.put("pacienteId", 2);
        return corpo;
    }

    private Map<String, Object> corpoAtualizacaoValido() {
        Map<String, Object> corpo = new LinkedHashMap<>(corpoCadastroValido());
        corpo.put("status", StatusConsulta.AGENDADA.name());
        return corpo;
    }

    private Consulta consultaSalva(Long id) {
        Medico medico = new Medico("Dra. Ana", "ana@x.com", "11999990000",
                SiglaCrm.SP, "111111", Especialidade.CARDIOLOGIA);
        medico.setId(1L);
        medico.setAtivo(true);

        Paciente paciente = new Paciente("João", "joao@x.com", "11999990001",
                "12345678901", Sexo.MASCULINO, LocalDate.of(1990, 1, 1));
        paciente.setId(2L);
        paciente.setAtivo(true);

        Consulta c = new Consulta(
                LocalDate.now().plusDays(7),
                LocalTime.of(10, 0),
                Duration.ofMinutes(30),
                BigDecimal.valueOf(150.00),
                "Consulta de rotina",
                medico,
                paciente
        );
        c.setId(id);
        c.setStatus(StatusConsulta.AGENDADA);
        c.setDataAgendamento(LocalDateTime.now());
        return c;
    }


    @Nested
    @DisplayName("POST /consultas")
    class Cadastrar {

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 201 quando paciente cadastra sua consulta")
        void deveCadastrarComoPaciente() throws Exception {
            when(consultaService.salvar(any(Consulta.class))).thenReturn(consultaSalva(10L));

            mvc.perform(post("/consultas")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoCadastroValido())))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.id").value(10))
                    .andExpect(jsonPath("$.status").value("AGENDADA"))
                    .andExpect(jsonPath("$.medico.id").value(1))
                    .andExpect(jsonPath("$.paciente.id").value(2));
        }

        @Test
        @WithMockUser(roles = "MEDICO")
        @DisplayName("Deve retornar 201 quando médico cadastra consulta")
        void deveCadastrarComoMedico() throws Exception {
            when(consultaService.salvar(any(Consulta.class))).thenReturn(consultaSalva(10L));

            mvc.perform(post("/consultas")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoCadastroValido())))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.id").value(10))
                    .andExpect(jsonPath("$.status").value("AGENDADA"));
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 400 quando o corpo é inválido (medicoId ausente)")
        void deveRetornar400QuandoBodyInvalido() throws Exception {
            Map<String, Object> corpo = corpoCadastroValido();
            corpo.remove("medicoId");

            mvc.perform(post("/consultas")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpo)))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").value(org.hamcrest.Matchers.containsString("medicoId")));
        }
    }


    @Nested
    @DisplayName("GET /consultas")
    class Listar {

        @Test
        @WithMockUser(roles = "MEDICO")
        @DisplayName("Deve retornar 200 quando médico lista consultas")
        void deveListarComoMedico() throws Exception {
            PageResponse<ConsultaResposta> page = new PageResponse<>(
                    List.of(consultaMapper.paraResposta(consultaSalva(1L))),
                    0, 5, 1L, 1, true, true, false, false
            );
            when(consultaService.buscarConsultas(anyInt(), anyInt(), any(), any(), any(), any(), any(), any())).thenReturn(page);

            mvc.perform(get("/consultas"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.content[0].id").value(1));
        }
    }


    @Nested
    @DisplayName("GET /consultas/{id}")
    class BuscarPorId {

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 200 quando paciente busca consulta")
        void deveBuscarComoPaciente() throws Exception {
            when(consultaService.buscarPorId(7L)).thenReturn(consultaSalva(7L));

            mvc.perform(get("/consultas/{id}", 7L))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(7));
        }

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 404 quando consulta não existe")
        void deveRetornar404QuandoNaoEncontrada() throws Exception {
            when(consultaService.buscarPorId(99L))
                    .thenThrow(new EntityNotFoundException("Consulta não encontrada"));

            mvc.perform(get("/consultas/{id}", 99L))
                    .andExpect(status().isNotFound());
        }
    }


    @Nested
    @DisplayName("PUT /consultas/{id}")
    class Editar {

        @Test
        @WithMockUser(roles = "RECEPCIONISTA")
        @DisplayName("Deve retornar 200 quando recepcionista edita")
        void deveEditarComSucesso() throws Exception {
            when(consultaService.atualizar(eq(3L), any(ConsultaAtualizacaoDto.class))).thenReturn(consultaSalva(3L));

            mvc.perform(put("/consultas/{id}", 3L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoAtualizacaoValido())))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.id").value(3));
        }

        @Test
        @WithMockUser(roles = "PACIENTE")
        @DisplayName("Deve retornar 403 quando paciente tenta editar consulta")
        void deveRetornar403QuandoPacienteEdita() throws Exception {
            mvc.perform(put("/consultas/{id}", 3L)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(corpoAtualizacaoValido())))
                    .andExpect(status().isForbidden());
        }
    }


    @Nested
    @DisplayName("DELETE /consultas/{id}")
    class Excluir {

        @Test
        @WithMockUser(roles = "ADMIN")
        @DisplayName("Deve retornar 204 quando admin exclui")
        void deveExcluir() throws Exception {
            mvc.perform(delete("/consultas/{id}", 5L))
                    .andExpect(status().isNoContent());
            verify(consultaService).removerPorId(5L);
        }

        @Test
        @WithMockUser(roles = "MEDICO")
        @DisplayName("Deve retornar 403 quando médico tenta excluir")
        void deveRetornar403QuandoMedicoExclui() throws Exception {
            mvc.perform(delete("/consultas/{id}", 5L))
                    .andExpect(status().isForbidden());
        }
    }


    @Test
    @DisplayName("Deve retornar 401 quando requisição não está autenticada")
    void deveRetornar401SemAutenticacao() throws Exception {
        mvc.perform(get("/consultas"))
                .andExpect(status().isUnauthorized());
    }
}
