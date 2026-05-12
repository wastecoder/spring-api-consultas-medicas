package com.consultas.api_consultas.integration;

import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

// Teste end-to-end: sobe a aplicação completa (com SecurityFilterChain real, JWT real,
// PopularBancoComDados rodando) e exercita o fluxo crítico:
// login admin → criar médico → criar paciente → criar consulta → buscar consulta.
@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DisplayName("Integração — fluxo login → criar consulta → buscar")
class FluxoConsultaIntegrationTest {

    @Autowired
    private MockMvc mvc;

    @Autowired
    private ObjectMapper objectMapper;


    @Test
    @DisplayName("Deve fazer login como admin, criar médico, paciente, consulta e buscá-la com sucesso")
    void deveExecutarFluxoCompleto() throws Exception {
        // 1. Login com o admin padrão (criado pelo CommandLineRunner)
        String token = autenticarAdmin();

        // 2. Cadastra médico (sufixo único evita colisão com o seed do PopularBancoComDados)
        String sufixo = UUID.randomUUID().toString().substring(0, 6);
        long medicoId = criarMedico(token, sufixo);

        // 3. Cadastra paciente
        long pacienteId = criarPaciente(token, sufixo);

        // 4. Agenda consulta (próximo dia útil às 10h, dentro do horário comercial)
        LocalDate dataConsulta = proximoDiaUtil();
        long consultaId = criarConsulta(token, medicoId, pacienteId, dataConsulta);

        // 5. Busca a consulta recém-criada e valida campos (incluindo auditoria)
        mvc.perform(get("/consultas/{id}", consultaId)
                        .header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value((int) consultaId))
                .andExpect(jsonPath("$.status").value("AGENDADA"))
                .andExpect(jsonPath("$.dataAtendimento").value(dataConsulta.toString()))
                .andExpect(jsonPath("$.medico.id").value((int) medicoId))
                .andExpect(jsonPath("$.paciente.id").value((int) pacienteId))
                .andExpect(jsonPath("$.auditoria.createdBy").value("admin"))
                .andExpect(jsonPath("$.auditoria.lastModifiedBy").value("admin"))
                .andExpect(jsonPath("$.auditoria.createdDate").exists())
                .andExpect(jsonPath("$.auditoria.lastModifiedDate").exists());
    }


    private String autenticarAdmin() throws Exception {
        String loginBody = objectMapper.writeValueAsString(new LoginDTO("admin", "testpass"));

        MvcResult result = mvc.perform(post("/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(loginBody))
                .andExpect(status().isOk())
                .andReturn();

        JsonNode body = objectMapper.readTree(result.getResponse().getContentAsString());
        // Aceita tanto a forma nova (accessToken) quanto a antiga (token), para não acoplar ao item 7.
        JsonNode accessToken = body.get("accessToken");
        if (accessToken == null) accessToken = body.get("token");
        return accessToken.asText();
    }

    private long criarMedico(String token, String sufixo) throws Exception {
        Map<String, Object> corpo = new LinkedHashMap<>();
        corpo.put("nome", "Dra. Ana Integração");
        corpo.put("email", "ana.integracao." + sufixo + "@clinica.com");
        corpo.put("crmSigla", "SP");
        corpo.put("crmDigitos", gerarCrmDigitos(sufixo));
        corpo.put("especialidade", "CARDIOLOGIA");
        corpo.put("telefone", "11987654321");

        MvcResult result = mvc.perform(post("/medicos")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(corpo)))
                .andExpect(status().isCreated())
                .andReturn();

        return objectMapper.readTree(result.getResponse().getContentAsString()).get("id").asLong();
    }

    private long criarPaciente(String token, String sufixo) throws Exception {
        Map<String, Object> corpo = new LinkedHashMap<>();
        corpo.put("nome", "João Integração");
        corpo.put("email", "joao.integracao." + sufixo + "@email.com");
        corpo.put("cpf", gerarCpf(sufixo));
        corpo.put("sexo", "MASCULINO");
        corpo.put("dataNascimento", "1990-05-20");
        corpo.put("telefone", "11987654322");

        MvcResult result = mvc.perform(post("/pacientes")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(corpo)))
                .andExpect(status().isCreated())
                .andReturn();

        return objectMapper.readTree(result.getResponse().getContentAsString()).get("id").asLong();
    }

    private long criarConsulta(String token, long medicoId, long pacienteId, LocalDate data) throws Exception {
        Map<String, Object> corpo = new LinkedHashMap<>();
        corpo.put("dataAtendimento", data.toString());
        corpo.put("horarioAtendimento", "10:00");
        corpo.put("duracaoEmMinutos", 30);
        corpo.put("preco", 150.00);
        corpo.put("motivo", "Consulta de integração");
        corpo.put("medicoId", medicoId);
        corpo.put("pacienteId", pacienteId);

        MvcResult result = mvc.perform(post("/consultas")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(corpo)))
                .andExpect(status().isCreated())
                .andReturn();

        return objectMapper.readTree(result.getResponse().getContentAsString()).get("id").asLong();
    }

    private LocalDate proximoDiaUtil() {
        LocalDate data = LocalDate.now().plusDays(7);
        while (data.getDayOfWeek() == DayOfWeek.SATURDAY || data.getDayOfWeek() == DayOfWeek.SUNDAY) {
            data = data.plusDays(1);
        }
        return data;
    }

    // Gera 6 dígitos a partir do sufixo (UUID hex), evitando conflito com o uk_medico_crm
    private String gerarCrmDigitos(String sufixo) {
        long valor = Long.parseLong(sufixo, 16) % 1_000_000L;
        return String.format("%06d", valor);
    }

    // Gera 11 dígitos a partir do sufixo, evitando conflito com o uk_paciente_cpf
    private String gerarCpf(String sufixo) {
        long valor = Long.parseLong(sufixo, 16);
        return String.format("%011d", Math.abs(valor) % 100_000_000_000L);
    }
}
