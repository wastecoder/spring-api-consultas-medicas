package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.dtos.respostas.MedicoRespostaFormatada;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DisplayName("MedicoMapper")
class MedicoMapperTest {

    private final MedicoMapper mapper = new MedicoMapperImpl();

    @Test
    @DisplayName("paraEntidade: copia campos do request DTO e mantém id/ativo nulos")
    void deveConverterRequisicaoEmEntidade() {
        MedicoRequisicao req = new MedicoRequisicao();
        req.setNome("Dra. Ana");
        req.setEmail("ana@clinica.com");
        req.setCrmSigla(SiglaCrm.SP);
        req.setCrmDigitos("123456");
        req.setEspecialidade(Especialidade.CARDIOLOGIA);
        req.setTelefone("11987654321");

        Medico medico = mapper.paraEntidade(req);

        assertNotNull(medico);
        assertNull(medico.getId());
        assertNull(medico.getAtivo());
        assertEquals("Dra. Ana", medico.getNome());
        assertEquals("ana@clinica.com", medico.getEmail());
        assertEquals(SiglaCrm.SP, medico.getCrmSigla());
        assertEquals("123456", medico.getCrmDigitos());
        assertEquals(Especialidade.CARDIOLOGIA, medico.getEspecialidade());
        assertEquals("11987654321", medico.getTelefone());
    }

    @Test
    @DisplayName("paraResposta: copia campos da entidade e deixa auditoria nula")
    void deveConverterEntidadeEmResposta() {
        Medico medico = medicoSalvo();

        MedicoResposta resposta = mapper.paraResposta(medico);

        assertNotNull(resposta);
        assertEquals(10L, resposta.getId());
        assertEquals("Dra. Ana", resposta.getNome());
        assertEquals(SiglaCrm.SP, resposta.getCrmSigla());
        assertEquals(Especialidade.CARDIOLOGIA, resposta.getEspecialidade());
        assertEquals(true, resposta.getAtivo());
        assertNull(resposta.getAuditoria());
    }

    @Test
    @DisplayName("paraRespostaComAuditoria: preenche o sub-objeto de auditoria")
    void deveConverterEntidadeEmRespostaComAuditoria() throws Exception {
        Medico medico = medicoSalvo();
        setAuditoria(medico, "admin", Instant.parse("2025-01-01T10:00:00Z"));

        MedicoResposta resposta = mapper.paraRespostaComAuditoria(medico);

        assertNotNull(resposta.getAuditoria());
        assertEquals("admin", resposta.getAuditoria().createdBy());
        assertEquals(Instant.parse("2025-01-01T10:00:00Z"), resposta.getAuditoria().createdDate());
    }

    @Test
    @DisplayName("aplicarAtualizacao: copia campos mutáveis preservando id, ativo e auditoria")
    void deveAplicarAtualizacaoMantendoIdEAtivo() throws Exception {
        Medico medicoExistente = medicoSalvo();
        setAuditoria(medicoExistente, "admin", Instant.parse("2025-01-01T10:00:00Z"));

        MedicoRequisicao req = new MedicoRequisicao();
        req.setNome("Dra. Ana Atualizada");
        req.setEmail("ana.nova@clinica.com");
        req.setCrmSigla(SiglaCrm.RJ);
        req.setCrmDigitos("654321");
        req.setEspecialidade(Especialidade.NEUROLOGIA);
        req.setTelefone("21999999999");

        mapper.aplicarAtualizacao(req, medicoExistente);

        assertEquals(10L, medicoExistente.getId());
        assertEquals(true, medicoExistente.getAtivo());
        assertEquals("admin", medicoExistente.getCreatedBy());
        assertEquals("Dra. Ana Atualizada", medicoExistente.getNome());
        assertEquals("ana.nova@clinica.com", medicoExistente.getEmail());
        assertEquals(SiglaCrm.RJ, medicoExistente.getCrmSigla());
        assertEquals("654321", medicoExistente.getCrmDigitos());
        assertEquals(Especialidade.NEUROLOGIA, medicoExistente.getEspecialidade());
        assertEquals("21999999999", medicoExistente.getTelefone());
    }

    @Test
    @DisplayName("paraRespostaFormatada: aplica os formatadores de CRM, telefone e ativo")
    void deveConverterEntidadeEmRespostaFormatada() {
        Medico medico = medicoSalvo();

        MedicoRespostaFormatada formatada = mapper.paraRespostaFormatada(medico);

        assertEquals(10L, formatada.getId());
        assertEquals("Dra. Ana", formatada.getNome());
        assertEquals("CRM/SP 123456", formatada.getCrm());
        assertEquals("CARDIOLOGIA", formatada.getEspecialidade());
        assertEquals("(11) 98765-4321", formatada.getTelefone());
        assertEquals("ativo", formatada.getAtivo());
    }

    private Medico medicoSalvo() {
        Medico m = new Medico();
        m.setId(10L);
        m.setNome("Dra. Ana");
        m.setEmail("ana@clinica.com");
        m.setCrmSigla(SiglaCrm.SP);
        m.setCrmDigitos("123456");
        m.setEspecialidade(Especialidade.CARDIOLOGIA);
        m.setTelefone("11987654321");
        m.setAtivo(true);
        return m;
    }

    private void setAuditoria(Object entidade, String createdBy, Instant createdDate) throws Exception {
        Class<?> auditClass = entidade.getClass().getSuperclass().getSuperclass();
        Field createdByField = auditClass.getDeclaredField("createdBy");
        createdByField.setAccessible(true);
        createdByField.set(entidade, createdBy);

        Field createdDateField = auditClass.getDeclaredField("createdDate");
        createdDateField.setAccessible(true);
        createdDateField.set(entidade, createdDate);
        assertTrue(true);
    }
}
