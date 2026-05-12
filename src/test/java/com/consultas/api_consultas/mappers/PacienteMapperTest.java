package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.PacienteRequisicao;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.dtos.respostas.PacienteRespostaFormatada;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@DisplayName("PacienteMapper")
class PacienteMapperTest {

    private final PacienteMapper mapper = new PacienteMapperImpl();

    @Test
    @DisplayName("paraEntidade: copia campos do request DTO e mantém id/ativo nulos")
    void deveConverterRequisicaoEmEntidade() throws Exception {
        PacienteRequisicao req = new PacienteRequisicao();
        setField(req, "nome", "João");
        setField(req, "email", "joao@email.com");
        setField(req, "cpf", "12345678901");
        setField(req, "sexo", Sexo.MASCULINO);
        setField(req, "dataNascimento", LocalDate.of(1990, 5, 15));
        setField(req, "telefone", "11987654321");

        Paciente paciente = mapper.paraEntidade(req);

        assertNotNull(paciente);
        assertNull(paciente.getId());
        assertNull(paciente.getAtivo());
        assertEquals("João", paciente.getNome());
        assertEquals("12345678901", paciente.getCpf());
        assertEquals(Sexo.MASCULINO, paciente.getSexo());
        assertEquals(LocalDate.of(1990, 5, 15), paciente.getDataNascimento());
    }

    @Test
    @DisplayName("paraResposta: copia campos da entidade e deixa auditoria nula")
    void deveConverterEntidadeEmResposta() {
        Paciente paciente = pacienteSalvo();

        PacienteResposta resposta = mapper.paraResposta(paciente);

        assertEquals(7L, resposta.getId());
        assertEquals("João", resposta.getNome());
        assertEquals("12345678901", resposta.getCpf());
        assertEquals(Sexo.MASCULINO, resposta.getSexo());
        assertNull(resposta.getAuditoria());
    }

    @Test
    @DisplayName("aplicarAtualizacao: copia campos mutáveis preservando id e ativo")
    void deveAplicarAtualizacaoMantendoIdEAtivo() throws Exception {
        Paciente pacienteExistente = pacienteSalvo();

        PacienteRequisicao req = new PacienteRequisicao();
        setField(req, "nome", "João Atualizado");
        setField(req, "email", "joao.novo@email.com");
        setField(req, "cpf", "98765432101");
        setField(req, "sexo", Sexo.FEMININO);
        setField(req, "dataNascimento", LocalDate.of(2000, 1, 1));
        setField(req, "telefone", "21988887777");

        mapper.aplicarAtualizacao(req, pacienteExistente);

        assertEquals(7L, pacienteExistente.getId());
        assertEquals(true, pacienteExistente.getAtivo());
        assertEquals("João Atualizado", pacienteExistente.getNome());
        assertEquals("98765432101", pacienteExistente.getCpf());
        assertEquals(Sexo.FEMININO, pacienteExistente.getSexo());
        assertEquals(LocalDate.of(2000, 1, 1), pacienteExistente.getDataNascimento());
    }

    @Test
    @DisplayName("paraRespostaFormatada: aplica formatadores de CPF, data, telefone e ativo")
    void deveConverterEntidadeEmRespostaFormatada() {
        Paciente paciente = pacienteSalvo();

        PacienteRespostaFormatada formatada = mapper.paraRespostaFormatada(paciente);

        assertEquals(7L, formatada.getId());
        assertEquals("123.456.789-01", formatada.getCpf());
        assertEquals("MASCULINO", formatada.getSexo());
        assertEquals("15/05/1990", formatada.getDataNascimento());
        assertEquals("(11) 98765-4321", formatada.getTelefone());
        assertEquals("ativo", formatada.getAtivo());
    }

    private Paciente pacienteSalvo() {
        Paciente p = new Paciente();
        p.setId(7L);
        p.setNome("João");
        p.setEmail("joao@email.com");
        p.setCpf("12345678901");
        p.setSexo(Sexo.MASCULINO);
        p.setDataNascimento(LocalDate.of(1990, 5, 15));
        p.setTelefone("11987654321");
        p.setAtivo(true);
        return p;
    }

    private void setField(Object target, String name, Object value) throws Exception {
        Field f = target.getClass().getDeclaredField(name);
        f.setAccessible(true);
        f.set(target, value);
    }
}
