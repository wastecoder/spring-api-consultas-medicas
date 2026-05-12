package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaCadastroDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.dtos.respostas.ConsultaRespostaFormatada;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@DisplayName("ConsultaMapper")
class ConsultaMapperTest {

    private final ConsultaMapper mapper = new ConsultaMapperImpl();

    @Test
    @DisplayName("paraEntidade(ConsultaCadastroDto): converte 'HH:mm' -> LocalTime, Integer -> Duration e ids -> stubs")
    void deveConverterCadastroEmEntidade() throws Exception {
        ConsultaCadastroDto dto = new ConsultaCadastroDto();
        setField(dto, "dataAtendimento", LocalDate.of(2026, 6, 10));
        setField(dto, "horarioAtendimento", "14:30");
        setField(dto, "duracaoEmMinutos", 60);
        setField(dto, "preco", new BigDecimal("150.00"));
        setField(dto, "motivo", "Consulta de rotina");
        setField(dto, "medicoId", 1L);
        setField(dto, "pacienteId", 2L);

        Consulta consulta = mapper.paraEntidade(dto);

        assertNotNull(consulta);
        assertEquals(LocalDate.of(2026, 6, 10), consulta.getDataAtendimento());
        assertEquals(LocalTime.of(14, 30), consulta.getHorarioAtendimento());
        assertEquals(Duration.ofMinutes(60), consulta.getDuracaoEmMinutos());
        assertEquals(new BigDecimal("150.00"), consulta.getPreco());
        assertEquals("Consulta de rotina", consulta.getMotivo());
        assertEquals(1L, consulta.getMedico().getId());
        assertEquals(2L, consulta.getPaciente().getId());
    }

    @Test
    @DisplayName("paraResposta: converte Duration em Long (minutos) e Medico/Paciente em PessoaResumo")
    void deveConverterEntidadeEmResposta() {
        Consulta consulta = consultaSalva();

        ConsultaResposta resposta = mapper.paraResposta(consulta);

        assertEquals(99L, resposta.getId());
        assertEquals(60L, resposta.getDuracaoEmMinutos());
        assertEquals(StatusConsulta.AGENDADA, resposta.getStatus());
        assertEquals(1L, resposta.getMedico().getId());
        assertEquals("Dra. Ana", resposta.getMedico().getNome());
        assertEquals(2L, resposta.getPaciente().getId());
        assertEquals("João", resposta.getPaciente().getNome());
    }

    @Test
    @DisplayName("aplicarAtualizacao: aplica campos do DTO mantendo id, dataAgendamento e auditoria")
    void deveAplicarAtualizacaoMantendoIdEDataAgendamento() throws Exception {
        Consulta existente = consultaSalva();
        LocalDateTime agendamentoOriginal = existente.getDataAgendamento();

        ConsultaAtualizacaoDto dto = new ConsultaAtualizacaoDto();
        setField(dto, "dataAtendimento", LocalDate.of(2026, 7, 15));
        setField(dto, "horarioAtendimento", "09:00");
        setField(dto, "duracaoEmMinutos", 45);
        setField(dto, "preco", new BigDecimal("200.00"));
        setField(dto, "motivo", "Retorno");
        setField(dto, "status", StatusConsulta.REALIZADA);
        setField(dto, "medicoId", 3L);
        setField(dto, "pacienteId", 4L);

        mapper.aplicarAtualizacao(dto, existente);

        assertEquals(99L, existente.getId());
        assertEquals(agendamentoOriginal, existente.getDataAgendamento());
        assertEquals(LocalDate.of(2026, 7, 15), existente.getDataAtendimento());
        assertEquals(LocalTime.of(9, 0), existente.getHorarioAtendimento());
        assertEquals(Duration.ofMinutes(45), existente.getDuracaoEmMinutos());
        assertEquals(new BigDecimal("200.00"), existente.getPreco());
        assertEquals("Retorno", existente.getMotivo());
        assertEquals(StatusConsulta.REALIZADA, existente.getStatus());
        assertEquals(3L, existente.getMedico().getId());
        assertEquals(4L, existente.getPaciente().getId());
    }

    @Test
    @DisplayName("paraRespostaFormatada: formata data, hora, dataAgendamento e preço")
    void deveConverterEntidadeEmRespostaFormatada() {
        Consulta consulta = consultaSalva();

        ConsultaRespostaFormatada formatada = mapper.paraRespostaFormatada(consulta);

        assertEquals(99L, formatada.getId());
        assertEquals("10/06/2026", formatada.getDataAtendimento());
        assertEquals("14:30", formatada.getHorarioAtendimento());
        assertEquals(60L, formatada.getDuracaoEmMinutos());
        assertEquals("10/06/2026 09:00", formatada.getDataAgendamento());
        // preço formatado contém símbolo de moeda e o valor (depende da locale)
        assertNotNull(formatada.getPreco());
        assertEquals(StatusConsulta.AGENDADA, formatada.getStatus());
    }

    private Consulta consultaSalva() {
        Medico medico = new Medico();
        medico.setId(1L);
        medico.setNome("Dra. Ana");

        Paciente paciente = new Paciente();
        paciente.setId(2L);
        paciente.setNome("João");

        Consulta c = new Consulta();
        c.setId(99L);
        c.setDataAtendimento(LocalDate.of(2026, 6, 10));
        c.setHorarioAtendimento(LocalTime.of(14, 30));
        c.setDuracaoEmMinutos(Duration.ofMinutes(60));
        c.setDataAgendamento(LocalDateTime.of(2026, 6, 10, 9, 0));
        c.setPreco(new BigDecimal("150.00"));
        c.setMotivo("Consulta de rotina");
        c.setStatus(StatusConsulta.AGENDADA);
        c.setMedico(medico);
        c.setPaciente(paciente);
        return c;
    }

    private void setField(Object target, String name, Object value) throws Exception {
        Field f = target.getClass().getDeclaredField(name);
        f.setAccessible(true);
        f.set(target, value);
    }
}
