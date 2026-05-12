package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.repositories.PacienteRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@DataJpaTest
@ActiveProfiles("test")
class ConsultaSpecificationsTest {

    @Autowired ConsultaRepository consultaRepository;
    @Autowired MedicoRepository medicoRepository;
    @Autowired PacienteRepository pacienteRepository;

    private Medico medicoJoao;
    private Medico medicaMaria;
    private Paciente pacienteAna;
    private Paciente pacienteBeto;

    @BeforeEach
    void setUp() {
        medicoJoao = medicoRepository.save(new Medico("Joao", "joao@clinica.com",
                "11912345678", SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA));
        medicaMaria = medicoRepository.save(new Medico("Maria", "maria@clinica.com",
                "11999998888", SiglaCrm.SP, "654321", Especialidade.NEUROLOGIA));
        pacienteAna = pacienteRepository.save(new Paciente("Ana", "ana@email.com",
                "21988887777", "11111111111", Sexo.FEMININO, LocalDate.of(1990, 1, 1)));
        pacienteBeto = pacienteRepository.save(new Paciente("Beto", "beto@email.com",
                "21977776666", "22222222222", Sexo.MASCULINO, LocalDate.of(1985, 5, 5)));

        // Agendada de Joao para Ana
        consultaRepository.save(new Consulta(LocalDate.of(2026, 6, 10), LocalTime.of(9, 0),
                Duration.ofMinutes(30), new BigDecimal("150.00"), "rotina", medicoJoao, pacienteAna));
        // Agendada de Maria para Beto
        consultaRepository.save(new Consulta(LocalDate.of(2026, 6, 11), LocalTime.of(10, 0),
                Duration.ofMinutes(30), new BigDecimal("150.00"), "rotina", medicaMaria, pacienteBeto));
        // Realizada de Joao para Beto (status alterado após persist)
        Consulta realizada = consultaRepository.save(new Consulta(LocalDate.of(2025, 5, 5),
                LocalTime.of(11, 0), Duration.ofMinutes(30), new BigDecimal("150.00"),
                "retorno", medicoJoao, pacienteBeto));
        realizada.setStatus(StatusConsulta.REALIZADA);
        consultaRepository.save(realizada);
    }

    @Test
    @DisplayName("Sem filtros: retorna todas (sem default AGENDADA)")
    void semFiltrosRetornaTodas() {
        Specification<Consulta> spec = Specification
                .where(ConsultaSpecifications.comStatus(null))
                .and(ConsultaSpecifications.comMedicoId(null))
                .and(ConsultaSpecifications.comPacienteId(null))
                .and(ConsultaSpecifications.comDataAtendimento(null));

        assertEquals(3, consultaRepository.findAll(spec).size());
    }

    @Test
    @DisplayName("comStatus(REALIZADA): filtra só realizadas")
    void filtraPorStatus() {
        List<Consulta> realizadas = consultaRepository.findAll(
                ConsultaSpecifications.comStatus(StatusConsulta.REALIZADA));
        assertEquals(1, realizadas.size());
    }

    @Test
    @DisplayName("Combinação medico + paciente + status")
    void combinaMedicoPacienteStatus() {
        Specification<Consulta> spec = Specification
                .where(ConsultaSpecifications.comMedicoId(medicoJoao.getId()))
                .and(ConsultaSpecifications.comPacienteId(pacienteBeto.getId()))
                .and(ConsultaSpecifications.comStatus(StatusConsulta.REALIZADA));

        List<Consulta> achadas = consultaRepository.findAll(spec);
        assertEquals(1, achadas.size());
        assertEquals(StatusConsulta.REALIZADA, achadas.get(0).getStatus());
    }

    @Test
    @DisplayName("comDataAtendimento isolado: combina sem o status (antes ignorava)")
    void filtraSomentePorData() {
        List<Consulta> achadas = consultaRepository.findAll(
                ConsultaSpecifications.comDataAtendimento(LocalDate.of(2025, 5, 5)));
        assertEquals(1, achadas.size());
        assertEquals(StatusConsulta.REALIZADA, achadas.get(0).getStatus());
    }

}
