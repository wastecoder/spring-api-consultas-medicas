package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
class ConsultaRepositoryTest {

    @Autowired
    MedicoRepository medicoRepository;

    @Autowired
    PacienteRepository pacienteRepository;

    @Autowired
    ConsultaRepository consultaRepository;

    private Medico medicoJoao;
    private Medico medicaMaria;
    private Medico medicoInexistente;

    private Paciente pacienteAna;
    private Paciente pacienteInexistente;

    private final Pageable ORDENAR_POR_MAIS_PROXIMO = PageRequest.of(0, 10, Sort.by(Sort.Direction.ASC, "dataAtendimento"));
    private final Sort ORDENAR_POR_MAIS_PROXIMO_SORT = Sort.by("dataAtendimento").ascending();

    @BeforeEach
    void setUp() {
        cadastrarDoisMedicos();
        criarMedicoInexistente();

        cadastrarUmPaciente();
        criarPacienteInexistente();

        cadastrarDuasConsultasAgendadas();
        cadastrarDuasConsultasCanceladasERealizadas();
    }

    private void cadastrarDoisMedicos() {
        List<Medico> medicos = medicoRepository.saveAll(List.of(
                        new Medico("Joao Pedro", "joao.pedro@medexample.com", "11912345678", SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA),
                        new Medico("Maria Luiza", "maria.luiza@medexample.com", "85977889900", SiglaCrm.CE, "654321", Especialidade.ORTOPEDIA)
                )
        );
        medicoJoao = medicos.get(0);
        medicaMaria = medicos.get(1);
    }

    private void criarMedicoInexistente() {
        medicoInexistente = new Medico("Medico Inexistente", "medico.inexistente@fake.com", "000000000", SiglaCrm.SP, "999999", Especialidade.DERMATOLOGIA);
        medicoInexistente.setId(99L);
    }

    private void cadastrarUmPaciente() {
        pacienteAna = pacienteRepository.save(
                new Paciente("Ana Paula", "ana.paula@paciente.com", "21988776655", "98765432100", Sexo.FEMININO, LocalDate.of(1985, 3, 22))
        );
    }

    private void criarPacienteInexistente() {
        pacienteInexistente = new Paciente("Paciente Inexistente", "paciente.inexistente@fake.com", "00000000000", "00000000000", Sexo.FEMININO, LocalDate.of(1999, 9, 9));
        pacienteInexistente.setId(99L);
    }

    private void cadastrarDuasConsultasAgendadas() {
        consultaRepository.saveAll(List.of(
                new Consulta(LocalDate.of(2025, 2, 2), LocalTime.of(9, 0), Duration.ofMinutes(60), new BigDecimal("210.00"), "Avaliação de dor torácica", medicoJoao, pacienteAna),
                new Consulta(LocalDate.of(2025, 1, 1), LocalTime.of(10, 0), Duration.ofMinutes(10), new BigDecimal("195.99"), "Dor no joelho ao caminhar", medicaMaria, pacienteAna)
        ));
    }

    private void cadastrarDuasConsultasCanceladasERealizadas() {
        Consulta realizada1 = new Consulta(LocalDate.of(2024, 6, 6), LocalTime.of(11, 30), Duration.ofMinutes(20), new BigDecimal("150.00"), "Checagem de pressão arterial elevada", medicoJoao, pacienteAna);

        Consulta realizada2 = new Consulta(LocalDate.of(2024, 5, 5), LocalTime.of(12, 0), Duration.ofMinutes(40), new BigDecimal("160.99"), "Avaliação de entorse no tornozelo", medicaMaria, pacienteAna);

        Consulta cancelada1 = new Consulta(LocalDate.of(2024, 4, 4), LocalTime.of(12, 30), Duration.ofMinutes(60), new BigDecimal("150.00"), "Palpitações e arritmia", medicoJoao, pacienteAna);

        Consulta cancelada2 = new Consulta(LocalDate.of(2024, 3, 3), LocalTime.of(13, 0), Duration.ofMinutes(90), new BigDecimal("160.99"), "Suspeita de hérnia de disco", medicaMaria, pacienteAna);

        // Salva inicialmente como AGENDADA (o padrão via @PrePersist)
        List<Consulta> consultas = consultaRepository.saveAll(List.of(realizada1, realizada2, cancelada1, cancelada2));

        // Atualiza status para realizadas e canceladas
        consultas.get(0).setStatus(StatusConsulta.REALIZADA);
        consultas.get(1).setStatus(StatusConsulta.REALIZADA);
        consultas.get(2).setStatus(StatusConsulta.CANCELADA);
        consultas.get(3).setStatus(StatusConsulta.CANCELADA);

        // Salva novamente todas as consultas com status atualizado
        consultaRepository.saveAll(consultas);
    }


    @Nested
    class findByDataAtendimentoBetweenAndStatusTests {
        @Test
        @DisplayName("Deve retornar consultas AGENDADAS entre duas datas")
        void shouldReturnScheduledAppointmentsBetweenDates() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 2, 2);
            StatusConsulta status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO_SORT);

            assertEquals(2, consultas.size());
            assertEquals(LocalDate.of(2025, 1, 1), consultas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2025, 2, 2), consultas.get(1).getDataAtendimento());
            assertTrue(consultas.stream().allMatch(c -> c.getStatus() == status));
        }

        @Test
        @DisplayName("Deve retornar consultas CANCELADAS entre duas datas")
        void shouldReturnCanceledAppointmentsBetweenDates() {
            LocalDate inicio = LocalDate.of(2024, 3, 1);
            LocalDate fim = LocalDate.of(2024, 4, 30);
            StatusConsulta status = StatusConsulta.CANCELADA;

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO_SORT);

            assertEquals(2, consultas.size());
            assertEquals(LocalDate.of(2024, 3, 3), consultas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2024, 4, 4), consultas.get(1).getDataAtendimento());
            assertTrue(consultas.stream().allMatch(c -> c.getStatus() == status));
        }

        @Test
        @DisplayName("Deve retornar consultas REALIZADAS entre duas datas")
        void shouldReturnDoneAppointmentsBetweenDates() {
            LocalDate inicio = LocalDate.of(2024, 5, 1);
            LocalDate fim = LocalDate.of(2024, 6, 30);
            StatusConsulta status = StatusConsulta.REALIZADA;

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO_SORT);

            assertEquals(2, consultas.size());
            assertEquals(LocalDate.of(2024, 5, 5), consultas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2024, 6, 6), consultas.get(1).getDataAtendimento());
            assertTrue(consultas.stream().allMatch(c -> c.getStatus() == status));
        }

        @Test
        @DisplayName("Não deve retornar consultas fora do intervalo de data")
        void shouldNotReturnAppointmentsOutOfDateRange() {
            LocalDate inicio = LocalDate.of(2030, 1, 1);
            LocalDate fim = LocalDate.of(2030, 12, 31);
            StatusConsulta status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO_SORT);

            assertTrue(consultas.isEmpty());
        }
    }

}
