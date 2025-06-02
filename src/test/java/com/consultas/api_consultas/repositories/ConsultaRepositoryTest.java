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
import org.springframework.data.domain.Sort;
import org.springframework.test.context.ActiveProfiles;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

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

    private final Sort ORDENAR_POR_MAIS_PROXIMO = Sort.by("dataAtendimento").ascending();

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
    class findByStatusTests {
        @Test
        @DisplayName("Deve retornar apenas consultas agendadas")
        void shouldReturnScheduledStatus() {
            var consultasAgendadas = consultaRepository.findByStatus(StatusConsulta.AGENDADA, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(2, consultasAgendadas.size());
            assertEquals(LocalDate.of(2025, 1, 1), consultasAgendadas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2025, 2, 2), consultasAgendadas.get(1).getDataAtendimento());
            assertTrue(consultasAgendadas.get(0).getDataAtendimento().isBefore(consultasAgendadas.get(1).getDataAtendimento()));
            assertTrue(consultasAgendadas.stream().allMatch(c -> c.getStatus() == StatusConsulta.AGENDADA));
        }

        @Test
        @DisplayName("Deve retornar apenas consultas realizadas")
        void shouldReturnDoneStatus() {
            var consultasRealizadas = consultaRepository.findByStatus(StatusConsulta.REALIZADA, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(2, consultasRealizadas.size());
            assertEquals(LocalDate.of(2024, 5, 5), consultasRealizadas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2024, 6, 6), consultasRealizadas.get(1).getDataAtendimento());
            assertTrue(consultasRealizadas.get(0).getDataAtendimento().isBefore(consultasRealizadas.get(1).getDataAtendimento()));
            assertTrue(consultasRealizadas.stream().allMatch(c -> c.getStatus() == StatusConsulta.REALIZADA));
        }

        @Test
        @DisplayName("Deve retornar apenas consultas canceladas")
        void shouldReturnCanceledStatus() {
            var consultasCanceladas = consultaRepository.findByStatus(StatusConsulta.CANCELADA, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(2, consultasCanceladas.size());
            assertEquals(LocalDate.of(2024, 3, 3), consultasCanceladas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2024, 4, 4), consultasCanceladas.get(1).getDataAtendimento());
            assertTrue(consultasCanceladas.get(0).getDataAtendimento().isBefore(consultasCanceladas.get(1).getDataAtendimento()));
            assertTrue(consultasCanceladas.stream().allMatch(c -> c.getStatus() == StatusConsulta.CANCELADA));
        }
    }

    @Nested
    class findByMedicoAndStatusTests {
        @Test
        @DisplayName("Deve retornar consultas por medico e status")
        void shouldReturnAppointmentsByDoctorsAndStatus() {
            var status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByMedicoAndStatus(medicoJoao, status, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(1, consultas.size());
            assertEquals(LocalDate.of(2025, 2, 2), consultas.get(0).getDataAtendimento());
            assertEquals(medicoJoao, consultas.get(0).getMedico());
            assertEquals(status, consultas.get(0).getStatus());
        }

        @Test
        @DisplayName("Não deve retornar consulta com medico inexistente")
        void shouldNotReturnAppointByInexistenceDoctor() {
            var consultas = consultaRepository.findByMedicoAndStatus(medicoInexistente, StatusConsulta.CANCELADA, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }
    }

    @Nested
    class findByPacienteAndStatusTests {
        @Test
        @DisplayName("Deve retornar consultas por paciente e status")
        void shouldReturnAppointmentsByPatientsAndStatus() {
            var status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByPacienteAndStatus(pacienteAna, status, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(2, consultas.size());
            assertEquals(LocalDate.of(2025, 1, 1), consultas.get(0).getDataAtendimento());
            assertEquals(LocalDate.of(2025, 2, 2), consultas.get(1).getDataAtendimento());
            assertTrue(consultas.get(0).getDataAtendimento().isBefore(consultas.get(1).getDataAtendimento()));
            assertEquals(pacienteAna, consultas.get(0).getPaciente());
            assertEquals(status, consultas.get(0).getStatus());
        }

        @Test
        @DisplayName("Não deve retornar consulta com paciente inexistente")
        void shouldNotReturnAppointByInexistencePatient() {
            var consultas = consultaRepository.findByPacienteAndStatus(pacienteInexistente, StatusConsulta.AGENDADA, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }
    }

    @Nested
    class findByMedicoAndPacienteAndStatusTests {
        @Test
        @DisplayName("Deve retornar consultas por medico, paciente e status")
        void shouldReturnAppointmentsByDoctorPatientAndStatus() {
            var status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByMedicoAndPacienteAndStatus(medicoJoao, pacienteAna, status, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(1, consultas.size());
            assertEquals(LocalDate.of(2025, 2, 2), consultas.get(0).getDataAtendimento());
            assertEquals(medicoJoao, consultas.get(0).getMedico());
            assertEquals(pacienteAna, consultas.get(0).getPaciente());
            assertEquals(status, consultas.get(0).getStatus());
        }

        @Test
        @DisplayName("Não deve retornar consulta com medico inexistente")
        void shouldNotReturnAppointByInexistenceDoctor() {
            var consultas = consultaRepository.findByMedicoAndPacienteAndStatus(medicoInexistente, pacienteAna, StatusConsulta.AGENDADA, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }

        @Test
        @DisplayName("Não deve retornar consulta com paciente inexistente")
        void shouldNotReturnAppointByInexistencePatient() {
            var consultas = consultaRepository.findByMedicoAndPacienteAndStatus(medicoJoao, pacienteInexistente, StatusConsulta.AGENDADA, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }

        @Test
        @DisplayName("Não deve retornar consulta com medico e paciente inexistentes")
        void shouldNotReturnAppointByInexistenceMedicoEPaciente() {
            var consultas = consultaRepository.findByMedicoAndPacienteAndStatus(medicoInexistente, pacienteInexistente, StatusConsulta.AGENDADA, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }
    }

    @Nested
    class findByDataAtendimentoAndStatusTests {
        @Test
        @DisplayName("Deve retornar consultas por data de atendimento e status")
        void shouldReturnAppointmentsByDateAndStatus() {
            var data = LocalDate.of(2025, 1, 1);
            var status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByDataAtendimentoAndStatus(data, status, ORDENAR_POR_MAIS_PROXIMO);

            assertEquals(1, consultas.size());
            assertEquals(data, consultas.get(0).getDataAtendimento());
            assertEquals(status, consultas.get(0).getStatus());
        }

        @Test
        @DisplayName("Não deve retornar consultas para data e status inexistentes")
        void shouldNotReturnAppointmentsForNonexistentDateAndStatus() {
            var dataInexistente = LocalDate.of(1500, 1, 1);
            var status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByDataAtendimentoAndStatus(dataInexistente, status, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }
    }

    @Nested
    class findByDataAtendimentoBetweenAndStatusTests {
        @Test
        @DisplayName("Deve retornar consultas AGENDADAS entre duas datas")
        void shouldReturnScheduledAppointmentsBetweenDates() {
            LocalDate inicio = LocalDate.of(2025, 1, 1);
            LocalDate fim = LocalDate.of(2025, 2, 2);
            StatusConsulta status = StatusConsulta.AGENDADA;

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO);

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

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO);

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

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO);

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

            var consultas = consultaRepository.findByDataAtendimentoBetweenAndStatus(inicio, fim, status, ORDENAR_POR_MAIS_PROXIMO);

            assertTrue(consultas.isEmpty());
        }
    }

}
