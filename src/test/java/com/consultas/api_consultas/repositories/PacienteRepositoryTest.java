package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

@DataJpaTest
@ActiveProfiles("test")
class PacienteRepositoryTest {

    @Autowired
    private PacienteRepository pacienteRepository;

    private final Sort ORDENAR_POR_NOME = Sort.by("nome").ascending();

    @BeforeEach
    void setUp() {
        var medicosCadastrados = cadastrarCincoPacientes();
        desativarSegundaMetadeDosPacientes(medicosCadastrados);
    }

    private List<Paciente> cadastrarCincoPacientes() {
        return pacienteRepository.saveAll(List.of(
                new Paciente("Ana Laura", "ana.laura@paciente.com", "11999887766", "12345678901", Sexo.FEMININO, LocalDate.of(1990, 8, 15)),
                new Paciente("Matheus Henrique", "matheus.henrique@paciente.com", "21988776655", "23456789012", Sexo.MASCULINO, LocalDate.of(1988, 4, 10)),
                new Paciente("Matheus Eduardo", "matheus.eduardo@paciente.com", "41966554433", "45678901234", Sexo.MASCULINO, LocalDate.of(1995, 7, 25)),
                new Paciente("Ana Paula", "ana.paula@paciente.com", "51955443322", "56789012345", Sexo.FEMININO, LocalDate.of(1982, 2, 14)),
                new Paciente("Ana Clara", "ana.clara@paciente.com", "31977665544", "34567890123", Sexo.FEMININO, LocalDate.of(1975, 11, 3))
        ));
    }

    private void desativarSegundaMetadeDosPacientes(List<Paciente> pacientesCadastrados) {
        int metade = pacientesCadastrados.size() / 2;

        for (int i = metade; i < pacientesCadastrados.size(); i++) {
            pacientesCadastrados.get(i).setAtivo(false);
        }

        pacienteRepository.saveAll(pacientesCadastrados);
    }


    @Nested
    class findByAtivoTests {
        @Test
        @DisplayName("Deve retornar apenas pacientes ativos")
        void shouldReturnOnlyActivePatients() {
            var pacientes = pacienteRepository.findByAtivo(true, ORDENAR_POR_NOME);

            assertEquals(2, pacientes.size());
            assertEquals("Ana Laura", pacientes.get(0).getNome());
            assertEquals("Matheus Henrique", pacientes.get(1).getNome());
            assertTrue(pacientes.stream().allMatch(Paciente::getAtivo));
        }

        @Test
        @DisplayName("Deve retornar apenas pacientes inativos")
        void shouldReturnOnlyInactivePatients() {
            var pacientes = pacienteRepository.findByAtivo(false, ORDENAR_POR_NOME);

            assertEquals(3, pacientes.size());
            assertEquals("Ana Clara", pacientes.get(0).getNome());
            assertEquals("Ana Paula", pacientes.get(1).getNome());
            assertEquals("Matheus Eduardo", pacientes.get(2).getNome());
            assertFalse(pacientes.stream().allMatch(Paciente::getAtivo));
        }
    }

    @Nested
    class findByCpfTests {
        @Test
        @DisplayName("Deve retornar o paciente com CPF '12345678901'")
        void shouldReturnPatientByCpf() {
            Paciente paciente = pacienteRepository
                    .findByCpf("12345678901")
                    .orElseThrow(() -> new AssertionError("Paciente nao encontrado"));

            assertEquals("Ana Laura", paciente.getNome());
            assertEquals("12345678901", paciente.getCpf());
        }

        @Test
        @DisplayName("Não deve retornar paciente com CPF inexistente")
        void shouldNotReturnPatientByCpf() {
            Optional<Paciente> optional = pacienteRepository
                    .findByCpf("00000000000");

            assertTrue(optional.isEmpty());
        }
    }

    @Nested
    class findByNomeContainingIgnoreCaseAndAtivoTests {
        @Test
        @DisplayName("Deve retornar os 'Matheus' entre os pacientes ativos")
        void shouldReturnActivePatientsByPartialName() {
            var pacientes = pacienteRepository.findByNomeContainingIgnoreCaseAndAtivo("maTHEUS", true, ORDENAR_POR_NOME);

            assertEquals(1, pacientes.size());
            assertEquals("Matheus Henrique", pacientes.get(0).getNome());
            assertTrue(pacientes.get(0).getAtivo());
        }

        @Test
        @DisplayName("Deve retornar as 'Ana' entre os pacientes inativos")
        void shouldReturnInactivePatientsByPartialName() {
            var pacientes = pacienteRepository.findByNomeContainingIgnoreCaseAndAtivo("aNA", false, ORDENAR_POR_NOME);

            assertEquals(2, pacientes.size());
            assertEquals("Ana Clara", pacientes.get(0).getNome());
            assertEquals("Ana Paula", pacientes.get(1).getNome());
            assertFalse(pacientes.stream().allMatch(Paciente::getAtivo));
        }
    }

    @Nested
    class findBySexoAndAtivoTests {
        @Test
        @DisplayName("Deve retornar os pacientes masculinos ativos")
        void shouldReturnActivePatientsBySex() {
            var pacientes = pacienteRepository.findBySexoAndAtivo(Sexo.MASCULINO, true, ORDENAR_POR_NOME);

            assertEquals(1, pacientes.size());
            assertEquals("Matheus Henrique", pacientes.get(0).getNome());
            assertTrue(pacientes.get(0).getAtivo());
        }

        @Test
        @DisplayName("Deve retornar os pacientes femininos inativos")
        void shouldReturnInactivePatientsBySex() {
            var pacientes = pacienteRepository.findBySexoAndAtivo(Sexo.FEMININO, false, ORDENAR_POR_NOME);

            assertEquals(2, pacientes.size());
            assertEquals("Ana Clara", pacientes.get(0).getNome());
            assertEquals("Ana Paula", pacientes.get(1).getNome());
            assertFalse(pacientes.stream().allMatch(Paciente::getAtivo));
        }
    }

}
