package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

@DataJpaTest
@ActiveProfiles("test")
class MedicoRepositoryTest {

    @Autowired
    MedicoRepository medicoRepository;

    @BeforeEach
    void setUp() {
        var medicosCadastrados = cadastrarQuatroMedicos();
        desativarSegundaMetadeDosMedicos(medicosCadastrados);
    }

    private List<Medico> cadastrarQuatroMedicos() {
        return medicoRepository.saveAll(List.of(
                new Medico("Joao Pedro", "joao.pedro@medexample.com", "11912345678", SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA),
                new Medico("Maria Luiza", "maria.luiza@medexample.com", "85977889900", SiglaCrm.CE, "654321", Especialidade.ORTOPEDIA),
                new Medico("Joao Vitor", "joao.vitor@medexample.com", "21999887766", SiglaCrm.RJ, "111222", Especialidade.PEDIATRIA),
                new Medico("Maria Clara", "maria.clara@medexample.com", "31988776655", SiglaCrm.MG, "222333", Especialidade.NEUROLOGIA),
                new Medico("Joao Kleber", "joao.kleber@medexample.com", "71955667788", SiglaCrm.BA, "333444", Especialidade.NEUROLOGIA)
        ));
    }

    private void desativarSegundaMetadeDosMedicos(List<Medico> medicosCadastrados) {
        int metade = medicosCadastrados.size() / 2;

        for (int i = metade; i < medicosCadastrados.size(); i++) {
            medicosCadastrados.get(i).setAtivo(false);
        }

        medicoRepository.saveAll(medicosCadastrados);
    }


    @Nested
    class findByAtivoTests {
        @Test
        @DisplayName("Deve retornar apenas médicos ativos")
        void shouldReturnOnlyActiveDoctors() {
            var medicos = medicoRepository.findByAtivo(true);

            assertEquals(2, medicos.size());
            assertEquals("Joao Pedro", medicos.get(0).getNome());
            assertEquals("Maria Luiza", medicos.get(1).getNome());
            assertTrue(medicos.stream().allMatch(Medico::getAtivo));
        }

        @Test
        @DisplayName("Deve retornar apenas médicos inativos")
        void shouldReturnOnlyInactiveDoctors() {
            var medicos = medicoRepository.findByAtivo(false);

            assertEquals(3, medicos.size());
            assertEquals("Joao Vitor", medicos.get(0).getNome());
            assertEquals("Maria Clara", medicos.get(1).getNome());
            assertEquals("Joao Kleber", medicos.get(2).getNome());
            assertFalse(medicos.stream().allMatch(Medico::getAtivo));
        }
    }

    @Nested
    class findByNomeContainingIgnoreCaseAndAtivoTests {
        @Test
        @DisplayName("Deve retornar as 'Maria' entre os médicos ativos")
        void shouldReturnActiveDoctorsByPartialName() {
            var medicos = medicoRepository.findByNomeContainingIgnoreCaseAndAtivo("Maria", true);

            assertEquals(1, medicos.size());
            assertEquals("Maria Luiza", medicos.get(0).getNome());
            assertTrue(medicos.get(0).getAtivo());
        }

        @Test
        @DisplayName("Deve retornar os 'Joao' entre os médicos inativos")
        void shouldReturnInactiveDoctorsByPartialName() {
            var medicos = medicoRepository.findByNomeContainingIgnoreCaseAndAtivo("Joao", false);

            assertEquals(2, medicos.size());
            assertEquals("Joao Vitor", medicos.get(0).getNome());
            assertEquals("Joao Kleber", medicos.get(1).getNome());
            assertFalse(medicos.stream().allMatch(Medico::getAtivo));
        }
    }

    @Nested
    class findByCrmSiglaAndCrmDigitosAndAtivoTests {
        @Test
        @DisplayName("Deve retornar o médico ativo com CRM 'SP 123456'")
        void shouldReturnActiveDoctorByCrm() {
            Medico medico = medicoRepository
                    .findByCrmSiglaAndCrmDigitosAndAtivo(SiglaCrm.SP, "123456", true)
                    .orElseThrow(() -> new AssertionError("Médico não encontrado"));

            assertEquals(SiglaCrm.SP, medico.getCrmSigla());
            assertEquals("123456", medico.getCrmDigitos());
            assertEquals("Joao Pedro", medico.getNome());
            assertTrue(medico.getAtivo());
        }

        @Test
        @DisplayName("Deve retornar o médico inativo com CRM 'MG 222333'")
        void shouldReturnInactiveDoctorByCrm() {
            Medico medico = medicoRepository
                    .findByCrmSiglaAndCrmDigitosAndAtivo(SiglaCrm.MG, "222333", false)
                    .orElseThrow(() -> new AssertionError("Médico não encontrado"));

            assertEquals(SiglaCrm.MG, medico.getCrmSigla());
            assertEquals("222333", medico.getCrmDigitos());
            assertEquals("Maria Clara", medico.getNome());
            assertFalse(medico.getAtivo());
        }

        @Test
        @DisplayName("Não deve retornar médico ativo com CRM inexistente")
        void shouldNotReturnActiveDoctorByCrm() {
            Optional<Medico> optional = medicoRepository
                    .findByCrmSiglaAndCrmDigitosAndAtivo(SiglaCrm.AC, "000000", true);

            assertTrue(optional.isEmpty());
        }
    }

}
