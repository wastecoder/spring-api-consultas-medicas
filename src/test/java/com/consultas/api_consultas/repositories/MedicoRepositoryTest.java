package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
class MedicoRepositoryTest {

    @Autowired
    MedicoRepository medicoRepository;

    private final Sort ORDENAR_POR_NOME = Sort.by("nome").ascending();

    @BeforeEach
    void setUp() {
        var medicosCadastrados = cadastrarCincoMedicos();
        desativarSegundaMetadeDosMedicos(medicosCadastrados);
    }

    private List<Medico> cadastrarCincoMedicos() {
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
            var pageable = PageRequest.of(0, 10, ORDENAR_POR_NOME);
            var medicosPage = medicoRepository.findByAtivo(true, pageable);

            assertEquals(2, medicosPage.getTotalElements());
            assertEquals("Joao Pedro", medicosPage.getContent().get(0).getNome());
            assertEquals("Maria Luiza", medicosPage.getContent().get(1).getNome());
            assertTrue(medicosPage.getContent().stream().allMatch(Medico::getAtivo));
        }

        @Test
        @DisplayName("Deve retornar apenas médicos inativos")
        void shouldReturnOnlyInactiveDoctors() {
            var pageable = PageRequest.of(0, 10, ORDENAR_POR_NOME);
            var medicosPage = medicoRepository.findByAtivo(false, pageable);

            assertEquals(3, medicosPage.getTotalElements());
            assertEquals("Joao Kleber", medicosPage.getContent().get(0).getNome());
            assertEquals("Joao Vitor", medicosPage.getContent().get(1).getNome());
            assertEquals("Maria Clara", medicosPage.getContent().get(2).getNome());
            assertFalse(medicosPage.getContent().stream().allMatch(Medico::getAtivo));
        }
    }

    @Nested
    class findByNomeContainingIgnoreCaseAndAtivoTests {
        @Test
        @DisplayName("Deve retornar as 'Maria' entre os médicos ativos")
        void shouldReturnActiveDoctorsByPartialName() {
            var pageable = PageRequest.of(0, 10, ORDENAR_POR_NOME);
            var medicosPage = medicoRepository.findByNomeContainingIgnoreCaseAndAtivo("Maria", true, pageable);

            assertEquals(1, medicosPage.getTotalElements());
            assertEquals("Maria Luiza", medicosPage.getContent().get(0).getNome());
            assertTrue(medicosPage.getContent().get(0).getAtivo());
        }

        @Test
        @DisplayName("Deve retornar os 'Joao' entre os médicos inativos")
        void shouldReturnInactiveDoctorsByPartialName() {
            var pageable = PageRequest.of(0, 10, ORDENAR_POR_NOME);
            var medicosPage = medicoRepository.findByNomeContainingIgnoreCaseAndAtivo("Joao", false, pageable);

            assertEquals(2, medicosPage.getTotalElements());
            assertEquals("Joao Kleber", medicosPage.getContent().get(0).getNome());
            assertEquals("Joao Vitor", medicosPage.getContent().get(1).getNome());
            assertFalse(medicosPage.getContent().stream().allMatch(Medico::getAtivo));
        }
    }

    @Nested
    class findByCrmSiglaAndCrmDigitosAndAtivoTests {
        @Test
        @DisplayName("Deve retornar o médico ativo com CRM 'SP 123456'")
        void shouldReturnActiveDoctorByCrm() {
            Medico medico = medicoRepository
                    .findByCrmSiglaAndCrmDigitos(SiglaCrm.SP, "123456")
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
                    .findByCrmSiglaAndCrmDigitos(SiglaCrm.MG, "222333")
                    .orElseThrow(() -> new AssertionError("Médico não encontrado"));

            assertEquals(SiglaCrm.MG, medico.getCrmSigla());
            assertEquals("222333", medico.getCrmDigitos());
            assertEquals("Maria Clara", medico.getNome());
            assertFalse(medico.getAtivo());
        }

        @Test
        @DisplayName("Não deve retornar médico com CRM inexistente")
        void shouldNotReturnActiveDoctorByCrm() {
            Optional<Medico> optional = medicoRepository
                    .findByCrmSiglaAndCrmDigitos(SiglaCrm.AC, "000000");

            assertTrue(optional.isEmpty());
        }
    }

}
