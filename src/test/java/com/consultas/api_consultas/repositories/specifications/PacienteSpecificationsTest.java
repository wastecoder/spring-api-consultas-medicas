package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.repositories.PacienteRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@DataJpaTest
@ActiveProfiles("test")
class PacienteSpecificationsTest {

    @Autowired
    PacienteRepository repository;

    private Paciente anaAtiva;
    private Paciente bernardoAtivo;
    private Paciente carlaInativa;

    @BeforeEach
    void setUp() {
        anaAtiva = new Paciente("Ana Lima", "ana@email.com", "11912345678",
                "11111111111", Sexo.FEMININO, LocalDate.of(1990, 1, 1));
        bernardoAtivo = new Paciente("Bernardo Silva", "bernardo@email.com", "21999998888",
                "22222222222", Sexo.MASCULINO, LocalDate.of(1985, 5, 5));
        carlaInativa = new Paciente("Carla Souza", "carla@email.com", "31988887777",
                "33333333333", Sexo.FEMININO, LocalDate.of(2000, 7, 7));
        repository.saveAll(List.of(anaAtiva, bernardoAtivo, carlaInativa));
        carlaInativa.setAtivo(false);
        repository.save(carlaInativa);
    }

    @Test
    @DisplayName("Sem filtros: retorna todos (sem default ativo=true)")
    void semFiltrosRetornaTodos() {
        Specification<Paciente> spec = Specification
                .where(PacienteSpecifications.comAtivo(null))
                .and(PacienteSpecifications.comNomeContendo(null))
                .and(PacienteSpecifications.comCpf(null))
                .and(PacienteSpecifications.comSexo(null));

        assertEquals(3, repository.findAll(spec).size());
    }

    @Test
    @DisplayName("Combinação nome + sexo + ativo (antes ignorava sexo) — agora todas as três aplicam")
    void combinacaoNomeSexoAtivo() {
        Specification<Paciente> spec = Specification
                .where(PacienteSpecifications.comNomeContendo("a"))
                .and(PacienteSpecifications.comSexo(Sexo.FEMININO))
                .and(PacienteSpecifications.comAtivo(true));

        List<Paciente> achados = repository.findAll(spec);
        assertEquals(1, achados.size());
        assertEquals("Ana Lima", achados.get(0).getNome());
    }

    @Test
    @DisplayName("comCpf: filtra pela igualdade exata")
    void comCpfFiltraExato() {
        List<Paciente> achados = repository.findAll(PacienteSpecifications.comCpf("22222222222"));
        assertEquals(1, achados.size());
        assertEquals("Bernardo Silva", achados.get(0).getNome());
    }

    @Test
    @DisplayName("comAtivo(false): só inativos")
    void comAtivoFalseRetornaApenasInativos() {
        List<Paciente> inativos = repository.findAll(PacienteSpecifications.comAtivo(false));
        assertEquals(1, inativos.size());
        assertEquals("Carla Souza", inativos.get(0).getNome());
    }

}
