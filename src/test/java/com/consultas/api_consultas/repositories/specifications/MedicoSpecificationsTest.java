package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.repositories.MedicoRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
class MedicoSpecificationsTest {

    @Autowired
    MedicoRepository repository;

    private Medico joaoAtivo;
    private Medico mariaAtiva;
    private Medico carlosInativo;

    @BeforeEach
    void setUp() {
        joaoAtivo = new Medico("Joao Pedro", "joao@clinica.com", "11912345678",
                SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA);
        mariaAtiva = new Medico("Maria Souza", "maria@clinica.com", "11999998888",
                SiglaCrm.SP, "654321", Especialidade.NEUROLOGIA);
        carlosInativo = new Medico("Carlos Lima", "carlos@clinica.com", "21988887777",
                SiglaCrm.RJ, "111222", Especialidade.PEDIATRIA);
        repository.saveAll(List.of(joaoAtivo, mariaAtiva, carlosInativo));
        carlosInativo.setAtivo(false);
        repository.save(carlosInativo);
    }

    @Test
    @DisplayName("Specification null em todos os filtros: retorna todos")
    void semFiltrosRetornaTodos() {
        Specification<Medico> spec = Specification
                .where(MedicoSpecifications.comAtivo(null))
                .and(MedicoSpecifications.comNomeContendo(null))
                .and(MedicoSpecifications.comCrm(null, null));

        assertEquals(3, repository.findAll(spec).size());
    }

    @Test
    @DisplayName("comAtivo(true): só ativos")
    void comAtivoTrueRetornaApenasAtivos() {
        List<Medico> ativos = repository.findAll(MedicoSpecifications.comAtivo(true));
        assertEquals(2, ativos.size());
        assertTrue(ativos.stream().allMatch(Medico::getAtivo));
    }

    @Test
    @DisplayName("comNomeContendo: busca case-insensitive parcial")
    void comNomeContendoCaseInsensitive() {
        List<Medico> achados = repository.findAll(MedicoSpecifications.comNomeContendo("maria"));
        assertEquals(1, achados.size());
        assertEquals("Maria Souza", achados.get(0).getNome());
    }

    @Test
    @DisplayName("comCrm: combina nome + sexo (e antes era ignorado)")
    void comCrmCompleto() {
        List<Medico> achados = repository.findAll(MedicoSpecifications.comCrm(SiglaCrm.SP, "123456"));
        assertEquals(1, achados.size());
        assertEquals("Joao Pedro", achados.get(0).getNome());
    }

    @Test
    @DisplayName("comCrm: só sigla, sem dígitos, filtra pela sigla")
    void comCrmApenasSigla() {
        List<Medico> achados = repository.findAll(MedicoSpecifications.comCrm(SiglaCrm.SP, null));
        assertEquals(2, achados.size());
    }

    @Test
    @DisplayName("Combinação completa: nome + ativo + sigla CRM")
    void combinacaoNomeAtivoCrm() {
        Specification<Medico> spec = Specification
                .where(MedicoSpecifications.comAtivo(true))
                .and(MedicoSpecifications.comNomeContendo("joao"))
                .and(MedicoSpecifications.comCrm(SiglaCrm.SP, null));

        List<Medico> achados = repository.findAll(spec);
        assertEquals(1, achados.size());
        assertEquals("Joao Pedro", achados.get(0).getNome());
    }

}
