package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface MedicoRepository extends JpaRepository<Medico, Long> {

    // Filtro por ativo
    List<Medico> findByAtivo(Boolean ativo, Sort sort);

    // Filtro combinado: ativo + nome (parcial e ignorando maiúsculas/minúsculas)
    List<Medico> findByNomeContainingIgnoreCaseAndAtivo(String nome, Boolean ativo, Sort sort);

    // Filtro combinado: CRM (sigla + dígitos)
    Optional<Medico> findByCrmSiglaAndCrmDigitos(SiglaCrm crmSigla, String crmDigitos);

    Optional<Medico> findByUsuarioUsernameAndAtivo(String username, Boolean ativo);

    Optional<Medico> findByUsuarioUsername(String username);


    // >>> Relatorios - Grupo: Medicos

    // Retorna a quantidade de médicos agrupados por especialidade, ordenados pelo nome da especialidade
    @Query("""
        SELECT m.especialidade, COUNT(m.id)
        FROM Medico m
        GROUP BY m.especialidade
        ORDER BY m.especialidade
    """)
    List<Object[]> contarMedicosPorEspecialidade();


    // >>> Relatorios - Grupo: Operacional

    // Retorna os médicos que não possuem nenhuma consulta agendada no mês e ano informados
    @Query("""
        SELECT m.id, m.nome, m.especialidade
        FROM Medico m
        WHERE m.id NOT IN (
            SELECT DISTINCT c.medico.id
            FROM Consulta c
            WHERE YEAR(c.dataAtendimento) = :ano AND MONTH(c.dataAtendimento) = :mes
        )
        ORDER BY m.nome ASC
    """)
    List<Object[]> buscarMedicosSemAgendamentoNoMes(int ano, int mes);

}
