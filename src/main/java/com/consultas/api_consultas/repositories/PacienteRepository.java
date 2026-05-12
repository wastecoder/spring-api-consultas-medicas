package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Paciente;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface PacienteRepository extends JpaRepository<Paciente, Long>, JpaSpecificationExecutor<Paciente> {

    Optional<Paciente> findByUsuarioUsernameAndAtivo(String username, Boolean ativo);

    Optional<Paciente> findByUsuarioUsername(String username);


    // >>> Relatorios - Grupo: Pacientes

    // Retorna a quantidade de pacientes agrupados por sexo
    @Query("""
        SELECT p.sexo, COUNT(p.id)
        FROM Paciente p
        GROUP BY p.sexo
        ORDER BY p.sexo DESC
    """)
    List<Object[]> distribuicaoPorSexo();

    // Retorna a quantidade de pacientes agrupados por faixa etária
    @Query("""
        SELECT
            CASE
                WHEN TIMESTAMPDIFF(YEAR, p.dataNascimento, CURRENT_DATE) < 18 THEN 'Menor de 18'
                WHEN TIMESTAMPDIFF(YEAR, p.dataNascimento, CURRENT_DATE) BETWEEN 18 AND 29 THEN '18-29'
                WHEN TIMESTAMPDIFF(YEAR, p.dataNascimento, CURRENT_DATE) BETWEEN 30 AND 44 THEN '30-44'
                WHEN TIMESTAMPDIFF(YEAR, p.dataNascimento, CURRENT_DATE) BETWEEN 45 AND 59 THEN '45-59'
                ELSE 'Maior de 60'
            END,
            COUNT(p.id)
        FROM Paciente p
        GROUP BY 1
    """)
    List<Object[]> distribuicaoPorFaixaEtaria();

}
