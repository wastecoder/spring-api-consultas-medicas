package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Consulta;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ConsultaRepository extends JpaRepository<Consulta, Long> {

    // Busca todas as consultas carregando médico e paciente juntos (evita LazyInitializationException)
    // Necessário porque os relacionamentos de Consulta são LAZY e causavam erro ao serem acessados
    @Query("SELECT c FROM Consulta c JOIN FETCH c.medico JOIN FETCH c.paciente")
    List<Consulta> findAllWithMedicoAndPaciente();

    // Mesmo motivo da query acima, mas buscando um ID
    @Query("SELECT c FROM Consulta c JOIN FETCH c.medico JOIN FETCH c.paciente WHERE c.id = :id")
    Optional<Consulta> findByIdWithMedicoAndPaciente(@Param("id") Long id);


}
