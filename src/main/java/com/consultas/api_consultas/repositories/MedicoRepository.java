package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MedicoRepository extends JpaRepository<Medico, Long> {
    List<Medico> findByAtivo(Boolean ativo);
    List<Medico> findByAtivoTrue();
}
