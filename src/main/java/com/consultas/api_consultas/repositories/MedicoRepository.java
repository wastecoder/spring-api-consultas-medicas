package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
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

}
