package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MedicoRepository extends JpaRepository<Medico, Long> {

    // Filtro por ativo
    List<Medico> findByAtivo(Boolean ativo);

    // Filtro combinado: ativo + nome (parcial e ignorando maiúsculas/minúsculas)
    List<Medico> findByNomeContainingIgnoreCaseAndAtivo(String nome, Boolean ativo);


    // Filtro combinado: ativo + CRM (sigla + dígitos)
    List<Medico> findByCrmSiglaAndCrmDigitosAndAtivo(SiglaCrm crmSigla, String crmDigitos, Boolean ativo);

}
