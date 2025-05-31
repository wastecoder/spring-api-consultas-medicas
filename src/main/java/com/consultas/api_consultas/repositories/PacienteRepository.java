package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface PacienteRepository extends JpaRepository<Paciente, Long> {

    // Filtro por ativo
    List<Paciente> findByAtivo(boolean ativo, Sort sort);

    // Pesquisa por CPF
    Optional<Paciente> findByCpf(String cpf, Sort sort);

    // Filtro combinado: ativo + nome (parcial e ignorando maiúsculas/minúsculas)
    List<Paciente> findByNomeContainingIgnoreCaseAndAtivo(String nome, boolean ativo, Sort sort);

    // Filtro combinado: ativo + sexo
    List<Paciente> findBySexoAndAtivo(Sexo sexo, boolean ativo, Sort sort);

}
