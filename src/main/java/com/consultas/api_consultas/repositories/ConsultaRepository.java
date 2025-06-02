package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
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

    // Filtro por status
    List<Consulta> findByStatus(StatusConsulta status, Sort sort);

    // Filtro por medico (ou medicoId?) + status
    List<Consulta> findByMedicoAndStatus(Medico medico, StatusConsulta status, Sort sort);

    // Filtro paciente (ou pacienteId?) + status
    List<Consulta> findByPacienteAndStatus(Paciente paciente, StatusConsulta status, Sort sort);

    // Filtro por medico + paciente + status
    List<Consulta> findByMedicoAndPacienteAndStatus(Medico medico, Paciente paciente, StatusConsulta status, Sort sort);

    // Filtro por data de atendimento + status
    List<Consulta> findByDataAtendimentoAndStatus(LocalDate dataAtendimento, StatusConsulta status, Sort sort);

    // Filtro entre datas + status
    List<Consulta> findByDataAtendimentoBetweenAndStatus(LocalDate inicio, LocalDate fim, StatusConsulta status, Sort sort);

}
