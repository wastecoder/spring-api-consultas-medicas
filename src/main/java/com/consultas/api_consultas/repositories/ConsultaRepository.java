package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

@Repository
public interface ConsultaRepository extends JpaRepository<Consulta, Long> {

    // Filtro por status
    List<Consulta> findByStatus(StatusConsulta status, Sort sort);

    // Filtro por medico + status
    List<Consulta> findByMedicoAndStatus(Medico medico, StatusConsulta status, Sort sort);

    // Filtro por paciente + status
    List<Consulta> findByPacienteAndStatus(Paciente paciente, StatusConsulta status, Sort sort);

    // Filtro por medico + paciente + status
    List<Consulta> findByMedicoAndPacienteAndStatus(Medico medico, Paciente paciente, StatusConsulta status, Sort sort);

    // Filtro por data de atendimento + status
    List<Consulta> findByDataAtendimentoAndStatus(LocalDate dataAtendimento, StatusConsulta status, Sort sort);

    // Filtro entre datas + status
    List<Consulta> findByDataAtendimentoBetweenAndStatus(LocalDate inicio, LocalDate fim, StatusConsulta status, Sort sort);

    // Retorna true se o médico possui ao menos uma consulta com o status especificado
    boolean existsByMedicoAndStatus(Medico medico, StatusConsulta status);

    // Retorna true se o paciente possui ao menos uma consulta com o status especificado
    boolean existsByPacienteAndStatus(Paciente paciente, StatusConsulta status);

    // Retorna true se houver ao menos uma consulta para o médico que conflita com o horário informado
    @Query(value = """
        SELECT CASE WHEN COUNT(*) > 0 THEN true ELSE false END
        FROM consulta c
        WHERE c.medico_id = :medicoId
          AND c.data_atendimento = :data
          AND NOT (
                :fim <= c.horario_atendimento
            OR  :inicio >= c.horario_atendimento + (c.duracao_em_minutos * INTERVAL '1 minute')
          )
          AND c.id <> :consultaId
        """, nativeQuery = true)
    boolean existeConflitoMedico(Long medicoId, LocalDate data, LocalTime inicio, LocalTime fim, Long consultaId);

    // Retorna true se houver ao menos uma consulta para o paciente que conflita com o horário informado
    @Query(value = """
        SELECT CASE WHEN COUNT(*) > 0 THEN true ELSE false END
        FROM consulta c
        WHERE c.paciente_id = :pacienteId
          AND c.data_atendimento = :data
          AND NOT (
                :fim <= c.horario_atendimento
            OR  :inicio >= c.horario_atendimento + (c.duracao_em_minutos * INTERVAL '1 minute')
          )
          AND c.id <> :consultaId
        """, nativeQuery = true)
    boolean existeConflitoPaciente(Long pacienteId, LocalDate data, LocalTime inicio, LocalTime fim, Long consultaId);


    // Relatorios

    // Retorna a quantidade de cada status de consulta
    @Query(value = """
        SELECT status, COUNT(*) AS total
        FROM consulta
        GROUP BY status
    """, nativeQuery = true)
    List<Object[]> contarConsultasPorStatus();

    // Retorna a quantidade de consultas por mês
    @Query(value = """
        SELECT EXTRACT(MONTH FROM data_atendimento) AS mes, COUNT(*) AS total
        FROM consulta
        GROUP BY mes
        ORDER BY mes
    """, nativeQuery = true)
    List<Object[]> contarConsultasPorMes();

    // Retorna a quantidade de consultas por ano
    @Query(value = """
        SELECT EXTRACT(YEAR FROM data_atendimento) AS ano, COUNT(*) AS total
        FROM consulta
        GROUP BY ano
        ORDER BY ano
    """, nativeQuery = true)
    List<Object[]> contarConsultasPorAno();

    // Retorna a quantidade de consultas por especialidade médica
    @Query("""
        SELECT c.medico.especialidade, COUNT(c.id)
        FROM Consulta c
        GROUP BY c.medico.especialidade
        ORDER BY c.medico.especialidade
    """)
    List<Object[]> contarConsultasPorEspecialidade();

    // Retorna todas as consultas de um paciente específico
    List<Consulta> findByPacienteId(Long id);

    // Retorna todas as consultas de um médico específico
    List<Consulta> findByMedicoId(Long id);

    // Retorna todas as consultas num intervalo de datas
    List<Consulta> findByDataAtendimentoBetween(LocalDate inicio, LocalDate fim);

}
