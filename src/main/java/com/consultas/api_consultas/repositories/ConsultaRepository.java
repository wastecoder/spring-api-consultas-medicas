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


    // >>> Relatorios - Grupo: Consultas

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


    // >>> Relatorios - Grupo: Medicos

    // Retorna a quantidade de consultas realizadas por médico
    @Query("""
        SELECT c.medico.id, c.medico.nome, COUNT(c.id)
        FROM Consulta c
        WHERE c.status = 'REALIZADA'
        GROUP BY c.medico.id, c.medico.nome
        ORDER BY COUNT(c.id) DESC, c.medico.nome ASC
    """)
    List<Object[]> contarConsultasRealizadasPorMedico();

    // Retorna os médicos com mais consultas em um determinado mês e ano
    @Query("""
        SELECT c.medico.id, c.medico.nome, COUNT(c.id)
        FROM Consulta c
        WHERE MONTH(c.dataAtendimento) = :mes
          AND YEAR(c.dataAtendimento) = :ano
        GROUP BY c.medico.id, c.medico.nome
        ORDER BY COUNT(c.id) DESC, c.medico.nome ASC
    """)
    List<Object[]> medicosComMaisConsultasNoMes(int mes, int ano);

    // Calcula a taxa de cancelamento por médico (em percentual), considerando apenas médicos com pelo menos um cancelamento
    @Query(value = """
        SELECT c.medico_id AS id, m.nome,
               COUNT(CASE WHEN c.status = 'CANCELADA' THEN 1 END) * 1.0 / COUNT(c.id) * 100 AS taxa
        FROM consulta c
        JOIN medico m ON m.id = c.medico_id
        GROUP BY c.medico_id, m.nome
        HAVING COUNT(CASE WHEN c.status = 'CANCELADA' THEN 1 END) > 0
        ORDER BY taxa DESC
    """, nativeQuery = true)
    List<Object[]> calcularTaxaCancelamentoPorMedico();

    // Calcula o faturamento total por médico, somando o preço das consultas realizadas
    @Query("""
        SELECT c.medico.id, c.medico.nome, SUM(c.preco)
        FROM Consulta c
        WHERE c.status = 'REALIZADA'
        GROUP BY c.medico.id, c.medico.nome
        ORDER BY SUM(c.preco) DESC, c.medico.nome ASC
    """)
    List<Object[]> calcularFaturamentoPorMedico();


    // >>> Relatorios - Grupo: Pacientes

    // Retorna o histórico de consultas (agendadas, realizadas e canceladas) de um paciente
    @Query("""
        SELECT c.id, c.dataAtendimento, c.horarioAtendimento, c.status, c.medico
        FROM Consulta c
        WHERE c.paciente.id = :id
        ORDER BY c.dataAtendimento DESC, c.horarioAtendimento DESC
    """)
    List<Object[]> buscarHistoricoPorPaciente(Long id);

    // Retorna a quantidade de cancelamentos realizados por paciente
    @Query("""
        SELECT c.paciente.id, c.paciente.nome, COUNT(c.id)
        FROM Consulta c
        WHERE c.status = 'CANCELADA'
        GROUP BY c.paciente.id, c.paciente.nome
        ORDER BY COUNT(c.id) DESC, c.paciente.nome ASC
    """)
    List<Object[]> contarCancelamentosPorPaciente();

    // Retorna os pacientes com maior número de consultas num período
    @Query("""
        SELECT c.paciente.id, c.paciente.nome, COUNT(c.id)
        FROM Consulta c
        WHERE c.dataAtendimento BETWEEN :inicio AND :fim
        GROUP BY c.paciente.id, c.paciente.nome
        ORDER BY COUNT(c.id) DESC, c.paciente.nome ASC
    """)
    List<Object[]> pacientesComMaisConsultasPorPeriodo(LocalDate inicio, LocalDate fim);


    // >>> Relatorios - Grupo: Produtividade

    // Retorna a quantidade de consultas por mês, filtrando pelo status informado
    @Query("""
        SELECT YEAR(c.dataAtendimento), MONTH(c.dataAtendimento), COUNT(c.id)
        FROM Consulta c
        WHERE c.status = :status
        GROUP BY YEAR(c.dataAtendimento), MONTH(c.dataAtendimento)
        ORDER BY YEAR(c.dataAtendimento), MONTH(c.dataAtendimento)
    """)
    List<Object[]> totalConsultasPorMes(StatusConsulta status);

    // Retorna o total de consultas com status REALIZADA
    @Query("""
        SELECT COUNT(c) FROM Consulta c
        WHERE c.status = 'REALIZADA'
    """)
    long contarConsultasRealizadas();

    // Retorna a data da primeira e da última consulta realizada
    @Query("""
        SELECT MIN(c.dataAtendimento), MAX(c.dataAtendimento)
        FROM Consulta c
        WHERE c.status = 'REALIZADA'
    """)
    Object intervaloConsultasRealizadas();

    // Retorna o tempo médio de duração das consultas realizadas (em minutos)
    @Query(value = """
        SELECT AVG(duracao_em_minutos)
        FROM consulta
        WHERE status = 'REALIZADA'
    """, nativeQuery = true)
    Double tempoMedioDuracaoConsultas();

    // Retorna o tempo médio entre agendamento e atendimento (em minutos)
    @Query(value = """
        SELECT AVG(EXTRACT(EPOCH FROM (data_atendimento - data_agendamento)) / 60)
        FROM consulta
        WHERE status = 'REALIZADA'
    """, nativeQuery = true)
    Double tempoMedioEsperaEmMinutos();

    // Retorna a taxa de comparecimento: realizadas ÷ (realizadas + agendadas)
    @Query("""
        SELECT 
            (SELECT COUNT(c) FROM Consulta c WHERE c.status = 'REALIZADA') * 1.0 / 
            (SELECT COUNT(c) FROM Consulta c WHERE c.status = 'AGENDADA' OR c.status = 'REALIZADA') * 100
    """)
    Double taxaComparecimento();

}
