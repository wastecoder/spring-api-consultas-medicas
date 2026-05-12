package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDate;

public final class ConsultaSpecifications {

    private ConsultaSpecifications() {}


    public static Specification<Consulta> comStatus(StatusConsulta status) {
        if (status == null) return null;
        return (root, query, cb) -> cb.equal(root.get("status"), status);
    }

    public static Specification<Consulta> comMedicoId(Long medicoId) {
        if (medicoId == null) return null;
        return (root, query, cb) -> cb.equal(root.get("medico").get("id"), medicoId);
    }

    public static Specification<Consulta> comPacienteId(Long pacienteId) {
        if (pacienteId == null) return null;
        return (root, query, cb) -> cb.equal(root.get("paciente").get("id"), pacienteId);
    }

    public static Specification<Consulta> comDataAtendimento(LocalDate dataAtendimento) {
        if (dataAtendimento == null) return null;
        return (root, query, cb) -> cb.equal(root.get("dataAtendimento"), dataAtendimento);
    }

}
