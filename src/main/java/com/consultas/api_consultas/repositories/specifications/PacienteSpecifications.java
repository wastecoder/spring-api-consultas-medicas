package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import org.springframework.data.jpa.domain.Specification;

public final class PacienteSpecifications {

    private PacienteSpecifications() {}


    public static Specification<Paciente> comAtivo(Boolean ativo) {
        if (ativo == null) return null;
        return (root, query, cb) -> cb.equal(root.get("ativo"), ativo);
    }

    public static Specification<Paciente> comNomeContendo(String nome) {
        if (nome == null || nome.isBlank()) return null;
        return (root, query, cb) ->
                cb.like(cb.lower(root.get("nome")), "%" + nome.toLowerCase() + "%");
    }

    public static Specification<Paciente> comCpf(String cpf) {
        if (cpf == null || cpf.isBlank()) return null;
        return (root, query, cb) -> cb.equal(root.get("cpf"), cpf);
    }

    public static Specification<Paciente> comSexo(Sexo sexo) {
        if (sexo == null) return null;
        return (root, query, cb) -> cb.equal(root.get("sexo"), sexo);
    }

}
