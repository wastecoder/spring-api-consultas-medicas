package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.SiglaCrm;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;

import java.util.ArrayList;
import java.util.List;

public final class MedicoSpecifications {

    private MedicoSpecifications() {}


    public static Specification<Medico> comAtivo(Boolean ativo) {
        if (ativo == null) return null;
        return (root, query, cb) -> cb.equal(root.get("ativo"), ativo);
    }

    public static Specification<Medico> comNomeContendo(String nome) {
        if (nome == null || nome.isBlank()) return null;
        return (root, query, cb) ->
                cb.like(cb.lower(root.get("nome")), "%" + nome.toLowerCase() + "%");
    }

    public static Specification<Medico> comCrm(SiglaCrm sigla, String digitos) {
        boolean temSigla = sigla != null;
        boolean temDigitos = digitos != null && !digitos.isBlank();
        if (!temSigla && !temDigitos) return null;
        return (root, query, cb) -> {
            List<Predicate> ps = new ArrayList<>();
            if (temSigla) ps.add(cb.equal(root.get("crmSigla"), sigla));
            if (temDigitos) ps.add(cb.equal(root.get("crmDigitos"), digitos));
            return cb.and(ps.toArray(Predicate[]::new));
        };
    }

    public static Specification<Medico> comEspecialidade(Especialidade especialidade) {
        if (especialidade == null) return null;
        return (root, query, cb) -> cb.equal(root.get("especialidade"), especialidade);
    }

}
