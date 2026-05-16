package com.consultas.api_consultas.repositories.specifications;

import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import org.springframework.data.jpa.domain.Specification;

public final class UsuarioSpecifications {

    private UsuarioSpecifications() {}


    public static Specification<Usuario> comAtivo(Boolean ativo) {
        if (ativo == null) return null;
        return (root, query, cb) -> cb.equal(root.get("ativo"), ativo);
    }

    public static Specification<Usuario> comUsernameContendo(String username) {
        if (username == null || username.isBlank()) return null;
        return (root, query, cb) ->
                cb.like(cb.lower(root.get("username")), "%" + username.toLowerCase() + "%");
    }

    public static Specification<Usuario> comFuncao(Funcao funcao) {
        if (funcao == null) return null;
        return (root, query, cb) -> cb.equal(root.get("funcao"), funcao);
    }

}
