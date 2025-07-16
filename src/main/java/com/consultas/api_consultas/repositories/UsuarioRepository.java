package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UsuarioRepository extends JpaRepository<Usuario, Long> {

    // Busca um usuário pelo seu username
    Optional<Usuario> findByUsername(String username);

    // Cadastro: Verifica se já existe um usuário com o username informado
    boolean existsByUsername(String username);

    // Atualização: Verifica se o username já está em uso por outro usuário
    boolean existsByUsernameAndIdNot(String username, Long id);

}
