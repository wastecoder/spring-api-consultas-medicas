package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UsuarioRepository extends JpaRepository<Usuario, Long>, JpaSpecificationExecutor<Usuario> {

    // Busca um usuário pelo seu username
    Optional<Usuario> findByUsername(String username);

    // Recuperação de senha: localiza usuário ativo a partir do email (case-insensitive)
    Optional<Usuario> findByEmailIgnoreCaseAndAtivoTrue(String email);

    // Cadastro: Verifica se já existe um usuário com o username informado
    boolean existsByUsername(String username);

    // Atualização: Verifica se o username já está em uso por outro usuário
    boolean existsByUsernameAndIdNot(String username, Long id);

    // Cadastro: Verifica se já existe um usuário com o email informado (case-insensitive)
    boolean existsByEmailIgnoreCase(String email);

    // Atualização: Verifica se o email já está em uso por outro usuário (case-insensitive)
    boolean existsByEmailIgnoreCaseAndIdNot(String email, Long id);

}
