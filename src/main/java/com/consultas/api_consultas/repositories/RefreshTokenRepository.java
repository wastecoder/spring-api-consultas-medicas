package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.RefreshToken;
import com.consultas.api_consultas.entities.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.Instant;
import java.util.Optional;

@Repository
public interface RefreshTokenRepository extends JpaRepository<RefreshToken, Long> {

    Optional<RefreshToken> findByToken(String token);

    void deleteByToken(String token);

    void deleteByUsuario(Usuario usuario);

    void deleteAllByExpiraEmBefore(Instant agora);

}
