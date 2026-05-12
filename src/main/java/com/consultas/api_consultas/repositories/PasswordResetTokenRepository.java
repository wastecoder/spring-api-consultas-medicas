package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.PasswordResetToken;
import com.consultas.api_consultas.entities.Usuario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.Instant;
import java.util.Optional;

@Repository
public interface PasswordResetTokenRepository extends JpaRepository<PasswordResetToken, Long> {

    Optional<PasswordResetToken> findByToken(String token);

    void deleteAllByExpiraEmBefore(Instant agora);

    // Invalida (marca como usado) qualquer token ainda valido pertencente ao usuario.
    // Garante que apenas um link de recuperacao fique ativo por vez.
    @Modifying
    @Query("""
            update PasswordResetToken t
               set t.usadoEm = :agora
             where t.usuario = :usuario
               and t.usadoEm is null
               and t.expiraEm > :agora
            """)
    int invalidarTokensAtivosDoUsuario(@Param("usuario") Usuario usuario, @Param("agora") Instant agora);

}
