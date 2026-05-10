package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.TokenBlacklist;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.Instant;

@Repository
public interface TokenBlacklistRepository extends JpaRepository<TokenBlacklist, Long> {

    boolean existsByJti(String jti);

    void deleteAllByExpiraEmBefore(Instant agora);

}
