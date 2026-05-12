package com.consultas.api_consultas.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "password_reset_token", indexes = {
        @Index(name = "uk_password_reset_token_token", columnList = "token", unique = true)
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PasswordResetToken {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true, length = 36)
    private String token;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JoinColumn(name = "usuario_id", nullable = false)
    private Usuario usuario;

    @Column(nullable = false)
    private Instant criadoEm;

    @Column(nullable = false)
    private Instant expiraEm;

    // Null enquanto o token estiver valido; preenchido apos consumo ou invalidacao manual.
    @Column
    private Instant usadoEm;

}
