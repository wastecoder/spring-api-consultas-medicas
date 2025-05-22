package com.consultas.api_consultas.entities;

import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Entity
@Data
@NoArgsConstructor
public class Paciente {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 100, nullable = false)
    private String nome;

    @Column(length = 11, nullable = false, unique = true)
    private String cpf;

    @Column(nullable = false)
    private LocalDate dataNascimento;

    @Column(length = 50, nullable = false, unique = true)
    private String email;

    @Column(length = 11)
    private String telefone;

    @Column(nullable = false)
    private Boolean ativo;


    @PrePersist
    public void prePersist() {
        if (ativo == null) {
            ativo = true;
        }
    }

    public Paciente(String nome, String cpf, LocalDate dataNascimento, String email, String telefone) {
        this.nome = nome;
        this.cpf = cpf;
        this.dataNascimento = dataNascimento;
        this.email = email;
        this.telefone = telefone;
    }
}
