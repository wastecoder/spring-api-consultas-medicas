package com.consultas.api_consultas.entities;

import com.consultas.api_consultas.enums.Sexo;
import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Entity
@Table(
        uniqueConstraints = {
                @UniqueConstraint(name = "uk_paciente_cpf", columnNames = "cpf"),
                @UniqueConstraint(name = "uk_paciente_email", columnNames = "email")
        }
)
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class Paciente extends Pessoa {

    @Column(length = 11, nullable = false)
    private String cpf;

    @Column(length = 9, nullable = false)
    @Enumerated(EnumType.STRING)
    private Sexo sexo;

    @Column(nullable = false)
    private LocalDate dataNascimento;


    @OneToOne
    @JoinColumn(name = "usuarioId", unique = true)
    private Usuario usuario;


    public Paciente(String nome, String email, String telefone, String cpf, Sexo sexo, LocalDate dataNascimento) {
        super(nome, email, telefone);
        this.sexo = sexo;
        this.cpf = cpf;
        this.dataNascimento = dataNascimento;
    }

}
