package com.consultas.api_consultas.entities;

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

    @Column(nullable = false)
    private LocalDate dataNascimento;


    public Paciente(String nome, String email, String telefone, String cpf, LocalDate dataNascimento) {
        super(nome, email, telefone);
        this.cpf = cpf;
        this.dataNascimento = dataNascimento;
    }

}
