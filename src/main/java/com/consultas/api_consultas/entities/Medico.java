package com.consultas.api_consultas.entities;

import com.consultas.api_consultas.enums.SiglaCrm;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;

@Entity
@Table(
        uniqueConstraints = @UniqueConstraint(
                name = "uk_medico_crm",
                columnNames = {"crmSigla", "crmDigitos"}
        )
)
@Data
@AllArgsConstructor
public class Medico {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 100, nullable = false)
    private String nome;

    @Column(length = 2, nullable = false)
    @Enumerated(EnumType.STRING)
    private SiglaCrm crmSigla;

    @Column(length = 6, nullable = false)
    private String crmDigitos;

    @Column(length = 50, nullable = false)
    private String especialidade;

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

    public Medico(String nome, SiglaCrm crmSigla, String crmDigitos, String especialidade, String email, String telefone) {
        this.nome = nome;
        this.crmSigla = crmSigla;
        this.crmDigitos = crmDigitos;
        this.especialidade = especialidade;
        this.email = email;
        this.telefone = telefone;
    }
}
