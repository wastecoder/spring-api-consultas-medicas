package com.consultas.api_consultas.entities;

import com.consultas.api_consultas.enums.SiglaCrm;
import jakarta.persistence.*;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Entity
@Table(
        uniqueConstraints = {
                @UniqueConstraint(name = "uk_medico_crm", columnNames = {"crmSigla", "crmDigitos"}),
                @UniqueConstraint(name = "uk_medico_email", columnNames = "email")
        }
)
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class Medico extends Pessoa {

    @Column(length = 2, nullable = false)
    @Enumerated(EnumType.STRING)
    private SiglaCrm crmSigla;

    @Column(length = 6, nullable = false)
    private String crmDigitos;

    @Column(length = 50, nullable = false)
    private String especialidade;


    public Medico(String nome, String email, String telefone, SiglaCrm crmSigla, String crmDigitos, String especialidade) {
        super(nome, email, telefone);
        this.crmSigla = crmSigla;
        this.crmDigitos = crmDigitos;
        this.especialidade = especialidade;
    }

}
