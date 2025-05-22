package com.consultas.api_consultas.entities;

import com.consultas.api_consultas.enums.StatusConsulta;
import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Data
@NoArgsConstructor
public class Consulta {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private LocalDateTime dataAtendimento;

    @Column(nullable = false, updatable = false)
    private LocalDateTime dataAgendamento;

    @Column(precision = 10, scale = 2)
    private BigDecimal preco;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Medico medico;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Paciente paciente;

    @Column(length = 200, nullable = false)
    private String motivo;

    @Column(length = 9, nullable = false)
    @Enumerated(EnumType.STRING)
    private StatusConsulta status;


    public Consulta(LocalDateTime dataAtendimento, BigDecimal preco, Medico medico, Paciente paciente, String motivo, StatusConsulta status) {
        this.dataAtendimento = dataAtendimento;
        this.preco = preco;
        this.medico = medico;
        this.paciente = paciente;
        this.motivo = motivo;
        this.status = status;

        this.dataAgendamento = LocalDateTime.now();
    }

}
