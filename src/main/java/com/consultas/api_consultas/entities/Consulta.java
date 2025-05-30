package com.consultas.api_consultas.entities;

import com.consultas.api_consultas.enums.StatusConsulta;
import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

@Entity
@Data
@NoArgsConstructor
public class Consulta {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private LocalDate dataAtendimento;

    @Column(nullable = false)
    private LocalTime horarioAtendimento;

    @Column(nullable = false)
    private Duration duracaoEmMinutos;

    @Column(nullable = false, updatable = false)
    private LocalDateTime dataAgendamento;

    @Column(nullable = false, precision = 10, scale = 2)
    private BigDecimal preco;

    @Column(length = 200, nullable = false)
    private String motivo;

    @Column(length = 9, nullable = false)
    @Enumerated(EnumType.STRING)
    private StatusConsulta status;


    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Medico medico;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    private Paciente paciente;


    @PrePersist
    public void definirDataAgendamentoEStatusConsulta() {
        this.dataAgendamento = LocalDateTime.now();
        this.status = StatusConsulta.AGENDADA;
    }

    public Consulta(
            LocalDate dataAtendimento,
            LocalTime horarioAtendimento,
            Duration duracaoEmMinutos,
            BigDecimal preco,
            String motivo,
            Medico medico,
            Paciente paciente
    ) {
        this.dataAtendimento = dataAtendimento;
        this.horarioAtendimento = horarioAtendimento;
        this.duracaoEmMinutos = duracaoEmMinutos;
        this.preco = preco;
        this.motivo = motivo;
        this.medico = medico;
        this.paciente = paciente;
    }


    public LocalDateTime getInicio() {
        return LocalDateTime.of(dataAtendimento, horarioAtendimento);
    }

    public LocalDateTime getFim() {
        return getInicio().plus(duracaoEmMinutos);
    }

}
