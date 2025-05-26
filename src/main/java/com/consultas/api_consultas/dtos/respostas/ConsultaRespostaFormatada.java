package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.utils.FormatoUtils;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;

@Getter
public class ConsultaRespostaFormatada {

    private Long id;

    private String dataAtendimento;

    @JsonFormat(pattern = "HH:mm")
    private String horarioAtendimento;

    private Long duracaoEmMinutos;

    private String dataAgendamento;

    private String preco;

    private String motivo;

    private StatusConsulta status;

    private PessoaResumo medico;

    private PessoaResumo paciente;


    public ConsultaRespostaFormatada(Consulta consulta) {
        this.id = consulta.getId();
        this.dataAtendimento = FormatoUtils.formatarData(consulta.getDataAtendimento());
        this.horarioAtendimento = FormatoUtils.formatarHora(consulta.getHorarioAtendimento());
        this.duracaoEmMinutos = consulta.getDuracaoEmMinutos().toMinutes();
        this.dataAgendamento = FormatoUtils.formatarDataHora(consulta.getDataAgendamento());
        this.preco = FormatoUtils.formatarPreco(consulta.getPreco());
        this.motivo = consulta.getMotivo();
        this.status = consulta.getStatus();

        this.medico = new PessoaResumo(consulta.getMedico());
        this.paciente = new PessoaResumo(consulta.getPaciente());
    }

}
