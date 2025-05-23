package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Medico;
import lombok.Data;

@Data
public class MedicoDetalhadoDto {

    private Long id;

    private String nome;

    private String email;

    private String crm;

    private String especialidade;

    private String telefone;

    private Boolean ativo;

    public MedicoDetalhadoDto(Medico medico) {
        this.id = medico.getId();
        this.nome = medico.getNome();
        this.email = medico.getEmail();
        this.crm = "CRM/" + medico.getCrmSigla() + " " + medico.getCrmDigitos();
        this.especialidade = medico.getEspecialidade();
        this.telefone = medico.getTelefone();
        this.ativo = medico.getAtivo();
    }

}
