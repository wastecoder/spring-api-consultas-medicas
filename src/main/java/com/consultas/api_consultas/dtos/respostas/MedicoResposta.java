package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import lombok.Getter;

@Getter
public class MedicoResposta {

    private Long id;

    private String nome;

    private String email;

    private SiglaCrm crmSigla;

    private String crmDigitos;

    private String especialidade;

    private String telefone;

    private Boolean ativo;


    public MedicoResposta(Medico medico) {
        this.id = medico.getId();
        this.nome = medico.getNome();
        this.email = medico.getEmail();
        this.crmSigla = medico.getCrmSigla();
        this.crmDigitos = medico.getCrmDigitos();
        this.especialidade = medico.getEspecialidade();
        this.telefone = medico.getTelefone();
        this.ativo = medico.getAtivo();
    }

}
