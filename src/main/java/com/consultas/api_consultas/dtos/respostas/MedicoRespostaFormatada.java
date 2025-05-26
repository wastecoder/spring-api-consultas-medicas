package com.consultas.api_consultas.dtos.respostas;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.utils.FormatoUtils;
import lombok.Getter;

@Getter
public class MedicoRespostaFormatada {

    private Long id;

    private String nome;

    private String email;

    private String crm;

    private String especialidade;

    private String telefone;

    private String ativo;


    public MedicoRespostaFormatada(Medico medico) {
        this.id = medico.getId();
        this.nome = medico.getNome();
        this.email = medico.getEmail();
        this.crm = FormatoUtils.formatarCrm(medico.getCrmSigla(), medico.getCrmDigitos());
        this.especialidade = medico.getEspecialidade();
        this.telefone = FormatoUtils.formatarTelefone(medico.getTelefone());
        this.ativo = FormatoUtils.formatarStatusAtivo(medico.getAtivo());
    }

}
