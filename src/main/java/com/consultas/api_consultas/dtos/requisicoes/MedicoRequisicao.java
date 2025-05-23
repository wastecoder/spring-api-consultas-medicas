package com.consultas.api_consultas.dtos.requisicoes;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import jakarta.validation.constraints.*;
import lombok.Data;

@Data
public class MedicoRequisicao {

    @NotBlank(message = "Nome é obrigatório")
    @Size(min = 3, max = 100, message = "Nome deve ter entre 3 a 100 caracteres")
    private String nome;

    @NotBlank(message = "E-mail é obrigatório")
    @Size(min = 5, max = 50, message = "E-mail deve ter entre 5 a 50 caracteres")
    @Pattern(
            regexp = "^[\\w._%+-]+@[\\w.-]+\\.[a-zA-Z]{2,}$",
            message = "E-mail deve ser válido"
    )
    private String email;

    @NotNull(message = "Sigla do CRM é obrigatória")
    private SiglaCrm crmSigla;

    @NotBlank(message = "Dígitos do CRM são obrigatórios")
    @Pattern(
            regexp = "\\d{6}",
            message = "Dígitos do CRM devem conter exatamente 6 dígitos numéricos"
    )
    private String crmDigitos;

    @NotBlank(message = "Especialidade é obrigatória")
    @Size(min = 5, max = 50, message = "Especialidade deve ter entre 5 a 50 caracteres")
    private String especialidade;

    @NotBlank(message = "Telefone é obrigatório")
    @Size(min = 10, max = 11, message = "Telefone deve ter entre 10 a 11 números")
    @Pattern(
            regexp = "\\d{10,11}",
            message = "Telefone deve conter apenas números com 10 ou 11 dígitos"
    )
    private String telefone;

    public Medico dtoParaMedico() {
        Medico medico = new Medico();
        medico.setNome(this.getNome());
        medico.setEmail(this.getEmail());
        medico.setCrmSigla(this.getCrmSigla());
        medico.setCrmDigitos(this.getCrmDigitos());
        medico.setEspecialidade(this.getEspecialidade());
        medico.setTelefone(this.getTelefone());
        return medico;
    }

}
