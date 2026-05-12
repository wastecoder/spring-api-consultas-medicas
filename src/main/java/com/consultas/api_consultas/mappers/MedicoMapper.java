package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.dtos.respostas.MedicoRespostaFormatada;
import com.consultas.api_consultas.entities.Medico;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

@Mapper(componentModel = "spring")
public interface MedicoMapper {

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "ativo", ignore = true)
    @Mapping(target = "usuario", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    Medico paraEntidade(MedicoRequisicao dto);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "ativo", ignore = true)
    @Mapping(target = "usuario", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    void aplicarAtualizacao(MedicoRequisicao dto, @MappingTarget Medico medico);

    @Mapping(target = "auditoria", ignore = true)
    MedicoResposta paraResposta(Medico medico);

    @Mapping(target = "auditoria",
            expression = "java(com.consultas.api_consultas.dtos.respostas.AuditoriaResposta.de(medico))")
    MedicoResposta paraRespostaComAuditoria(Medico medico);

    @Mapping(target = "crm",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarCrm(medico.getCrmSigla(), medico.getCrmDigitos()))")
    @Mapping(target = "especialidade",
            expression = "java(medico.getEspecialidade().name())")
    @Mapping(target = "telefone",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarTelefone(medico.getTelefone()))")
    @Mapping(target = "ativo",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarStatusAtivo(medico.getAtivo()))")
    MedicoRespostaFormatada paraRespostaFormatada(Medico medico);

}
