package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.PacienteRequisicao;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.dtos.respostas.PacienteRespostaFormatada;
import com.consultas.api_consultas.entities.Paciente;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

@Mapper(componentModel = "spring")
public interface PacienteMapper {

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "ativo", ignore = true)
    @Mapping(target = "usuario", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    Paciente paraEntidade(PacienteRequisicao dto);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "ativo", ignore = true)
    @Mapping(target = "usuario", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    void aplicarAtualizacao(PacienteRequisicao dto, @MappingTarget Paciente paciente);

    @Mapping(target = "auditoria", ignore = true)
    PacienteResposta paraResposta(Paciente paciente);

    @Mapping(target = "auditoria",
            expression = "java(com.consultas.api_consultas.dtos.respostas.AuditoriaResposta.de(paciente))")
    PacienteResposta paraRespostaComAuditoria(Paciente paciente);

    @Mapping(target = "cpf",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarCpf(paciente.getCpf()))")
    @Mapping(target = "sexo",
            expression = "java(paciente.getSexo().name())")
    @Mapping(target = "dataNascimento",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarData(paciente.getDataNascimento()))")
    @Mapping(target = "telefone",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarTelefone(paciente.getTelefone()))")
    @Mapping(target = "ativo",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarStatusAtivo(paciente.getAtivo()))")
    PacienteRespostaFormatada paraRespostaFormatada(Paciente paciente);

}
