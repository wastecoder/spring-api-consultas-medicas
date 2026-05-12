package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import org.mapstruct.Builder;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

// Builder do Lombok no Usuario nao inclui os campos de auditoria herdados — forcamos
// o MapStruct a usar setters em vez do builder.
@Mapper(componentModel = "spring", builder = @Builder(disableBuilder = true))
public interface UsuarioMapper {

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "ativo", ignore = true)
    @Mapping(target = "senha", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    Usuario paraEntidade(UsuarioCadastroDto dto);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "ativo", ignore = true)
    @Mapping(target = "senha", ignore = true)
    @Mapping(target = "funcao", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    void aplicarAtualizacao(UsuarioAtualizacaoDto dto, @MappingTarget Usuario usuario);

    @Mapping(target = "auditoria", ignore = true)
    UsuarioResposta paraResposta(Usuario usuario);

    @Mapping(target = "auditoria",
            expression = "java(com.consultas.api_consultas.dtos.respostas.AuditoriaResposta.de(usuario))")
    UsuarioResposta paraRespostaComAuditoria(Usuario usuario);

}
