package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaCadastroDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.dtos.respostas.ConsultaRespostaFormatada;
import com.consultas.api_consultas.dtos.respostas.PessoaResumo;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Pessoa;
import com.consultas.api_consultas.utils.ConversorEntradaUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Named;

import java.time.Duration;
import java.time.LocalTime;

@Mapper(componentModel = "spring")
public interface ConsultaMapper {

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "dataAgendamento", ignore = true)
    @Mapping(target = "status", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    @Mapping(target = "horarioAtendimento", source = "horarioAtendimento", qualifiedByName = "stringParaLocalTime")
    @Mapping(target = "duracaoEmMinutos", source = "duracaoEmMinutos", qualifiedByName = "integerParaDuration")
    @Mapping(target = "medico", source = "medicoId", qualifiedByName = "idParaMedico")
    @Mapping(target = "paciente", source = "pacienteId", qualifiedByName = "idParaPaciente")
    Consulta paraEntidade(ConsultaCadastroDto dto);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "dataAgendamento", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    @Mapping(target = "horarioAtendimento", source = "horarioAtendimento", qualifiedByName = "stringParaLocalTime")
    @Mapping(target = "duracaoEmMinutos", source = "duracaoEmMinutos", qualifiedByName = "integerParaDuration")
    @Mapping(target = "medico", source = "medicoId", qualifiedByName = "idParaMedico")
    @Mapping(target = "paciente", source = "pacienteId", qualifiedByName = "idParaPaciente")
    Consulta paraEntidade(ConsultaAtualizacaoDto dto);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "dataAgendamento", ignore = true)
    @Mapping(target = "createdBy", ignore = true)
    @Mapping(target = "createdDate", ignore = true)
    @Mapping(target = "lastModifiedBy", ignore = true)
    @Mapping(target = "lastModifiedDate", ignore = true)
    @Mapping(target = "horarioAtendimento", source = "horarioAtendimento", qualifiedByName = "stringParaLocalTime")
    @Mapping(target = "duracaoEmMinutos", source = "duracaoEmMinutos", qualifiedByName = "integerParaDuration")
    @Mapping(target = "medico", source = "medicoId", qualifiedByName = "idParaMedico")
    @Mapping(target = "paciente", source = "pacienteId", qualifiedByName = "idParaPaciente")
    void aplicarAtualizacao(ConsultaAtualizacaoDto dto, @MappingTarget Consulta consulta);

    @Mapping(target = "duracaoEmMinutos", source = "duracaoEmMinutos", qualifiedByName = "durationParaMinutos")
    @Mapping(target = "auditoria", ignore = true)
    ConsultaResposta paraResposta(Consulta consulta);

    @Mapping(target = "duracaoEmMinutos", source = "duracaoEmMinutos", qualifiedByName = "durationParaMinutos")
    @Mapping(target = "auditoria",
            expression = "java(com.consultas.api_consultas.dtos.respostas.AuditoriaResposta.de(consulta))")
    ConsultaResposta paraRespostaComAuditoria(Consulta consulta);

    @Mapping(target = "dataAtendimento",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarData(consulta.getDataAtendimento()))")
    @Mapping(target = "horarioAtendimento",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarHora(consulta.getHorarioAtendimento()))")
    @Mapping(target = "duracaoEmMinutos", source = "duracaoEmMinutos", qualifiedByName = "durationParaMinutos")
    @Mapping(target = "dataAgendamento",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarDataHora(consulta.getDataAgendamento()))")
    @Mapping(target = "preco",
            expression = "java(com.consultas.api_consultas.utils.FormatoUtils.formatarPreco(consulta.getPreco()))")
    ConsultaRespostaFormatada paraRespostaFormatada(Consulta consulta);


    @Named("stringParaLocalTime")
    default LocalTime stringParaLocalTime(String horario) {
        return horario != null ? ConversorEntradaUtils.converterStringParaLocalTime(horario) : null;
    }

    @Named("integerParaDuration")
    default Duration integerParaDuration(Integer minutos) {
        return minutos != null ? ConversorEntradaUtils.converterIntegerParaDuration(minutos) : null;
    }

    @Named("durationParaMinutos")
    default Long durationParaMinutos(Duration duracao) {
        return duracao != null ? duracao.toMinutes() : null;
    }

    @Named("idParaMedico")
    default Medico idParaMedico(Long id) {
        if (id == null) return null;
        Medico medico = new Medico();
        medico.setId(id);
        return medico;
    }

    @Named("idParaPaciente")
    default Paciente idParaPaciente(Long id) {
        if (id == null) return null;
        Paciente paciente = new Paciente();
        paciente.setId(id);
        return paciente;
    }

    default PessoaResumo pessoaParaResumo(Pessoa pessoa) {
        return pessoa != null ? new PessoaResumo(pessoa.getId(), pessoa.getNome()) : null;
    }

}
