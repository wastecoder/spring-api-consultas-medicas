package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Pessoa;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.*;

@Component
@RequiredArgsConstructor
public class ConsultaRules {

    private final ConsultaRepository consultaRepository;


    public void validarCadastro(Consulta consultaNova) {
        verificaSePessoaEstaAtiva(consultaNova.getMedico(), "médico");
        verificaSePessoaEstaAtiva(consultaNova.getPaciente(), "paciente");

        verificaSobreposicaoHorario(consultaNova);

        verificarDiaUtil(consultaNova);
        verificarHorarioComercial(consultaNova);
    }

    public void validarAtualizacao(Consulta consultaAtualizada, Consulta consultaAntiga) {
        validarCadastro(consultaAtualizada);

        validarTransicaoDeStatus(consultaAntiga, consultaAtualizada);
    }

    private void validarTransicaoDeStatus(Consulta consultaAntiga, Consulta consultaNova) {
        validarTransicaoAposHorario(consultaAntiga, consultaNova.getStatus());
        validarCancelamentoDeConsultaRealizada(consultaAntiga.getStatus(), consultaNova.getStatus());
    }


    /**
     * 1. Não permitir agendamento se pessoa (médico ou paciente) estiver inativo.
     */
    void verificaSePessoaEstaAtiva(Pessoa pessoa, String contexto) {
        if (Boolean.FALSE.equals(pessoa.getAtivo())) {
            throw new BusinessRuleException("Não é possível agendar consulta com " + contexto + " inativo.");
        }
    }

    /**
     * 3. Não permitir duas consultas no mesmo horário para o mesmo médico.
     * 4. Não permitir duas consultas no mesmo horário para o mesmo paciente.
     */
    void verificaSobreposicaoHorario(Consulta consulta) {
        LocalDate data = consulta.getDataAtendimento();
        LocalTime inicio = consulta.getHorarioAtendimento();
        Duration duracao = consulta.getDuracaoEmMinutos();
        LocalTime fim = inicio.plus(duracao);

        Long consultaId = consulta.getId(); // Evita detectar a própria consulta ao editar

        if (consultaRepository.existeConflitoMedico(consulta.getMedico().getId(), data, inicio, fim, consultaId)) {
            throw new BusinessRuleException("O horário solicitado entra em conflito com outra consulta para o médico.");
        }

        if (consultaRepository.existeConflitoPaciente(consulta.getPaciente().getId(), data, inicio, fim, consultaId)) {
            throw new BusinessRuleException("O horário solicitado entra em conflito com outra consulta para o paciente.");
        }
    }

    /**
     * 5. Consulta só pode ser agendada em dias úteis.
     */
    void verificarDiaUtil(Consulta consulta) {
        var inicio = consulta.getInicio();

        if (inicio.getDayOfWeek() == DayOfWeek.SATURDAY || inicio.getDayOfWeek() == DayOfWeek.SUNDAY) {
            throw new BusinessRuleException("Consulta deve ser marcada em um dia útil (Segunda a Sexta).");
        }
    }

    /**
     * 5. Consulta só pode ser agendada em horários comerciais (08:00 às 18:00).
     */
    void verificarHorarioComercial(Consulta consulta) {
        var horaInicio = consulta.getInicio().toLocalTime();
        var horaFim = consulta.getFim().toLocalTime();

        var abertura = LocalTime.of(8, 0);
        var fechamento = LocalTime.of(18, 0);

        if (horaInicio.isBefore(abertura) || horaFim.isAfter(fechamento)) {
            throw new BusinessRuleException("Consulta deve ocorrer entre 08:00 e 18:00.");
        }
    }

    /**
     * 7. Após a data/hora da consulta, ela só pode ser marcada como REALIZADA ou CANCELADA.
     */
    void validarTransicaoAposHorario(Consulta consultaAntiga, StatusConsulta novoStatus) {
        LocalDateTime dataHoraConsulta = LocalDateTime.of(consultaAntiga.getDataAtendimento(), consultaAntiga.getHorarioAtendimento());
        boolean consultaNoPassado = LocalDateTime.now().isAfter(dataHoraConsulta);

        if (consultaAntiga.getStatus() == StatusConsulta.AGENDADA
                && consultaNoPassado
                && !(novoStatus == StatusConsulta.REALIZADA || novoStatus == StatusConsulta.CANCELADA)) {
            throw new BusinessRuleException("Após a data/hora da consulta, ela só pode ser marcada como REALIZADA ou CANCELADA.");
        }
    }

    /**
     * 8. Não permitir cancelamento de consultas já realizadas.
     */
    void validarCancelamentoDeConsultaRealizada(StatusConsulta statusAtual, StatusConsulta novoStatus) {
        if (statusAtual == StatusConsulta.REALIZADA && novoStatus == StatusConsulta.CANCELADA) {
            throw new BusinessRuleException("Não é permitido cancelar uma consulta que já foi realizada.");
        }
    }

}
