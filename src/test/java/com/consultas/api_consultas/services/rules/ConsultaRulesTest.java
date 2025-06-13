package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Pessoa;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Regras de negócio da Consulta")
class ConsultaRulesTest {

    @Mock
    ConsultaRepository consultaRepository;

    @InjectMocks
    ConsultaRules consultaRules;

    private Medico medicoAtivo;
    private Medico medicoInativo;

    private Paciente pacienteAtivo;
    private Paciente pacienteInativo;

    private Consulta consulta1;
    private Consulta consulta2;
    private Consulta consulta3;

    private Consulta consultaAgendadaOntem;
    private Consulta consultaRealizadaOntem;

    @BeforeEach
    void setUp() {
        cadastrarUmMedicoAtivo();
        cadastrarUmMedicoInativo();

        cadastrarUmPacienteAtivo();
        cadastrarUmPacienteInativo();

        cadastrarTresConsultas();
        cadastrarConsultasNoPassado();
    }

    private void cadastrarUmMedicoAtivo() {
        medicoAtivo = new Medico("Joao Pedro", "joao.pedro@medexample.com", "11912345678", SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA);
        medicoAtivo.setId(1L);
    }

    private void cadastrarUmMedicoInativo() {
        medicoInativo = new Medico("Maria Luiza", "maria.luiza@medexample.com", "85977889900", SiglaCrm.CE, "654321", Especialidade.ORTOPEDIA);
        medicoInativo.setId(2L);
        medicoInativo.setAtivo(false);
    }

    private void cadastrarUmPacienteAtivo() {
        pacienteAtivo = new Paciente("Ana Paula", "ana.paula@paciente.com", "21988776655", "98765432100", Sexo.FEMININO, LocalDate.of(1985, 3, 22));
        pacienteAtivo.setId(1L);
    }

    private void cadastrarUmPacienteInativo() {
        pacienteInativo = new Paciente("Matheus Henrique", "matheus.henrique@exemplo.com", "22222222222", "99999999999", Sexo.MASCULINO, LocalDate.of(1990, 1, 1));
        pacienteInativo.setId(2L);
        pacienteInativo.setAtivo(false);
    }

    private void cadastrarTresConsultas() {
        //Consulta 1 = [08:00, 10:00]
        //Consulta 2 = [10:00, 11:00]
        //Consulta 3 = [10:30, 11:00]
        consulta1 = new Consulta(LocalDate.of(2025, 6, 6), LocalTime.of(8, 0), Duration.ofMinutes(120), new BigDecimal("210.00"), "Avaliação de dor torácica", medicoAtivo, pacienteAtivo);
        consulta2 = new Consulta(LocalDate.of(2025, 6, 6), LocalTime.of(10, 0), Duration.ofMinutes(60), new BigDecimal("150.00"), "Checagem de pressão arterial", medicoAtivo, pacienteAtivo);
        consulta3 = new Consulta(LocalDate.of(2025, 6, 6), LocalTime.of(10, 30), Duration.ofMinutes(30), new BigDecimal("150.00"), "Palpitações e arritmia", medicoAtivo, pacienteAtivo);
    }

    private void cadastrarConsultasNoPassado() {
        consultaAgendadaOntem = new Consulta(
                LocalDate.now().minusDays(1),
                LocalTime.of(10, 0),
                Duration.ofMinutes(30),
                BigDecimal.TEN,
                "Consulta agendada ontem",
                medicoAtivo,
                pacienteAtivo
        );
        consultaAgendadaOntem.setStatus(StatusConsulta.AGENDADA);

        consultaRealizadaOntem = new Consulta(
                LocalDate.now().minusDays(1),
                LocalTime.of(11, 0),
                Duration.ofMinutes(30),
                BigDecimal.TEN,
                "Consulta realizada ontem",
                medicoAtivo,
                pacienteAtivo
        );
        consultaRealizadaOntem.setStatus(StatusConsulta.REALIZADA);
    }

    private Consulta clonarConsulta(Consulta original) {
        Consulta copia = new Consulta(
                original.getDataAtendimento(),
                original.getHorarioAtendimento(),
                original.getDuracaoEmMinutos(),
                original.getPreco(),
                original.getMotivo(),
                original.getMedico(),
                original.getPaciente()
        );
        copia.setStatus(original.getStatus());
        return copia;
    }


    @Nested
    @DisplayName("Pessoa ativa")
    class VerificaSePessoaEstaAtiva {

        @Test
        @DisplayName("Não deve lançar exceção se a pessoa estiver ativa")
        void devePermitirPessoaAtiva() {
            Pessoa pessoaAtiva = medicoAtivo;

            assertDoesNotThrow(() ->
                    consultaRules.verificaSePessoaEstaAtiva(pessoaAtiva, "médico"));
        }

        @Test
        @DisplayName("Deve lançar exceção se a pessoa estiver inativa")
        void deveLancarExcecaoSePessoaInativa() {
            Pessoa pessoaInativa = pacienteInativo;

            BusinessRuleException e = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.verificaSePessoaEstaAtiva(pessoaInativa, "paciente")
            );

            assertEquals("Não é possível agendar consulta com paciente inativo.", e.getMessage());
        }

    }

    @Nested
    @DisplayName("Conflitos de horário")
    class VerificaSobreposicaoHorario {

        @Test
        @DisplayName("Deve lançar exceção se houver conflito com médico")
        void deveDetectarConflitoMedico() {
            // Simula conflito com médico (consulta2 sobrepõe a consulta1)
            when(consultaRepository.existeConflitoMedico(
                    consulta2.getMedico().getId(),
                    consulta2.getDataAtendimento(),
                    consulta2.getHorarioAtendimento(),
                    consulta2.getHorarioAtendimento().plus(consulta2.getDuracaoEmMinutos()),
                    consulta2.getId()
            )).thenReturn(true);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.verificaSobreposicaoHorario(consulta2)
            );

            assertEquals("O horário solicitado entra em conflito com outra consulta para o médico.", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se houver conflito com paciente")
        void deveDetectarConflitoPaciente() {
            // Simula conflito com paciente (consulta3 sobrepõe a consulta2)
            when(consultaRepository.existeConflitoMedico(
                    any(), any(), any(), any(), any())).thenReturn(false);

            when(consultaRepository.existeConflitoPaciente(
                    consulta3.getPaciente().getId(),
                    consulta3.getDataAtendimento(),
                    consulta3.getHorarioAtendimento(),
                    consulta3.getHorarioAtendimento().plus(consulta3.getDuracaoEmMinutos()),
                    consulta3.getId()
            )).thenReturn(true);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.verificaSobreposicaoHorario(consulta3)
            );

            assertEquals("O horário solicitado entra em conflito com outra consulta para o paciente.", ex.getMessage());
        }

        @Test
        @DisplayName("Não deve lançar exceção se não houver conflitos")
        void devePermitirSemConflitos() {
            when(consultaRepository.existeConflitoMedico(any(), any(), any(), any(), any())).thenReturn(false);
            when(consultaRepository.existeConflitoPaciente(any(), any(), any(), any(), any())).thenReturn(false);

            assertDoesNotThrow(() -> consultaRules.verificaSobreposicaoHorario(consulta1));
        }

    }

    @Nested
    @DisplayName("Validação de horário e dia")
    class ValidarHorarioEDia {

        @Test
        @DisplayName("Deve lançar exceção se consulta for no fim de semana")
        void deveValidarDiaUtil() {
            Consulta consulta = new Consulta(
                    LocalDate.of(2025, 6, 7), // Sábado
                    LocalTime.of(10, 0),
                    Duration.ofMinutes(30),
                    BigDecimal.TEN,
                    "Rotina",
                    medicoAtivo,
                    pacienteAtivo
            );

            // Then
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.verificarDiaUtil(consulta)
            );

            assertEquals("Consulta deve ser marcada em um dia útil (Segunda a Sexta).", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se fora do horário comercial")
        void deveValidarHorarioComercial() {
            // Consulta das 17:00 às 18:30 (30min fora do horário comercial)
            Consulta consulta = new Consulta(
                    LocalDate.of(2025, 6, 9),
                    LocalTime.of(17, 0),
                    Duration.ofMinutes(90),
                    BigDecimal.TEN,
                    "Rotina",
                    medicoAtivo,
                    pacienteAtivo
            );

            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.verificarHorarioComercial(consulta)
            );

            assertEquals("Consulta deve ocorrer entre 08:00 e 18:00.", ex.getMessage());
        }

        @Test
        @DisplayName("Não deve lançar exceção se consulta estiver em horário e dia válidos")
        void horarioEDiaValidos() {
            assertDoesNotThrow(() -> {
                consultaRules.verificarDiaUtil(consulta1);
                consultaRules.verificarHorarioComercial(consulta1);
            });
        }

    }

    @Nested
    @DisplayName("Transição de status da consulta")
    class ValidarTransicaoDeStatus {

        @Test
        @DisplayName("Deve lançar exceção se tentar alterar status de consulta passada para algo diferente de REALIZADA ou CANCELADA")
        void deveValidarStatusConsultaAposHorario() {
            Consulta consultaNova = clonarConsulta(consultaAgendadaOntem);
            consultaNova.setStatus(StatusConsulta.AGENDADA); // Tentando manter AGENDADA após a data

            StatusConsulta statusConsultaNova = consultaNova.getStatus();
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.validarTransicaoAposHorario(consultaAgendadaOntem, statusConsultaNova)
            );

            assertEquals("Após a data/hora da consulta, ela só pode ser marcada como REALIZADA ou CANCELADA.", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se tentar cancelar uma consulta já realizada")
        void deveValidarCancelamentoDeConsultaRealizada() {
            Consulta consultaNova = clonarConsulta(consultaRealizadaOntem);
            consultaNova.setStatus(StatusConsulta.CANCELADA); // Tentando cancelar após realizada

            StatusConsulta statusConsultaOntem = consultaRealizadaOntem.getStatus();
            StatusConsulta statusConsultaNova = consultaNova.getStatus();
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () ->
                    consultaRules.validarCancelamentoDeConsultaRealizada(statusConsultaOntem, statusConsultaNova)
            );

            assertEquals("Não é permitido cancelar uma consulta que já foi realizada.", ex.getMessage());
        }

        @Test
        @DisplayName("Não deve lançar exceção se status de consulta passada for REALIZADA")
        void devePermitirStatusRealizadaAposHorario() {
            Consulta consultaNova = clonarConsulta(consultaAgendadaOntem);
            consultaNova.setStatus(StatusConsulta.REALIZADA);

            assertDoesNotThrow(() ->
                    consultaRules.validarTransicaoAposHorario(consultaAgendadaOntem, consultaNova.getStatus()));
        }

        @Test
        @DisplayName("Não deve lançar exceção se status de consulta passada for CANCELADA")
        void devePermitirStatusCanceladaAposHorario() {
            Consulta consultaNova = clonarConsulta(consultaAgendadaOntem);
            consultaNova.setStatus(StatusConsulta.CANCELADA);

            assertDoesNotThrow(() ->
                    consultaRules.validarTransicaoAposHorario(consultaAgendadaOntem, consultaNova.getStatus()));
        }

    }

}
