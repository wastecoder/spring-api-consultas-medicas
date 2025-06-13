package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class PacienteRulesTest {

    @Mock
    private ConsultaRepository consultaRepository;

    @InjectMocks
    private PacienteRules pacienteRules;

    @Test
    @DisplayName("Deve permitir inativar paciente se ele não tem consultas futuras")
    void devePermitirInativarPacienteSemConsultasFuturas() {
        Paciente paciente = new Paciente();
        when(consultaRepository.existsByPacienteAndStatus(paciente, StatusConsulta.AGENDADA)).thenReturn(false);

        assertDoesNotThrow(() -> pacienteRules.verificarSeNaoTemConsultasFuturas(paciente));
    }

    @Test
    @DisplayName("Deve lançar exceção se paciente tiver consultas futuras agendadas")
    void deveLancarExcecaoSePacienteTemConsultasFuturas() {
        Paciente paciente = new Paciente();
        when(consultaRepository.existsByPacienteAndStatus(paciente, StatusConsulta.AGENDADA)).thenReturn(true);

        BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                () -> pacienteRules.verificarSeNaoTemConsultasFuturas(paciente)
        );

        assertEquals("Não é possível inativar o paciente, pois ele possui consultas futuras agendadas", ex.getMessage());
    }

}
