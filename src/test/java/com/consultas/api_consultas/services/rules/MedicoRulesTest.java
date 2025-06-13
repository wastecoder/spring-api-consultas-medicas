package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.entities.Medico;
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
class MedicoRulesTest {

    @Mock
    private ConsultaRepository consultaRepository;

    @InjectMocks
    private MedicoRules medicoRules;

    @Test
    @DisplayName("Deve permitir inativar médico se ele não tem consultas futuras")
    void devePermitirInativarMedicoSemConsultasFuturas() {
        Medico medico = new Medico();
        when(consultaRepository.existsByMedicoAndStatus(medico, StatusConsulta.AGENDADA)).thenReturn(false);

        assertDoesNotThrow(() -> medicoRules.verificarSeNaoTemConsultasFuturas(medico));
    }

    @Test
    @DisplayName("Deve lançar exceção se médico tiver consultas futuras agendadas")
    void deveLancarExcecaoSeMedicoTemConsultasFuturas() {
        Medico medico = new Medico();
        when(consultaRepository.existsByMedicoAndStatus(medico, StatusConsulta.AGENDADA)).thenReturn(true);

        BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                () -> medicoRules.verificarSeNaoTemConsultasFuturas(medico)
        );

        assertEquals("Não é possível inativar o médico, pois ele possui consultas futuras agendadas", ex.getMessage());
    }

}
