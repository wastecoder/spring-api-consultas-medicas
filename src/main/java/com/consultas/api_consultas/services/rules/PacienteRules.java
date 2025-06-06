package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PacienteRules {

    private final ConsultaRepository consultaRepository;


    /**
     * Verifica se o paciente tem consultas agendadas.
     * Se tiver, lança exceção.
     */
    public void verificarSeNaoTemConsultasFuturas(Paciente paciente) {
        boolean possuiConsultasFuturas = consultaRepository.existsByPacienteAndStatus(paciente, StatusConsulta.AGENDADA);

        if (possuiConsultasFuturas) {
            throw new BusinessRuleException("Não é possível inativar o paciente, pois ele possui consultas futuras agendadas");
        }
    }

}
