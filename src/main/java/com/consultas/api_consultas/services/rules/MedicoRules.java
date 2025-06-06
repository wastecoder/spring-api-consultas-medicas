package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class MedicoRules {

    private final ConsultaRepository consultaRepository;


    /**
     * Verifica se o médico tem consultas agendadas.
     * Se tiver, lança exceção.
     */
    public void verificarSeNaoTemConsultasFuturas(Medico medico) {
        boolean possuiConsultasFuturas  = consultaRepository.existsByMedicoAndStatus(medico, StatusConsulta.AGENDADA);

        if (possuiConsultasFuturas) {
            throw new BusinessRuleException("Não é possível inativar o médico, pois ele possui consultas futuras agendadas");
        }
    }

}
