package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.ConsultasPorStatusDto;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.RelatorioService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RelatorioServiceImpl implements RelatorioService {

    private final ConsultaRepository consultaRepository;

    @Override
    public ConsultasPorStatusDto gerarRelatorioConsultasPorStatus() {
        log.info("Gerando relatório de consultas por status");

        List<Object[]> resultados = consultaRepository.contarConsultasPorStatus();

        long agendadas = 0;
        long canceladas = 0;
        long realizadas = 0;

        for (Object[] resultado : resultados) {
            String status = (String) resultado[0];
            long total = ((Number) resultado[1]).longValue();

            switch (status) {
                case "AGENDADA" -> agendadas = total;
                case "CANCELADA" -> canceladas = total;
                case "REALIZADA" -> realizadas = total;
                default -> log.warn("Status desconhecido encontrado no relatório: {}", status);
            }
        }

        return new ConsultasPorStatusDto(agendadas, canceladas, realizadas);
    }

}
