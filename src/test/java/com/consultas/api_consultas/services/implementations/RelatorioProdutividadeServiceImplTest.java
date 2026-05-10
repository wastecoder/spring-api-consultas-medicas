package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.MediaConsultasDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TaxaComparecimentoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TempoMedioDuracaoDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TempoMedioEsperaDto;
import com.consultas.api_consultas.dtos.respostas.relatorios.produtividade.TotalConsultasRealizadasNoMesDto;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Relatório de Produtividade")
class RelatorioProdutividadeServiceImplTest {

    @InjectMocks
    private RelatorioProdutividadeServiceImpl service;

    @Mock
    private ConsultaRepository repository;


    @Nested
    @DisplayName("totalConsultasPorMes")
    class TotalPorMes {

        @Test
        @DisplayName("Deve mapear (ano, mes, total) usando o status filtrado")
        void deveMapear() {
            when(repository.totalConsultasPorMes(StatusConsulta.REALIZADA)).thenReturn(List.<Object[]>of(
                    new Object[]{2025, 5, 12L}
            ));

            List<TotalConsultasRealizadasNoMesDto> dtos = service.totalConsultasPorMes(StatusConsulta.REALIZADA);
            assertEquals(new TotalConsultasRealizadasNoMesDto(2025, 5, 12L), dtos.get(0));
        }
    }


    @Nested
    @DisplayName("mediaConsultas")
    class Media {

        @Test
        @DisplayName("Deve calcular média por dia, semana e mês com base no intervalo")
        void deveCalcular() {
            when(repository.contarConsultasRealizadas()).thenReturn(30L);
            // 30 consultas em 30 dias = 1/dia
            when(repository.intervaloConsultasRealizadas()).thenReturn(new Object[]{
                    LocalDate.of(2025, 1, 1), LocalDate.of(2025, 1, 30)
            });

            MediaConsultasDto dto = service.mediaConsultas();

            assertEquals(1.0, dto.porDia(), 1e-9);
            assertEquals(7.0, dto.porSemana(), 1e-9);
            assertEquals(30.0, dto.porMes(), 1e-9);
        }

        @Test
        @DisplayName("Deve retornar zeros quando intervalo está vazio")
        void deveRetornarZerosQuandoSemDados() {
            when(repository.contarConsultasRealizadas()).thenReturn(0L);
            when(repository.intervaloConsultasRealizadas()).thenReturn(new Object[]{null, null});

            MediaConsultasDto dto = service.mediaConsultas();

            assertEquals(0, dto.porDia());
            assertEquals(0, dto.porSemana());
            assertEquals(0, dto.porMes());
        }
    }


    @Nested
    @DisplayName("tempoMedioDuracao")
    class TempoDuracao {

        @Test
        @DisplayName("Deve devolver minutos retornados pelo repositório")
        void deveRetornarValor() {
            when(repository.tempoMedioDuracaoConsultas()).thenReturn(35.0);
            assertEquals(35.0, service.tempoMedioDuracao().minutos());
        }

        @Test
        @DisplayName("Deve devolver 0 quando repositório retorna null")
        void deveRetornarZeroQuandoNull() {
            when(repository.tempoMedioDuracaoConsultas()).thenReturn(null);
            assertEquals(0, service.tempoMedioDuracao().minutos());
        }
    }


    @Nested
    @DisplayName("tempoMedioEspera")
    class TempoEspera {

        @Test
        @DisplayName("Deve converter minutos em dias com 2 casas decimais")
        void deveConverterMinutosParaDias() {
            // 1440 min = 1 dia
            when(repository.tempoMedioEsperaEmMinutos()).thenReturn(2880.0);
            TempoMedioEsperaDto dto = service.tempoMedioEspera();
            assertEquals(2.0, dto.dias());
        }

        @Test
        @DisplayName("Deve devolver 0 quando repositório retorna null")
        void deveRetornarZeroQuandoNull() {
            when(repository.tempoMedioEsperaEmMinutos()).thenReturn(null);
            assertEquals(0, service.tempoMedioEspera().dias());
        }
    }


    @Nested
    @DisplayName("taxaComparecimento")
    class Taxa {

        @Test
        @DisplayName("Deve formatar a taxa com 2 casas decimais")
        void deveFormatarTaxa() {
            when(repository.taxaComparecimento()).thenReturn(85.555);
            TaxaComparecimentoDto dto = service.taxaComparecimento();
            assertEquals(85.56, dto.percentual()); // arredondamento HALF_UP
        }

        @Test
        @DisplayName("Deve devolver 0 quando repositório retorna null")
        void deveRetornarZeroQuandoNull() {
            when(repository.taxaComparecimento()).thenReturn(null);
            assertEquals(0, service.taxaComparecimento().percentual());
        }
    }
}
