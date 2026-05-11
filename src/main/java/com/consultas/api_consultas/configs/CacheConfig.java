package com.consultas.api_consultas.configs;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;
import java.util.List;

@Configuration
@EnableCaching
public class CacheConfig {

    public static final List<String> CACHE_NAMES = List.of(
            // RelatorioConsultaService
            "consultasPorStatus",
            "consultasPorMes",
            "consultasPorAno",
            "consultasPorEspecialidade",
            // RelatorioFinanceiroService
            "faturamentoMensal",
            "faturamentoPorMedico",
            "faturamentoPorEspecialidade",
            "perdasComCancelamentos",
            "perdaMensalComCancelamentos",
            // RelatorioMedicoService
            "medicoConsultasRealizadas",
            "medicoPorEspecialidade",
            "medicoTaxaCancelamento",
            "medicoFaturamento",
            // RelatorioPacienteService
            "pacienteCancelamentos",
            "pacienteDistribuicaoSexo",
            "pacienteDistribuicaoFaixaEtaria",
            // RelatorioProdutividadeService
            "produtividadeConsultasPorMes",
            "produtividadeMediaConsultas",
            "produtividadeTempoMedioDuracao",
            "produtividadeTempoMedioEspera",
            "produtividadeTaxaComparecimento"
    );

    @Bean
    public CacheManager cacheManager(
            @Value("${app.cache.ttl-segundos:60}") long ttlSegundos,
            @Value("${app.cache.tamanho-maximo:500}") long tamanhoMaximo
    ) {
        CaffeineCacheManager manager = new CaffeineCacheManager();
        manager.setCacheNames(CACHE_NAMES);
        manager.setCaffeine(Caffeine.newBuilder()
                .expireAfterWrite(Duration.ofSeconds(ttlSegundos))
                .maximumSize(tamanhoMaximo)
                .recordStats());
        return manager;
    }

}
