package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.RelatorioFinanceiroService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.CacheManager;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

// Valida que @EnableCaching + CacheConfig est�o ativos e que um @Cacheable
// de relat�rio realmente impede chamada redundante ao repositorio dentro do TTL.
@SpringBootTest
@ActiveProfiles("test")
@TestPropertySource(properties = "popular.banco=false")
@DisplayName("Cache de relat�rios (Caffeine)")
class RelatorioCacheTest {

    @Autowired
    private RelatorioFinanceiroService financeiroService;

    @Autowired
    private CacheManager cacheManager;

    @MockBean
    private ConsultaRepository consultaRepository;


    @BeforeEach
    void limparTodosCaches() {
        cacheManager.getCacheNames().forEach(nome -> {
            var cache = cacheManager.getCache(nome);
            if (cache != null) cache.clear();
        });
    }


    @Test
    @DisplayName("faturamentoMensal: 2 chamadas seguidas devem acionar o repository uma so vez")
    void faturamentoMensalCacheado() {
        when(consultaRepository.faturamentoMensal()).thenReturn(List.<Object[]>of(
                new Object[]{2025, 1, BigDecimal.valueOf(1500)},
                new Object[]{2025, 2, BigDecimal.valueOf(2200)}
        ));

        var primeira = financeiroService.faturamentoMensal();
        var segunda = financeiroService.faturamentoMensal();

        assertThat(primeira).hasSize(2);
        assertThat(segunda).isEqualTo(primeira);
        verify(consultaRepository, times(1)).faturamentoMensal();
    }

    @Test
    @DisplayName("Apos limpar o cache, o repository e acionado de novo")
    void aposClearChamaDeNovo() {
        when(consultaRepository.faturamentoMensal()).thenReturn(List.<Object[]>of(
                new Object[]{2025, 1, BigDecimal.valueOf(100)}
        ));

        financeiroService.faturamentoMensal();
        cacheManager.getCache("faturamentoMensal").clear();
        financeiroService.faturamentoMensal();

        verify(consultaRepository, times(2)).faturamentoMensal();
    }

}
