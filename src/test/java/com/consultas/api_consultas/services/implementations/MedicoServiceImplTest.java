package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.services.rules.MedicoRules;
import jakarta.persistence.EntityNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.function.Executable;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Sort;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Médico")
class MedicoServiceImplTest {

    @InjectMocks
    private MedicoServiceImpl medicoService;

    @Mock
    private MedicoRepository repository;

    @Mock
    private MedicoRules medicoRules;

    private Medico medicoAtivo;
    private Medico medicoInativo;
    private Medico medicoInexistente;

    @BeforeEach
    void setUp() {
        criarMedicoAtivo();
        criarMedicoInativo();
        criarMedicoInexistente();
    }

    private void criarMedicoAtivo() {
        Usuario usuario = new Usuario();
        usuario.setId(100L);
        usuario.setUsername("joao.medico");
        usuario.setSenha("senha123");
        usuario.setFuncao(Funcao.MEDICO);
        usuario.setAtivo(true);

        medicoAtivo = new Medico();
        medicoAtivo.setId(1L);
        medicoAtivo.setNome("João da Silva");
        medicoAtivo.setEmail("joao@medico.com");
        medicoAtivo.setTelefone("999999999");
        medicoAtivo.setCrmSigla(SiglaCrm.SP);
        medicoAtivo.setCrmDigitos("123456");
        medicoAtivo.setEspecialidade(Especialidade.CARDIOLOGIA);
        medicoAtivo.setAtivo(true);
        medicoAtivo.setUsuario(usuario);
    }

    private void criarMedicoInativo() {
        Usuario usuario = new Usuario();
        usuario.setId(101L);
        usuario.setUsername("maria.inativa");
        usuario.setSenha("senha456");
        usuario.setFuncao(Funcao.MEDICO);
        usuario.setAtivo(true);

        medicoInativo = new Medico();
        medicoInativo.setId(2L);
        medicoInativo.setNome("Maria Oliveira");
        medicoInativo.setEmail("maria@medico.com");
        medicoInativo.setTelefone("888888888");
        medicoInativo.setCrmSigla(SiglaCrm.RJ);
        medicoInativo.setCrmDigitos("654321");
        medicoInativo.setEspecialidade(Especialidade.DERMATOLOGIA);
        medicoInativo.setAtivo(false);
        medicoInativo.setUsuario(usuario);
    }

    private void criarMedicoInexistente() {
        medicoInexistente = new Medico();
        medicoInexistente.setId(999L);
    }


    @Nested
    @DisplayName("Salvar médico")
    class SalvarMedico {

        @Test
        @DisplayName("Deve salvar médico com sucesso")
        void deveSalvarMedicoComSucesso() {
            when(repository.save(medicoAtivo)).thenReturn(medicoAtivo);

            Medico salvo = medicoService.salvar(medicoAtivo);

            assertNotNull(salvo);
            assertEquals(medicoAtivo.getNome(), salvo.getNome());
            assertEquals(medicoAtivo.getCrmDigitos(), salvo.getCrmDigitos());
            verify(repository).save(medicoAtivo);
        }
    }

    @Nested
    @DisplayName("Buscar todos os médicos")
    class BuscarTodosMedicos {

        @Test
        @DisplayName("Deve retornar todos os médicos com sucesso")
        void deveRetornarTodosMedicosComSucesso() {
            List<Medico> listaMock = List.of(medicoAtivo, medicoInativo);

            when(repository.findAll()).thenReturn(listaMock);

            List<Medico> medicos = medicoService.buscarTodos();

            assertNotNull(medicos);
            assertEquals(2, medicos.size());
            assertTrue(medicos.contains(medicoAtivo));
            assertTrue(medicos.contains(medicoInativo));
            verify(repository).findAll();
        }
    }

    @Nested
    @DisplayName("Buscar médico por ID")
    class BuscarMedicoPorId {

        @Test
        @DisplayName("Deve retornar médico se encontrado")
        void deveRetornarMedicoQuandoEncontrado() {
            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));

            Medico encontrado = medicoService.buscarPorId(medicoAtivo.getId());

            assertNotNull(encontrado);
            assertEquals(medicoAtivo.getId(), encontrado.getId());
            assertEquals(medicoAtivo.getNome(), encontrado.getNome());
            verify(repository).findById(medicoAtivo.getId());
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se médico não for encontrado")
        void deveLancarExcecaoSeMedicoNaoForEncontrado() {
            Long idInexistente = medicoInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, () ->
                    medicoService.buscarPorId(idInexistente)
            );

            assertEquals("Médico com ID [" + idInexistente + "] não encontrado", e.getMessage());
            verify(repository).findById(idInexistente);
        }
    }

    @Nested
    @DisplayName("Buscar médicos com filtros")
    class BuscarMedicosComFiltros {

        private final Sort ordenacaoPorNome = Sort.by("nome").ascending();

        @Test
        @DisplayName("Deve buscar por nome e ativo quando nome for informado")
        void deveBuscarPorNomeEAtivo() {
            String nome = "João";
            boolean ativo = true;

            when(repository.findByNomeContainingIgnoreCaseAndAtivo(nome, ativo, ordenacaoPorNome))
                    .thenReturn(List.of(medicoAtivo));

            List<Medico> resultado = medicoService.buscarMedicos(nome, null, null, ativo);

            assertNotNull(resultado);
            assertEquals(1, resultado.size());
            assertEquals(medicoAtivo, resultado.get(0));
            verify(repository).findByNomeContainingIgnoreCaseAndAtivo(nome, ativo, ordenacaoPorNome);
        }

        @Test
        @DisplayName("Deve buscar por CRM quando CRM for informado e nome não for")
        void deveBuscarPorCrm() {
            SiglaCrm sigla = medicoAtivo.getCrmSigla();
            String digitos = medicoAtivo.getCrmDigitos();

            when(repository.findByCrmSiglaAndCrmDigitos(sigla, digitos))
                    .thenReturn(Optional.of(medicoAtivo));

            List<Medico> resultado = medicoService.buscarMedicos(null, sigla, digitos, true);

            assertNotNull(resultado);
            assertEquals(1, resultado.size());
            assertEquals(medicoAtivo, resultado.get(0));
            verify(repository).findByCrmSiglaAndCrmDigitos(sigla, digitos);
        }

        @Test
        @DisplayName("Deve buscar por ativo quando apenas ativo for informado")
        void deveBuscarPorAtivo() {
            when(repository.findByAtivo(true, ordenacaoPorNome))
                    .thenReturn(List.of(medicoAtivo));

            List<Medico> resultado = medicoService.buscarMedicos(null, null, null, true);

            assertNotNull(resultado);
            assertEquals(1, resultado.size());
            assertEquals(medicoAtivo, resultado.get(0));
            verify(repository).findByAtivo(true, ordenacaoPorNome);
        }

        @Test
        @DisplayName("Deve buscar apenas médicos ativos quando nenhum filtro for informado")
        void deveBuscarAtivosQuandoNenhumFiltroInformado() {
            when(repository.findByAtivo(true, ordenacaoPorNome))
                    .thenReturn(List.of(medicoAtivo));

            List<Medico> resultado = medicoService.buscarMedicos(null, null, null, null);

            assertNotNull(resultado);
            assertEquals(1, resultado.size());
            assertEquals(medicoAtivo, resultado.get(0));
            verify(repository).findByAtivo(true, ordenacaoPorNome);
        }

        @Test
        @DisplayName("Deve buscar por ativo quando CRM estiver incompleto (apenas sigla ou apenas dígitos)")
        void deveBuscarPorAtivoQuandoCrmIncompleto() {
            // Apenas sigla informada, digitos nulo
            SiglaCrm sigla = SiglaCrm.SP;
            String digitos = null;

            when(repository.findByAtivo(true, ordenacaoPorNome))
                    .thenReturn(List.of(medicoAtivo));

            List<Medico> resultado = medicoService.buscarMedicos(null, sigla, digitos, true);

            assertNotNull(resultado);
            assertEquals(1, resultado.size());
            assertEquals(medicoAtivo, resultado.get(0));
            verify(repository).findByAtivo(true, ordenacaoPorNome);
        }
    }

    @Nested
    @DisplayName("Atualizar médico")
    class AtualizarMedico {

        @Test
        @DisplayName("Deve atualizar médico com sucesso")
        void deveAtualizarMedicoComSucesso() {
            Medico atualizado = new Medico(
                    "João Alterado",
                    "joao.novo@medico.com",
                    "888888888",
                    SiglaCrm.RJ,
                    "654321",
                    Especialidade.NEUROLOGIA
            );
            atualizado.setId(medicoAtivo.getId());
            atualizado.setAtivo(false); // Deve ser ignorado

            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));
            when(repository.save(any(Medico.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Medico salvo = medicoService.atualizar(medicoAtivo.getId(), atualizado);

            assertNotNull(salvo);
            assertEquals(atualizado.getNome(), salvo.getNome());
            assertEquals(atualizado.getEmail(), salvo.getEmail());
            assertEquals(atualizado.getTelefone(), salvo.getTelefone());
            assertEquals(atualizado.getCrmSigla(), salvo.getCrmSigla());
            assertEquals(atualizado.getCrmDigitos(), salvo.getCrmDigitos());
            assertEquals(atualizado.getEspecialidade(), salvo.getEspecialidade());
            assertTrue(salvo.getAtivo()); // Deve permanecer ativo
            verify(repository).save(medicoAtivo);
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se médico não existir")
        void deveLancarExcecaoSeMedicoNaoExistir() {
            Long idInexistente = medicoInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            Executable acao = () -> medicoService.atualizar(idInexistente, new Medico());
            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, acao);

            assertEquals("Médico com ID [" + idInexistente + "] não encontrado", ex.getMessage());
            verify(repository).findById(idInexistente);
        }

        @Test
        @DisplayName("Não deve alterar o campo ativo mesmo se informado no médico atualizado")
        void naoDeveAlterarCampoAtivo() {
            Medico atualizado = new Medico(
                    medicoAtivo.getNome(),
                    medicoAtivo.getEmail(),
                    medicoAtivo.getTelefone(),
                    medicoAtivo.getCrmSigla(),
                    medicoAtivo.getCrmDigitos(),
                    medicoAtivo.getEspecialidade()
            );
            atualizado.setAtivo(false); // Tentativa de desativar

            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));
            when(repository.save(any(Medico.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Medico salvo = medicoService.atualizar(medicoAtivo.getId(), atualizado);

            assertTrue(salvo.getAtivo()); // Continua ativo
        }
    }

    @Nested
    @DisplayName("Remover médico por ID")
    class RemoverPorId {

        @Test
        @DisplayName("Deve excluir médico quando estiver inativo")
        void deveExcluirMedicoQuandoInativo() {
            when(repository.findById(medicoInativo.getId())).thenReturn(Optional.of(medicoInativo));
            doNothing().when(repository).deleteById(medicoInativo.getId());

            assertDoesNotThrow(() -> medicoService.removerPorId(medicoInativo.getId()));

            verify(repository).deleteById(medicoInativo.getId());
        }

        @Test
        @DisplayName("Deve lançar BusinessRuleException se médico estiver ativo")
        void deveLancarExcecaoSeMedicoAtivo() {
            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));

            Executable acaoRemover = () -> medicoService.removerPorId(medicoAtivo.getId());
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, acaoRemover);

            assertEquals("Médico deve estar inativo para ser excluído.", ex.getMessage());
            verify(repository, never()).deleteById(anyLong());
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se médico não existir")
        void deveLancarExcecaoSeMedicoNaoExistir() {
            Long idInexistente = medicoInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    medicoService.removerPorId(idInexistente)
            );

            assertTrue(ex.getMessage().contains("Médico com ID [" + idInexistente + "] não encontrado"));
            verify(repository, never()).deleteById(anyLong());
        }
    }

    @Nested
    @DisplayName("Inativar médico por ID")
    class InativarPorId {

        @Test
        @DisplayName("Deve inativar médico com sucesso se não tiver consultas futuras")
        void deveInativarMedicoComSucesso() {
            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));
            doNothing().when(medicoRules).verificarSeNaoTemConsultasFuturas(medicoAtivo);
            when(repository.save(any(Medico.class))).thenAnswer(invocation -> invocation.getArgument(0));

            medicoService.inativarPorId(medicoAtivo.getId());

            assertFalse(medicoAtivo.getAtivo());
            verify(repository).save(medicoAtivo);
        }

        @Test
        @DisplayName("Não deve alterar médico se já estiver inativo")
        void naoDeveInativarMedicoQueJaEstaInativo() {
            when(repository.findById(medicoInativo.getId())).thenReturn(Optional.of(medicoInativo));
            doNothing().when(medicoRules).verificarSeNaoTemConsultasFuturas(medicoInativo);

            medicoService.inativarPorId(medicoInativo.getId());

            assertFalse(medicoInativo.getAtivo());
            verify(repository, never()).save(any(Medico.class));
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se médico não existir")
        void deveLancarExceptionSeMedicoNaoExistir() {
            Long idInexistente = medicoInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    medicoService.inativarPorId(idInexistente)
            );

            assertTrue(ex.getMessage().contains("Médico com ID [" + idInexistente + "] não encontrado"));
        }

        @Test
        @DisplayName("Deve lançar exceção se médico tiver consultas futuras")
        void deveLancarExceptionSeMedicoTiverConsultasFuturas() {
            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));
            doThrow(new BusinessRuleException("Médico possui consultas futuras"))
                    .when(medicoRules).verificarSeNaoTemConsultasFuturas(medicoAtivo);

            Executable acao = () -> medicoService.inativarPorId(medicoAtivo.getId());
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, acao);

            assertEquals("Médico possui consultas futuras", ex.getMessage());
            verify(repository, never()).save(any(Medico.class));
        }
    }

    @Nested
    @DisplayName("Ativar médico por ID")
    class AtivarPorId {

        @Test
        @DisplayName("Deve ativar médico com sucesso se estiver inativo")
        void deveAtivarMedicoComSucesso() {
            medicoInativo.setAtivo(false);
            when(repository.findById(medicoInativo.getId())).thenReturn(Optional.of(medicoInativo));
            when(repository.save(any(Medico.class))).thenAnswer(invocation -> invocation.getArgument(0));

            medicoService.ativarPorId(medicoInativo.getId());

            assertTrue(medicoInativo.getAtivo());
            verify(repository).save(medicoInativo);
        }

        @Test
        @DisplayName("Não deve alterar médico se já estiver ativo")
        void naoDeveAtivarMedicoQueJaEstaAtivo() {
            when(repository.findById(medicoAtivo.getId())).thenReturn(Optional.of(medicoAtivo));

            medicoService.ativarPorId(medicoAtivo.getId());

            assertTrue(medicoAtivo.getAtivo());
            verify(repository, never()).save(any(Medico.class));
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se médico não existir")
        void deveLancarExcecaoSeMedicoNaoExistir() {
            Long idInexistente = medicoInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    medicoService.ativarPorId(idInexistente)
            );

            assertTrue(ex.getMessage().contains("Médico com ID [" + idInexistente + "] não encontrado"));
            verify(repository, never()).save(any(Medico.class));
        }
    }

    @Nested
    @DisplayName("Buscar médico por username e ativo")
    class BuscarPorUsernameEAtivo {

        @Test
        @DisplayName("Deve retornar médico se username e ativo forem correspondentes")
        void deveRetornarMedicoAtivoPorUsername() {
            String username = medicoAtivo.getUsuario().getUsername();
            when(repository.findByUsuarioUsernameAndAtivo(username, true))
                    .thenReturn(Optional.of(medicoAtivo));

            Medico resultado = medicoService.buscarPorUsernameEAtivo(username, true);

            assertNotNull(resultado);
            assertEquals(medicoAtivo, resultado);
        }

        @Test
        @DisplayName("Deve retornar médico inativo se username e ativo forem correspondentes")
        void deveRetornarMedicoInativoPorUsername() {
            String username = medicoInativo.getUsuario().getUsername();
            when(repository.findByUsuarioUsernameAndAtivo(username, false))
                    .thenReturn(Optional.of(medicoInativo));

            Medico resultado = medicoService.buscarPorUsernameEAtivo(username, false);

            assertNotNull(resultado);
            assertEquals(medicoInativo, resultado);
        }

        @Test
        @DisplayName("Deve lançar exceção se médico não for encontrado pelo username e ativo")
        void deveLancarExcecaoSeNaoEncontrarMedicoPorUsernameEAtivo() {
            String usernameInexistente = "usuario.inexistente";
            when(repository.findByUsuarioUsernameAndAtivo(usernameInexistente, true))
                    .thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    medicoService.buscarPorUsernameEAtivo(usernameInexistente, true)
            );

            assertEquals("Médico associado ao usuário não encontrado", ex.getMessage());
        }
    }
}
