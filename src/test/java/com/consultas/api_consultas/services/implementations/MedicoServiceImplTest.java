package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.mappers.MedicoMapper;
import com.consultas.api_consultas.mappers.MedicoMapperImpl;
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
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
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

    @Spy
    private MedicoMapper medicoMapper = new MedicoMapperImpl();

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

        private final Pageable pageable = PageRequest.of(0, 10, Sort.by("nome").ascending());

        @Test
        @DisplayName("Deve combinar todos os filtros informados via Specification")
        void deveCombinarTodosOsFiltros() {
            when(repository.findAll(any(Specification.class), eq(pageable)))
                    .thenReturn(new PageImpl<>(List.of(medicoAtivo), pageable, 1));

            PageResponse<MedicoResposta> resultado =
                    medicoService.buscarMedicos(0, 10, "João", SiglaCrm.SP, "123456", true, "nome", "asc");

            assertNotNull(resultado);
            assertEquals(1, resultado.totalElements());
            assertEquals("João da Silva", resultado.content().get(0).getNome());
            verify(repository).findAll(any(Specification.class), eq(pageable));
        }

        @Test
        @DisplayName("Sem nenhum filtro: ainda chama findAll com Specification (que ignora cada null)")
        void deveChamarFindAllQuandoNenhumFiltroInformado() {
            when(repository.findAll(any(Specification.class), eq(pageable)))
                    .thenReturn(new PageImpl<>(List.of(medicoAtivo, medicoInativo), pageable, 2));

            PageResponse<MedicoResposta> resultado =
                    medicoService.buscarMedicos(0, 10, null, null, null, null, "nome", "asc");

            assertNotNull(resultado);
            assertEquals(2, resultado.totalElements());
            verify(repository).findAll(any(Specification.class), eq(pageable));
        }

        @Test
        @DisplayName("CRM apenas com sigla agora é filtro válido (sem prioridade)")
        void deveAceitarCrmIncompleto() {
            when(repository.findAll(any(Specification.class), eq(pageable)))
                    .thenReturn(new PageImpl<>(List.of(medicoAtivo), pageable, 1));

            PageResponse<MedicoResposta> resultado =
                    medicoService.buscarMedicos(0, 10, null, SiglaCrm.SP, null, true, "nome", "asc");

            assertNotNull(resultado);
            assertEquals(1, resultado.totalElements());
            verify(repository).findAll(any(Specification.class), eq(pageable));
        }
    }

    @Nested
    @DisplayName("Atualizar médico")
    class AtualizarMedico {

        @Test
        @DisplayName("Deve atualizar médico com sucesso")
        void deveAtualizarMedicoComSucesso() {
            MedicoRequisicao atualizado = new MedicoRequisicao();
            atualizado.setNome("João Alterado");
            atualizado.setEmail("joao.novo@medico.com");
            atualizado.setTelefone("888888888");
            atualizado.setCrmSigla(SiglaCrm.RJ);
            atualizado.setCrmDigitos("654321");
            atualizado.setEspecialidade(Especialidade.NEUROLOGIA);

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

            Executable acao = () -> medicoService.atualizar(idInexistente, new MedicoRequisicao());
            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, acao);

            assertEquals("Médico com ID [" + idInexistente + "] não encontrado", ex.getMessage());
            verify(repository).findById(idInexistente);
        }

        @Test
        @DisplayName("Não deve alterar o campo ativo — DTO de atualização não expõe ativo")
        void naoDeveAlterarCampoAtivo() {
            MedicoRequisicao atualizado = new MedicoRequisicao();
            atualizado.setNome(medicoAtivo.getNome());
            atualizado.setEmail(medicoAtivo.getEmail());
            atualizado.setTelefone(medicoAtivo.getTelefone());
            atualizado.setCrmSigla(medicoAtivo.getCrmSigla());
            atualizado.setCrmDigitos(medicoAtivo.getCrmDigitos());
            atualizado.setEspecialidade(medicoAtivo.getEspecialidade());

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
