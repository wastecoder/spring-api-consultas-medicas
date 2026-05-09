package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.services.rules.PacienteRules;
import com.consultas.api_consultas.utils.SecurityUtil;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.AccessDeniedException;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Paciente")
class PacienteServiceImplTest {

    @InjectMocks
    private PacienteServiceImpl pacienteService;

    @Mock
    private PacienteRepository repository;

    @Mock
    private PacienteRules pacienteRules;

    @Mock
    private SecurityUtil securityUtil;

    private Paciente pacienteAtivo;
    private Paciente pacienteInativo;
    private Paciente pacienteInexistente;
    private Paciente pacienteComAcessoNegado;

    @BeforeEach
    void setUp() {
        criarPacienteAtivoComUsuario();
        criarPacienteInativo();
        criarPacienteInexistente();
        criarPacienteComAcessoNegado();
    }

    private void criarPacienteAtivoComUsuario() {
        Usuario usuarioPacienteAtivo = new Usuario();
        usuarioPacienteAtivo.setId(10L);
        usuarioPacienteAtivo.setUsername("ana.paciente");
        usuarioPacienteAtivo.setSenha("senha123");
        usuarioPacienteAtivo.setFuncao(Funcao.PACIENTE);
        usuarioPacienteAtivo.setAtivo(true);

        pacienteAtivo = new Paciente(
                "Ana Souza",
                "ana@paciente.com",
                "99999999999",
                "12345678900",
                Sexo.FEMININO,
                LocalDate.of(1990, 3, 15)
        );
        pacienteAtivo.setId(1L);
        pacienteAtivo.setAtivo(true);
        pacienteAtivo.setUsuario(usuarioPacienteAtivo);
    }

    private void criarPacienteInativo() {
        Usuario usuarioPacienteInativo = new Usuario();
        usuarioPacienteInativo.setId(11L);
        usuarioPacienteInativo.setUsername("carlos.inativo");
        usuarioPacienteInativo.setSenha("senha456");
        usuarioPacienteInativo.setFuncao(Funcao.PACIENTE);
        usuarioPacienteInativo.setAtivo(true);

        pacienteInativo = new Paciente(
                "Carlos Lima",
                "carlos@paciente.com",
                "88888888888",
                "11122233344",
                Sexo.MASCULINO,
                LocalDate.of(1985, 7, 20)
        );
        pacienteInativo.setId(2L);
        pacienteInativo.setAtivo(false);
        pacienteInativo.setUsuario(usuarioPacienteInativo);
    }

    private void criarPacienteInexistente() {
        pacienteInexistente = new Paciente();
        pacienteInexistente.setId(999L);
    }

    private void criarPacienteComAcessoNegado() {
        pacienteComAcessoNegado = new Paciente(
                "João Oliveira",
                "joao@semacesso.com",
                "77777777777",
                "99988877766",
                Sexo.MASCULINO,
                LocalDate.of(1992, 12, 5)
        );
        pacienteComAcessoNegado.setId(3L);
        pacienteComAcessoNegado.setAtivo(true);
    }

    @Nested
    @DisplayName("Salvar paciente")
    class SalvarPaciente {

        @Test
        @DisplayName("Deve salvar paciente com sucesso")
        void deveSalvarPacienteComSucesso() {
            when(repository.save(pacienteAtivo)).thenReturn(pacienteAtivo);

            Paciente salvo = pacienteService.salvar(pacienteAtivo);

            assertNotNull(salvo);
            assertEquals(pacienteAtivo.getNome(), salvo.getNome());
            assertEquals(pacienteAtivo.getCpf(), salvo.getCpf());
            verify(repository).save(pacienteAtivo);
        }
    }

    @Nested
    @DisplayName("Buscar todos os pacientes")
    class BuscarTodosPacientes {

        @Test
        @DisplayName("Deve retornar todos os pacientes com sucesso")
        void deveRetornarTodosPacientesComSucesso() {
            List<Paciente> listaMock = List.of(pacienteAtivo, pacienteInativo, pacienteComAcessoNegado);

            when(repository.findAll()).thenReturn(listaMock);

            List<Paciente> pacientes = pacienteService.buscarTodos();

            assertNotNull(pacientes);
            assertEquals(3, pacientes.size());
            assertTrue(pacientes.contains(pacienteAtivo));
            assertTrue(pacientes.contains(pacienteInativo));
            assertTrue(pacientes.contains(pacienteComAcessoNegado));
            verify(repository).findAll();
        }
    }

    @Nested
    @DisplayName("Buscar paciente por ID")
    class BuscarPacientePorId {

        @Test
        @DisplayName("Deve retornar paciente se encontrado e acesso permitido")
        void deveRetornarPacienteQuandoEncontradoEComPermissao() {
            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);

            Paciente encontrado = pacienteService.buscarPorId(pacienteAtivo.getId());

            assertNotNull(encontrado);
            assertEquals(pacienteAtivo.getId(), encontrado.getId());
            assertEquals(pacienteAtivo.getNome(), encontrado.getNome());
            verify(repository).findById(pacienteAtivo.getId());
            verify(securityUtil).canAccessPatient(pacienteAtivo);
        }

        @Test
        @DisplayName("Deve lançar AccessDeniedException se acesso for negado")
        void deveLancarExcecaoSeAcessoForNegado() {
            when(repository.findById(pacienteComAcessoNegado.getId())).thenReturn(Optional.of(pacienteComAcessoNegado));
            when(securityUtil.canAccessPatient(pacienteComAcessoNegado)).thenReturn(false);

            Executable acaoBuscar = () -> pacienteService.buscarPorId(pacienteComAcessoNegado.getId());
            AccessDeniedException e = assertThrows(AccessDeniedException.class, acaoBuscar);

            assertEquals("Você não tem permissão para acessar este paciente", e.getMessage());
            verify(repository).findById(pacienteComAcessoNegado.getId());
            verify(securityUtil).canAccessPatient(pacienteComAcessoNegado);
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se paciente não for encontrado")
        void deveLancarExcecaoSePacienteNaoForEncontrado() {
            Long idInexistente = pacienteInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, () ->
                    pacienteService.buscarPorId(idInexistente)
            );

            assertEquals("Paciente com ID [" + idInexistente + "] não encontrado", e.getMessage());
            verify(repository).findById(idInexistente);
            verifyNoInteractions(securityUtil);
        }
    }

    @Nested
    @DisplayName("Buscar pacientes com filtros")
    class BuscarPacientesComFiltros {

        private static final int PAGINA = 0;
        private static final int TAMANHO = 5;
        private final Pageable pageable = PageRequest.of(PAGINA, TAMANHO, Sort.by(Sort.Direction.ASC, "nome"));

        @Test
        @DisplayName("Deve buscar por nome e ativo quando nome for informado")
        void deveBuscarPorNomeEAtivo() {
            String nome = "Ana";
            boolean ativo = true;
            when(repository.findByNomeContainingIgnoreCaseAndAtivo(nome, ativo, pageable))
                    .thenReturn(new PageImpl<>(List.of(pacienteAtivo), pageable, 1));

            PageResponse<PacienteResposta> resultado = pacienteService.buscarPacientes(PAGINA, TAMANHO, nome, null, null, ativo);

            assertNotNull(resultado);
            assertEquals(1, resultado.totalElements());
            assertEquals(1, resultado.content().size());
            assertEquals(pacienteAtivo.getId(), resultado.content().get(0).getId());
            verify(repository).findByNomeContainingIgnoreCaseAndAtivo(nome, ativo, pageable);
        }

        @Test
        @DisplayName("Deve buscar por CPF quando CPF for informado e nome não for")
        void deveBuscarPorCpf() {
            String cpf = pacienteAtivo.getCpf();
            when(repository.findByCpf(cpf)).thenReturn(Optional.of(pacienteAtivo));

            PageResponse<PacienteResposta> resultado = pacienteService.buscarPacientes(PAGINA, TAMANHO, null, cpf, null, true);

            assertNotNull(resultado);
            assertEquals(1, resultado.totalElements());
            assertEquals(pacienteAtivo.getId(), resultado.content().get(0).getId());
            verify(repository).findByCpf(cpf);
        }

        @Test
        @DisplayName("Deve retornar página vazia quando CPF não for encontrado")
        void deveRetornarPaginaVaziaQuandoCpfNaoEncontrado() {
            String cpf = "00000000000";
            when(repository.findByCpf(cpf)).thenReturn(Optional.empty());

            PageResponse<PacienteResposta> resultado = pacienteService.buscarPacientes(PAGINA, TAMANHO, null, cpf, null, true);

            assertNotNull(resultado);
            assertEquals(0, resultado.totalElements());
            assertTrue(resultado.content().isEmpty());
            verify(repository).findByCpf(cpf);
        }

        @Test
        @DisplayName("Deve buscar por sexo e ativo quando apenas sexo for informado")
        void deveBuscarPorSexoEAtivo() {
            Sexo sexo = pacienteAtivo.getSexo();
            when(repository.findBySexoAndAtivo(sexo, true, pageable))
                    .thenReturn(new PageImpl<>(List.of(pacienteAtivo), pageable, 1));

            PageResponse<PacienteResposta> resultado = pacienteService.buscarPacientes(PAGINA, TAMANHO, null, null, sexo, true);

            assertNotNull(resultado);
            assertEquals(1, resultado.content().size());
            assertEquals(pacienteAtivo.getId(), resultado.content().get(0).getId());
            verify(repository).findBySexoAndAtivo(sexo, true, pageable);
        }

        @Test
        @DisplayName("Deve buscar por ativo quando apenas ativo for informado")
        void deveBuscarPorAtivo() {
            when(repository.findByAtivo(true, pageable))
                    .thenReturn(new PageImpl<>(List.of(pacienteAtivo, pacienteComAcessoNegado), pageable, 2));

            PageResponse<PacienteResposta> resultado = pacienteService.buscarPacientes(PAGINA, TAMANHO, null, null, null, true);

            assertNotNull(resultado);
            assertEquals(2, resultado.totalElements());
            verify(repository).findByAtivo(true, pageable);
        }

        @Test
        @DisplayName("Deve buscar apenas pacientes ativos quando nenhum filtro for informado")
        void deveBuscarAtivosQuandoNenhumFiltroInformado() {
            when(repository.findByAtivo(true, pageable))
                    .thenReturn(new PageImpl<>(List.of(pacienteAtivo, pacienteComAcessoNegado), pageable, 2));

            PageResponse<PacienteResposta> resultado = pacienteService.buscarPacientes(PAGINA, TAMANHO, null, null, null, null);

            assertNotNull(resultado);
            assertEquals(2, resultado.totalElements());
            verify(repository).findByAtivo(true, pageable);
        }
    }

    @Nested
    @DisplayName("Atualizar paciente")
    class AtualizarPaciente {

        @Test
        @DisplayName("Deve atualizar paciente com sucesso")
        void deveAtualizarPacienteComSucesso() {
            Paciente atualizado = new Paciente(
                    "Ana Maria",
                    "ana.novo@paciente.com",
                    "55555555555",
                    "33333333333",
                    Sexo.FEMININO,
                    LocalDate.of(1985, 1, 1)
            );
            atualizado.setId(pacienteAtivo.getId());
            atualizado.setAtivo(false); // Deve ser ignorado

            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);
            when(repository.save(any(Paciente.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Paciente salvo = pacienteService.atualizar(pacienteAtivo.getId(), atualizado);

            assertNotNull(salvo);
            assertEquals(atualizado.getNome(), salvo.getNome());
            assertEquals(atualizado.getEmail(), salvo.getEmail());
            assertEquals(atualizado.getTelefone(), salvo.getTelefone());
            assertEquals(atualizado.getCpf(), salvo.getCpf());
            assertEquals(LocalDate.of(1985, 1, 1), salvo.getDataNascimento());
            assertTrue(salvo.getAtivo()); // Deve permanecer ativo
            verify(repository).save(pacienteAtivo);
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se paciente não existir")
        void deveLancarExcecaoSePacienteNaoExistir() {
            Long idInexistente = pacienteInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            Executable acaoAtualizarInexistente = () -> pacienteService.atualizar(idInexistente, new Paciente());
            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, acaoAtualizarInexistente);

            assertEquals("Paciente com ID [" + idInexistente + "] não encontrado", e.getMessage());
            verify(repository).findById(idInexistente);
        }

        @Test
        @DisplayName("Deve lançar AccessDeniedException se acesso for negado")
        void deveLancarExcecaoSeAcessoNegado() {
            when(repository.findById(pacienteComAcessoNegado.getId())).thenReturn(Optional.of(pacienteComAcessoNegado));
            when(securityUtil.canAccessPatient(pacienteComAcessoNegado)).thenReturn(false);

            Executable acaoAtualizarSemPermissao = () -> pacienteService.atualizar(pacienteComAcessoNegado.getId(), new Paciente());
            AccessDeniedException e = assertThrows(AccessDeniedException.class, acaoAtualizarSemPermissao);

            assertEquals("Você não tem permissão para acessar este paciente", e.getMessage());
            verify(repository).findById(pacienteComAcessoNegado.getId());
            verify(securityUtil).canAccessPatient(pacienteComAcessoNegado);
        }

        @Test
        @DisplayName("Não deve alterar o campo ativo mesmo se informado no paciente atualizado")
        void naoDeveAlterarCampoAtivo() {
            Paciente atualizado = new Paciente(
                    pacienteAtivo.getNome(),
                    pacienteAtivo.getEmail(),
                    pacienteAtivo.getTelefone(),
                    pacienteAtivo.getCpf(),
                    pacienteAtivo.getSexo(),
                    pacienteAtivo.getDataNascimento()
            );
            atualizado.setAtivo(false); // Tentando desativar

            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);
            when(repository.save(any(Paciente.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Paciente salvo = pacienteService.atualizar(pacienteAtivo.getId(), atualizado);

            assertTrue(salvo.getAtivo()); // Continua ativo
        }
    }

    @Nested
    @DisplayName("Remover paciente por ID")
    class RemoverPorId {

        @Test
        @DisplayName("Deve excluir paciente quando estiver inativo")
        void deveExcluirPacienteQuandoInativo() {
            when(repository.findById(pacienteInativo.getId())).thenReturn(Optional.of(pacienteInativo));
            when(securityUtil.canAccessPatient(pacienteInativo)).thenReturn(true);
            doNothing().when(repository).deleteById(pacienteInativo.getId());

            assertDoesNotThrow(() -> pacienteService.removerPorId(pacienteInativo.getId()));

            verify(repository, times(1)).deleteById(pacienteInativo.getId());
        }

        @Test
        @DisplayName("Deve lançar BusinessRuleException se paciente estiver ativo")
        void deveLancarExceptionSePacienteAtivo() {
            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);

            Executable acaoRemoverAtivo = () -> pacienteService.removerPorId(pacienteAtivo.getId());
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, acaoRemoverAtivo);

            assertEquals("Paciente deve estar inativo para ser excluído.", ex.getMessage());
            verify(repository, never()).deleteById(anyLong());
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se paciente não existir")
        void deveLancarExceptionSePacienteNaoExistir() {
            Long idInexistente = pacienteInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    pacienteService.removerPorId(idInexistente)
            );

            assertTrue(ex.getMessage().contains("Paciente com ID [" + idInexistente + "] não encontrado"));
            verify(repository, never()).deleteById(anyLong());
        }
    }

    @Nested
    @DisplayName("Inativar paciente por ID")
    class InativarPorId {

        @Test
        @DisplayName("Deve inativar paciente com sucesso se não tiver consultas futuras")
        void deveInativarPacienteComSucesso() {
            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);
            doNothing().when(pacienteRules).verificarSeNaoTemConsultasFuturas(pacienteAtivo);
            when(repository.save(any(Paciente.class))).thenAnswer(invocation -> invocation.getArgument(0));

            pacienteService.inativarPorId(pacienteAtivo.getId());

            assertFalse(pacienteAtivo.getAtivo());
            verify(repository).save(pacienteAtivo);
        }

        @Test
        @DisplayName("Não deve alterar paciente se já estiver inativo")
        void naoDeveInativarPacienteQueJaEstaInativo() {
            when(repository.findById(pacienteInativo.getId())).thenReturn(Optional.of(pacienteInativo));
            when(securityUtil.canAccessPatient(pacienteInativo)).thenReturn(true);
            doNothing().when(pacienteRules).verificarSeNaoTemConsultasFuturas(pacienteInativo);

            pacienteService.inativarPorId(pacienteInativo.getId());

            assertFalse(pacienteInativo.getAtivo());
            verify(repository, never()).save(any(Paciente.class));
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se paciente não existir")
        void deveLancarExceptionSePacienteNaoExistir() {
            Long idInexistente = pacienteInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    pacienteService.inativarPorId(idInexistente)
            );

            assertTrue(ex.getMessage().contains("Paciente com ID [" + idInexistente + "] não encontrado"));
        }

        @Test
        @DisplayName("Deve lançar exceção se paciente tiver consultas futuras")
        void deveLancarExceptionSePacienteTiverConsultasFuturas() {
            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);
            doThrow(new BusinessRuleException("Paciente possui consultas futuras"))
                    .when(pacienteRules).verificarSeNaoTemConsultasFuturas(pacienteAtivo);

            Executable acao = () -> pacienteService.inativarPorId(pacienteAtivo.getId());
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, acao);

            assertEquals("Paciente possui consultas futuras", ex.getMessage());
            verify(repository, never()).save(any(Paciente.class));
        }

        @Test
        @DisplayName("Deve lançar AccessDeniedException se acesso for negado")
        void deveLancarAccessDeniedSeAcessoNegado() {
            when(repository.findById(pacienteComAcessoNegado.getId())).thenReturn(Optional.of(pacienteComAcessoNegado));
            when(securityUtil.canAccessPatient(pacienteComAcessoNegado)).thenReturn(false);

            Executable acaoInativar = () -> pacienteService.inativarPorId(pacienteComAcessoNegado.getId());
            AccessDeniedException ex = assertThrows(AccessDeniedException.class, acaoInativar);

            assertEquals("Você não tem permissão para acessar este paciente", ex.getMessage());
            verify(repository, never()).save(any(Paciente.class));
        }
    }

    @Nested
    @DisplayName("Ativar paciente por ID")
    class AtivarPorId {

        @Test
        @DisplayName("Deve ativar paciente com sucesso se estiver inativo")
        void deveAtivarPacienteComSucesso() {
            pacienteInativo.setAtivo(false);
            when(repository.findById(pacienteInativo.getId())).thenReturn(Optional.of(pacienteInativo));
            when(securityUtil.canAccessPatient(pacienteInativo)).thenReturn(true);
            when(repository.save(any(Paciente.class))).thenAnswer(invocation -> invocation.getArgument(0));

            pacienteService.ativarPorId(pacienteInativo.getId());

            assertTrue(pacienteInativo.getAtivo());
            verify(repository).save(pacienteInativo);
        }

        @Test
        @DisplayName("Não deve alterar paciente se já estiver ativo")
        void naoDeveAtivarPacienteQueJaEstaAtivo() {
            when(repository.findById(pacienteAtivo.getId())).thenReturn(Optional.of(pacienteAtivo));
            when(securityUtil.canAccessPatient(pacienteAtivo)).thenReturn(true);

            pacienteService.ativarPorId(pacienteAtivo.getId());

            assertTrue(pacienteAtivo.getAtivo());
            verify(repository, never()).save(any(Paciente.class));
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se paciente não existir")
        void deveLancarExceptionSePacienteNaoExistir() {
            Long idInexistente = pacienteInexistente.getId();
            when(repository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    pacienteService.ativarPorId(idInexistente)
            );

            assertTrue(ex.getMessage().contains("Paciente com ID [" + idInexistente + "] não encontrado"));
        }

        @Test
        @DisplayName("Deve lançar AccessDeniedException se acesso for negado")
        void deveLancarAccessDeniedSeAcessoNegado() {
            when(repository.findById(pacienteComAcessoNegado.getId())).thenReturn(Optional.of(pacienteComAcessoNegado));
            when(securityUtil.canAccessPatient(pacienteComAcessoNegado)).thenReturn(false);

            Executable acaoAtivar = () -> pacienteService.ativarPorId(pacienteComAcessoNegado.getId());
            AccessDeniedException ex = assertThrows(AccessDeniedException.class, acaoAtivar);

            assertEquals("Você não tem permissão para acessar este paciente", ex.getMessage());
            verify(repository, never()).save(any(Paciente.class));
        }
    }

    @Nested
    @DisplayName("Buscar paciente por username e ativo")
    class BuscarPorUsernameEAtivo {

        @Test
        @DisplayName("Deve retornar paciente se username e ativo forem correspondentes")
        void deveRetornarPacienteAtivoPorUsername() {
            String username = pacienteAtivo.getUsuario().getUsername();
            when(repository.findByUsuarioUsernameAndAtivo(username, true))
                    .thenReturn(Optional.of(pacienteAtivo));

            Paciente resultado = pacienteService.buscarPorUsernameEAtivo(username, true);

            assertNotNull(resultado);
            assertEquals(pacienteAtivo, resultado);
        }

        @Test
        @DisplayName("Deve retornar paciente inativo se username e ativo forem correspondentes")
        void deveRetornarPacienteInativoPorUsername() {
            String username = pacienteInativo.getUsuario().getUsername();
            when(repository.findByUsuarioUsernameAndAtivo(username, false))
                    .thenReturn(Optional.of(pacienteInativo));

            Paciente resultado = pacienteService.buscarPorUsernameEAtivo(username, false);

            assertNotNull(resultado);
            assertEquals(pacienteInativo, resultado);
        }

        @Test
        @DisplayName("Deve lançar exceção se paciente não for encontrado pelo username e ativo")
        void deveLancarExcecaoSeNaoEncontrarPacientePorUsernameEAtivo() {
            String usernameInexistente = "nao.existe";
            when(repository.findByUsuarioUsernameAndAtivo(usernameInexistente, true))
                    .thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    pacienteService.buscarPorUsernameEAtivo(usernameInexistente, true)
            );

            assertEquals("Paciente associado ao usuário não encontrado", ex.getMessage());
        }
    }
}
