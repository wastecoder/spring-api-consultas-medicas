package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.rules.UsuarioRules;
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
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Usuário")
class UsuarioServiceImplTest {

    @InjectMocks
    private UsuarioServiceImpl usuarioService;

    @Mock
    private UsuarioRepository usuarioRepository;

    @Mock
    private PasswordEncoder passwordEncoder;

    @Mock
    private UsuarioRules usuarioRules;

    private Usuario usuarioAdmin;
    private Usuario usuarioRecepcionistaValido;
    private Usuario usuarioMedicoValido;
    private Usuario usuarioPacienteValido;

    private UsuarioCadastroDto recepcionistaComIdAssociado;
    private UsuarioCadastroDto adminComIdAssociado;
    private UsuarioCadastroDto medicoSemIdAssociado;
    private UsuarioCadastroDto medicoComAssociacaoExistente;
    private UsuarioCadastroDto pacienteSemIdAssociado;
    private UsuarioCadastroDto pacienteComAssociacaoExistente;
    private UsuarioCadastroDto usuarioDuplicado;

    private Medico medicoSemAssociacao;
    private Paciente pacienteSemAssociacao;

    private static final String SENHA_VALIDA = "senha123";
    private static final String SENHA_CODIFICADA = "senhaCodificada";
    private static final Long ID_INEXISTENTE = 999L;

    @BeforeEach
    void setUp() {
        criarUsuarioAdmin();
        criarUsuarioRecepcionistaValido();
        criarUsuarioMedicoValido();
        criarUsuarioPacienteValido();

        criarRecepcionistaComIdAssociado();
        criarAdminComIdAssociado();
        criarMedicoSemIdAssociado();
        criarMedicoComMedicoJaAssociado();

        criarPacienteSemIdAssociado();
        criarPacienteComPacienteJaAssociado();
        criarUsuarioDuplicado();
    }

    private void criarUsuarioAdmin() {
        usuarioAdmin = Usuario.builder()
                .id(1L)
                .username("admin")
                .senha("encodedSenha")
                .funcao(Funcao.ADMIN)
                .ativo(true)
                .build();
    }

    private void criarUsuarioRecepcionistaValido() {
        usuarioRecepcionistaValido = Usuario.builder()
                .id(2L)
                .username("recep.valida")
                .senha("encodedSenha")
                .funcao(Funcao.RECEPCIONISTA)
                .ativo(true)
                .build();
    }

    private void criarUsuarioMedicoValido() {
        usuarioMedicoValido = Usuario.builder()
                .id(3L)
                .username("medico.valido")
                .senha("encodedSenha")
                .funcao(Funcao.MEDICO)
                .ativo(true)
                .build();

        medicoSemAssociacao = new Medico();
        medicoSemAssociacao.setId(100L);
        medicoSemAssociacao.setNome("Médico Associável");
        medicoSemAssociacao.setUsuario(null);
    }

    private void criarUsuarioPacienteValido() {
        usuarioPacienteValido = Usuario.builder()
                .id(4L)
                .username("paciente.valido")
                .senha("encodedSenha")
                .funcao(Funcao.PACIENTE)
                .ativo(true)
                .build();

        pacienteSemAssociacao = new Paciente();
        pacienteSemAssociacao.setId(200L);
        pacienteSemAssociacao.setNome("Paciente Associável");
        pacienteSemAssociacao.setUsuario(null);
    }

    private void criarRecepcionistaComIdAssociado() {
        recepcionistaComIdAssociado = UsuarioCadastroDto.builder()
                .username("recep.invalida")
                .senha(SENHA_VALIDA)
                .funcao(Funcao.RECEPCIONISTA)
                .idAssociado(ID_INEXISTENTE)
                .build();
    }

    private void criarAdminComIdAssociado() {
        adminComIdAssociado = UsuarioCadastroDto.builder()
                .username("admin.com.id")
                .senha(SENHA_VALIDA)
                .funcao(Funcao.ADMIN)
                .idAssociado(ID_INEXISTENTE)
                .build();
    }

    private void criarMedicoSemIdAssociado() {
        medicoSemIdAssociado = UsuarioCadastroDto.builder()
                .username("medico.sem.id")
                .senha(SENHA_VALIDA)
                .funcao(Funcao.MEDICO)
                .idAssociado(null)
                .build();
    }

    private void criarMedicoComMedicoJaAssociado() {
        medicoComAssociacaoExistente = UsuarioCadastroDto.builder()
                .username("medico.ja.associado")
                .senha(SENHA_VALIDA)
                .funcao(Funcao.MEDICO)
                .idAssociado(101L)
                .build();

        Usuario jaAssociado = new Usuario();
        jaAssociado.setId(9L);
        jaAssociado.setUsername("usuario.medico.antigo");

        Medico medicoComAssociacao = new Medico();
        medicoComAssociacao.setId(101L);
        medicoComAssociacao.setNome("Médico Já Associado");
        medicoComAssociacao.setUsuario(jaAssociado);
    }

    private void criarPacienteSemIdAssociado() {
        pacienteSemIdAssociado = UsuarioCadastroDto.builder()
                .username("paciente.sem.id")
                .senha(SENHA_VALIDA)
                .funcao(Funcao.PACIENTE)
                .idAssociado(null)
                .build();
    }

    private void criarPacienteComPacienteJaAssociado() {
        pacienteComAssociacaoExistente = UsuarioCadastroDto.builder()
                .username("paciente.ja.associado")
                .senha(SENHA_VALIDA)
                .funcao(Funcao.PACIENTE)
                .idAssociado(202L)
                .build();

        Usuario jaAssociado = new Usuario();
        jaAssociado.setId(10L);
        jaAssociado.setUsername("usuario.paciente.antigo");

        Paciente pacienteComAssociacao = new Paciente();
        pacienteComAssociacao.setId(202L);
        pacienteComAssociacao.setNome("Paciente Já Associado");
        pacienteComAssociacao.setUsuario(jaAssociado);
    }

    private void criarUsuarioDuplicado() {
        usuarioDuplicado = UsuarioCadastroDto.builder()
                .username(usuarioAdmin.getUsername())
                .senha(SENHA_VALIDA)
                .funcao(Funcao.ADMIN)
                .idAssociado(null)
                .build();
    }


    @Nested
    @DisplayName("Salvar usuário")
    class SalvarUsuario {

        private UsuarioCadastroDto converterParaCadastroDto(Usuario usuario, Long idAssociado) {
            return UsuarioCadastroDto.builder()
                    .username(usuario.getUsername())
                    .senha(SENHA_VALIDA)
                    .funcao(usuario.getFuncao())
                    .idAssociado(idAssociado)
                    .build();
        }

        @Test
        @DisplayName("Deve salvar ADMIN com sucesso")
        void deveSalvarAdminComSucesso() {
            UsuarioCadastroDto dto = converterParaCadastroDto(usuarioAdmin, null);

            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(usuarioAdmin.getSenha());
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(usuarioAdmin);

            UsuarioResposta resposta = usuarioService.salvar(dto);

            assertNotNull(resposta);
            assertEquals(usuarioAdmin.getUsername(), resposta.getUsername());
            verify(usuarioRules).validarUsernameDuplicado(dto.getUsername(), null);
            verify(usuarioRules).validarRegrasDeAssociacao(dto);
            verify(usuarioRepository).save(any(Usuario.class));
        }

        @Test
        @DisplayName("Deve salvar RECEPCIONISTA com sucesso (sem associação)")
        void deveSalvarRecepcionistaComSucesso() {
            UsuarioCadastroDto dto = converterParaCadastroDto(usuarioRecepcionistaValido, null);

            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(usuarioRecepcionistaValido.getSenha());
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(usuarioRecepcionistaValido);

            UsuarioResposta resposta = usuarioService.salvar(dto);

            assertNotNull(resposta);
            assertEquals(usuarioRecepcionistaValido.getUsername(), resposta.getUsername());
            verify(usuarioRules).validarRegrasDeAssociacao(dto);
        }

        @Test
        @DisplayName("Deve salvar MÉDICO com sucesso (com ID associado)")
        void deveSalvarMedicoComSucesso() {
            UsuarioCadastroDto dto = converterParaCadastroDto(usuarioMedicoValido, medicoSemAssociacao.getId());

            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(usuarioMedicoValido.getSenha());
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(usuarioMedicoValido);

            doNothing().when(usuarioRules).validarUsernameDuplicado(anyString(), any());
            doNothing().when(usuarioRules).validarRegrasDeAssociacao(any());
            doNothing().when(usuarioRules).associarUsuarioAoMedicoOuPaciente(any(), any());

            UsuarioResposta resposta = usuarioService.salvar(dto);

            assertNotNull(resposta);
            verify(usuarioRules).validarUsernameDuplicado(dto.getUsername(), null);
            verify(usuarioRules).validarRegrasDeAssociacao(dto);
            verify(usuarioRules).associarUsuarioAoMedicoOuPaciente(dto, usuarioMedicoValido);
        }

        @Test
        @DisplayName("Deve salvar PACIENTE com sucesso (com ID associado)")
        void deveSalvarPacienteComSucesso() {
            UsuarioCadastroDto dto = converterParaCadastroDto(usuarioPacienteValido, pacienteSemAssociacao.getId());

            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(usuarioPacienteValido.getSenha());
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(usuarioPacienteValido);

            doNothing().when(usuarioRules).validarUsernameDuplicado(anyString(), any());
            doNothing().when(usuarioRules).validarRegrasDeAssociacao(any());
            doNothing().when(usuarioRules).associarUsuarioAoMedicoOuPaciente(any(), any());

            UsuarioResposta resposta = usuarioService.salvar(dto);

            assertNotNull(resposta);
            verify(usuarioRules).validarUsernameDuplicado(dto.getUsername(), null);
            verify(usuarioRules).validarRegrasDeAssociacao(dto);
            verify(usuarioRules).associarUsuarioAoMedicoOuPaciente(dto, usuarioPacienteValido);
        }

        @Test
        @DisplayName("Deve lançar exceção se username já estiver em uso")
        void deveLancarExcecaoSeUsernameDuplicado() {
            doThrow(new BusinessRuleException("Nome de usuário já está em uso: " + usuarioDuplicado.getUsername()))
                    .when(usuarioRules).validarUsernameDuplicado(usuarioDuplicado.getUsername(), null);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class, () -> usuarioService.salvar(usuarioDuplicado));

            assertEquals("Nome de usuário já está em uso: " + usuarioDuplicado.getUsername(), ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se RECEPCIONISTA tiver idAssociado")
        void deveLancarExcecaoSeRecepcionistaComAssociacao() {
            doThrow(new BusinessRuleException("Recepcionista não pode estar associado a médico ou paciente."))
                    .when(usuarioRules).validarRegrasDeAssociacao(recepcionistaComIdAssociado);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(recepcionistaComIdAssociado));

            assertEquals("Recepcionista não pode estar associado a médico ou paciente.", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se ADMIN tiver idAssociado")
        void deveLancarExcecaoSeAdminComAssociacao() {
            doThrow(new BusinessRuleException("Admin não pode estar associado a médico ou paciente."))
                    .when(usuarioRules).validarRegrasDeAssociacao(adminComIdAssociado);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(adminComIdAssociado));

            assertEquals("Admin não pode estar associado a médico ou paciente.", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se MÉDICO não tiver idAssociado")
        void deveLancarExcecaoSeMedicoSemIdAssociado() {
            doThrow(new BusinessRuleException("Função MEDICO exige ID de associação."))
                    .when(usuarioRules).validarRegrasDeAssociacao(medicoSemIdAssociado);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(medicoSemIdAssociado));

            assertEquals("Função MEDICO exige ID de associação.", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se PACIENTE não tiver idAssociado")
        void deveLancarExcecaoSePacienteSemIdAssociado() {
            doThrow(new BusinessRuleException("Função PACIENTE exige ID de associação."))
                    .when(usuarioRules).validarRegrasDeAssociacao(pacienteSemIdAssociado);

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(pacienteSemIdAssociado));

            assertEquals("Função PACIENTE exige ID de associação.", ex.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se médico já estiver associado a outro usuário")
        void deveLancarExcecaoSeMedicoJaAssociado() {
            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(SENHA_CODIFICADA);
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(new Usuario());

            doThrow(new BusinessRuleException("Este médico já está associado a um usuário."))
                    .when(usuarioRules).associarUsuarioAoMedicoOuPaciente(any(), any());

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(medicoComAssociacaoExistente));

            assertEquals("Este médico já está associado a um usuário.", ex.getMessage());
            verify(usuarioRules).associarUsuarioAoMedicoOuPaciente(eq(medicoComAssociacaoExistente), any(Usuario.class));
        }

        @Test
        @DisplayName("Deve lançar exceção se paciente já estiver associado a outro usuário")
        void deveLancarExcecaoSePacienteJaAssociado() {
            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(SENHA_CODIFICADA);
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(new Usuario());

            doThrow(new BusinessRuleException("Este paciente já está associado a um usuário."))
                    .when(usuarioRules).associarUsuarioAoMedicoOuPaciente(any(), any());

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(pacienteComAssociacaoExistente));

            assertEquals("Este paciente já está associado a um usuário.", ex.getMessage());
            verify(usuarioRules).associarUsuarioAoMedicoOuPaciente(eq(pacienteComAssociacaoExistente), any(Usuario.class));
        }

        @Test
        @DisplayName("Deve lançar exceção se médico com ID associado não for encontrado")
        void deveLancarExcecaoSeMedicoNaoEncontradoPorId() {
            UsuarioCadastroDto dto = UsuarioCadastroDto.builder()
                    .username("medico.invalido.id")
                    .senha(SENHA_VALIDA)
                    .funcao(Funcao.MEDICO)
                    .idAssociado(ID_INEXISTENTE)
                    .build();

            when(passwordEncoder.encode(SENHA_VALIDA)).thenReturn(SENHA_CODIFICADA);
            when(usuarioRepository.save(any(Usuario.class))).thenReturn(new Usuario());

            doNothing().when(usuarioRules).validarUsernameDuplicado(dto.getUsername(), null);
            doNothing().when(usuarioRules).validarRegrasDeAssociacao(dto);

            doThrow(new BusinessRuleException("Médico não encontrado com ID: " + ID_INEXISTENTE))
                    .when(usuarioRules).associarUsuarioAoMedicoOuPaciente(eq(dto), any(Usuario.class));

            BusinessRuleException ex = assertThrows(BusinessRuleException.class,
                    () -> usuarioService.salvar(dto));

            assertEquals("Médico não encontrado com ID: " + ID_INEXISTENTE, ex.getMessage());
            verify(usuarioRules).associarUsuarioAoMedicoOuPaciente(eq(dto), any(Usuario.class));
        }
    }

    @Nested
    @DisplayName("Buscar todos os usuários")
    class BuscarTodosUsuarios {

        @Test
        @DisplayName("Deve retornar lista vazia quando não houver usuários")
        void deveRetornarListaVazia() {
            when(usuarioRepository.findAll()).thenReturn(Collections.emptyList());

            List<Usuario> usuarios = usuarioService.buscarTodos();

            assertNotNull(usuarios);
            assertTrue(usuarios.isEmpty());
            verify(usuarioRepository).findAll();
        }

        @Test
        @DisplayName("Deve retornar lista com usuários")
        void deveRetornarListaComUsuarios() {
            List<Usuario> listaMock = List.of(usuarioAdmin, usuarioMedicoValido);

            when(usuarioRepository.findAll()).thenReturn(listaMock);

            List<Usuario> usuarios = usuarioService.buscarTodos();

            assertNotNull(usuarios);
            assertEquals(2, usuarios.size());
            assertTrue(usuarios.contains(usuarioAdmin));
            assertTrue(usuarios.contains(usuarioMedicoValido));
            verify(usuarioRepository).findAll();
        }
    }

    @Nested
    @DisplayName("Buscar usuário por ID")
    class BuscarUsuarioPorId {

        @Test
        @DisplayName("Deve retornar usuário se encontrado")
        void deveRetornarUsuarioQuandoEncontrado() {
            when(usuarioRepository.findById(usuarioAdmin.getId())).thenReturn(Optional.of(usuarioAdmin));

            Usuario encontrado = usuarioService.buscarPorId(usuarioAdmin.getId());

            assertNotNull(encontrado);
            assertEquals(usuarioAdmin.getId(), encontrado.getId());
            assertEquals(usuarioAdmin.getUsername(), encontrado.getUsername());
            verify(usuarioRepository).findById(usuarioAdmin.getId());
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se usuário não for encontrado")
        void deveLancarExcecaoSeUsuarioNaoForEncontrado() {
            when(usuarioRepository.findById(ID_INEXISTENTE)).thenReturn(Optional.empty());

            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, () ->
                    usuarioService.buscarPorId(ID_INEXISTENTE)
            );

            assertEquals("Usuário com ID [" + ID_INEXISTENTE + "] não encontrado", e.getMessage());
            verify(usuarioRepository).findById(ID_INEXISTENTE);
        }
    }

    @Nested
    @DisplayName("Atualizar usuário")
    class AtualizarUsuario {

        private UsuarioAtualizacaoDto criarDtoAtualizacao(Usuario usuario, String novaSenha) {
            return UsuarioAtualizacaoDto.builder()
                    .username(usuario.getUsername())
                    .senha(novaSenha)
                    .build();
        }

        @Test
        @DisplayName("Deve atualizar usuário com sucesso, incluindo alteração de senha")
        void deveAtualizarUsuarioComSucesso() {
            UsuarioAtualizacaoDto dto = criarDtoAtualizacao(usuarioAdmin, "novaSenha123");

            when(usuarioRepository.findById(usuarioAdmin.getId())).thenReturn(Optional.of(usuarioAdmin));
            doNothing().when(usuarioRules).validarUsernameDuplicado(dto.getUsername(), usuarioAdmin.getId());
            when(passwordEncoder.encode(dto.getSenha())).thenReturn(SENHA_CODIFICADA);
            when(usuarioRepository.save(any(Usuario.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Usuario atualizado = usuarioService.atualizar(usuarioAdmin.getId(), dto);

            assertNotNull(atualizado);
            assertEquals(dto.getUsername(), atualizado.getUsername());
            assertEquals(SENHA_CODIFICADA, atualizado.getSenha());

            verify(usuarioRepository).findById(usuarioAdmin.getId());
            verify(usuarioRules).validarUsernameDuplicado(dto.getUsername(), usuarioAdmin.getId());
            verify(passwordEncoder).encode(dto.getSenha());
            verify(usuarioRepository).save(usuarioAdmin);
        }

        @Test
        @DisplayName("Deve permitir atualizar usuário mantendo o mesmo username")
        void devePermitirAtualizarMesmoUsername() {
            UsuarioAtualizacaoDto dtoMesmoUsername = criarDtoAtualizacao(usuarioAdmin, "novaSenha456");

            when(usuarioRepository.findById(usuarioAdmin.getId())).thenReturn(Optional.of(usuarioAdmin));
            doNothing().when(usuarioRules).validarUsernameDuplicado(usuarioAdmin.getUsername(), usuarioAdmin.getId());
            when(passwordEncoder.encode(dtoMesmoUsername.getSenha())).thenReturn(SENHA_CODIFICADA);
            when(usuarioRepository.save(any(Usuario.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Usuario atualizado = usuarioService.atualizar(usuarioAdmin.getId(), dtoMesmoUsername);

            assertNotNull(atualizado);
            assertEquals(usuarioAdmin.getUsername(), atualizado.getUsername());
            assertEquals(SENHA_CODIFICADA, atualizado.getSenha());

            verify(usuarioRepository).findById(usuarioAdmin.getId());
            verify(usuarioRules).validarUsernameDuplicado(usuarioAdmin.getUsername(), usuarioAdmin.getId());
            verify(passwordEncoder).encode(dtoMesmoUsername.getSenha());
            verify(usuarioRepository).save(usuarioAdmin);
        }

        @Test
        @DisplayName("Deve atualizar usuário sem alterar senha se senha for nula")
        void deveAtualizarUsuarioSemAlterarSenha() {
            UsuarioAtualizacaoDto dtoSemSenha = criarDtoAtualizacao(usuarioAdmin, null);

            when(usuarioRepository.findById(usuarioAdmin.getId())).thenReturn(Optional.of(usuarioAdmin));
            doNothing().when(usuarioRules).validarUsernameDuplicado(dtoSemSenha.getUsername(), usuarioAdmin.getId());
            when(usuarioRepository.save(any(Usuario.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Usuario atualizado = usuarioService.atualizar(usuarioAdmin.getId(), dtoSemSenha);

            assertNotNull(atualizado);
            assertEquals(dtoSemSenha.getUsername(), atualizado.getUsername());
            assertEquals(usuarioAdmin.getSenha(), atualizado.getSenha()); // senha não mudou

            verify(usuarioRepository).findById(usuarioAdmin.getId());
            verify(usuarioRules).validarUsernameDuplicado(dtoSemSenha.getUsername(), usuarioAdmin.getId());
            verify(passwordEncoder, never()).encode(anyString());
            verify(usuarioRepository).save(usuarioAdmin);
        }

        @Test
        @DisplayName("Deve atualizar usuário sem alterar senha se senha for apenas espaços em branco")
        void deveAtualizarUsuarioSemAlterarSenhaSeSenhaEmBranco() {
            UsuarioAtualizacaoDto dtoSenhaBranca = criarDtoAtualizacao(usuarioAdmin, "   ");

            when(usuarioRepository.findById(usuarioAdmin.getId())).thenReturn(Optional.of(usuarioAdmin));
            doNothing().when(usuarioRules).validarUsernameDuplicado(dtoSenhaBranca.getUsername(), usuarioAdmin.getId());
            when(usuarioRepository.save(any(Usuario.class))).thenAnswer(invocation -> invocation.getArgument(0));

            Usuario atualizado = usuarioService.atualizar(usuarioAdmin.getId(), dtoSenhaBranca);

            assertNotNull(atualizado);
            assertEquals(dtoSenhaBranca.getUsername(), atualizado.getUsername());
            assertEquals(usuarioAdmin.getSenha(), atualizado.getSenha()); // senha não mudou

            verify(usuarioRepository).findById(usuarioAdmin.getId());
            verify(usuarioRules).validarUsernameDuplicado(dtoSenhaBranca.getUsername(), usuarioAdmin.getId());
            verify(passwordEncoder, never()).encode(anyString());
            verify(usuarioRepository).save(usuarioAdmin);
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se usuário não existir")
        void deveLancarExcecaoSeUsuarioNaoExistir() {
            UsuarioAtualizacaoDto dto = criarDtoAtualizacao(usuarioAdmin, "senha");

            when(usuarioRepository.findById(ID_INEXISTENTE)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    usuarioService.atualizar(ID_INEXISTENTE, dto));

            assertEquals("Usuário com ID [" + ID_INEXISTENTE + "] não encontrado", ex.getMessage());

            verify(usuarioRepository).findById(ID_INEXISTENTE);
            verify(usuarioRules, never()).validarUsernameDuplicado(anyString(), any());
            verify(passwordEncoder, never()).encode(anyString());
            verify(usuarioRepository, never()).save(any());
        }

        @Test
        @DisplayName("Deve lançar BusinessRuleException se username duplicado")
        void deveLancarExcecaoSeUsernameDuplicado() {
            UsuarioAtualizacaoDto dto = criarDtoAtualizacao(usuarioAdmin, "senha");

            when(usuarioRepository.findById(usuarioAdmin.getId())).thenReturn(Optional.of(usuarioAdmin));
            doThrow(new BusinessRuleException("Nome de usuário já está em uso"))
                    .when(usuarioRules).validarUsernameDuplicado(dto.getUsername(), usuarioAdmin.getId());

            Executable acao = () -> usuarioService.atualizar(usuarioAdmin.getId(), dto);
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, acao);

            assertEquals("Nome de usuário já está em uso", ex.getMessage());

            verify(usuarioRepository).findById(usuarioAdmin.getId());
            verify(usuarioRules).validarUsernameDuplicado(dto.getUsername(), usuarioAdmin.getId());
            verify(passwordEncoder, never()).encode(anyString());
            verify(usuarioRepository, never()).save(any());
        }
    }

    @Nested
    @DisplayName("Remover usuário por ID")
    class RemoverUsuarioPorId {

        private Usuario criarUsuarioSimples(Long id, Boolean ativo) {
            return Usuario.builder()
                    .id(id)
                    .ativo(ativo)
                    .build();
        }

        @Test
        @DisplayName("Deve excluir usuário quando estiver inativo")
        void deveExcluirUsuarioQuandoInativo() {
            Usuario usuarioInativo = criarUsuarioSimples(1L, false);

            when(usuarioRepository.findById(usuarioInativo.getId())).thenReturn(Optional.of(usuarioInativo));
            doNothing().when(usuarioRules).desassociarUsuarioDePessoa(usuarioInativo);
            doNothing().when(usuarioRepository).deleteById(usuarioInativo.getId());

            assertDoesNotThrow(() -> usuarioService.removerPorId(usuarioInativo.getId()));

            verify(usuarioRepository).findById(usuarioInativo.getId());
            verify(usuarioRules).desassociarUsuarioDePessoa(usuarioInativo);
            verify(usuarioRepository).deleteById(usuarioInativo.getId());
        }

        @Test
        @DisplayName("Deve lançar BusinessRuleException se usuário estiver ativo")
        void deveLancarExcecaoSeUsuarioAtivo() {
            Usuario usuarioAtivo = criarUsuarioSimples(2L, true);

            when(usuarioRepository.findById(usuarioAtivo.getId())).thenReturn(Optional.of(usuarioAtivo));

            Executable acao = () -> usuarioService.removerPorId(usuarioAtivo.getId());
            BusinessRuleException ex = assertThrows(BusinessRuleException.class, acao);

            assertEquals("Usuário deve estar inativo para ser excluído.", ex.getMessage());

            verify(usuarioRepository).findById(usuarioAtivo.getId());
            verify(usuarioRules, never()).desassociarUsuarioDePessoa(any());
            verify(usuarioRepository, never()).deleteById(anyLong());
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se usuário não existir")
        void deveLancarExcecaoSeUsuarioNaoExistir() {
            when(usuarioRepository.findById(ID_INEXISTENTE)).thenReturn(Optional.empty());

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class,
                    () -> usuarioService.removerPorId(ID_INEXISTENTE));

            assertEquals("Usuário com ID [" + ID_INEXISTENTE + "] não encontrado", ex.getMessage());

            verify(usuarioRepository).findById(ID_INEXISTENTE);
            verify(usuarioRules, never()).desassociarUsuarioDePessoa(any());
            verify(usuarioRepository, never()).deleteById(anyLong());
        }

        @Test
        @DisplayName("Deve excluir usuário mesmo se atributo 'ativo' for null (tratado como inativo)")
        void deveExcluirUsuarioSeAtivoForNull() {
            Usuario usuarioComAtivoNull = criarUsuarioSimples(42L, null);

            when(usuarioRepository.findById(usuarioComAtivoNull.getId())).thenReturn(Optional.of(usuarioComAtivoNull));
            doNothing().when(usuarioRules).desassociarUsuarioDePessoa(usuarioComAtivoNull);
            doNothing().when(usuarioRepository).deleteById(usuarioComAtivoNull.getId());

            assertDoesNotThrow(() -> usuarioService.removerPorId(usuarioComAtivoNull.getId()));

            verify(usuarioRepository).findById(usuarioComAtivoNull.getId());
            verify(usuarioRules).desassociarUsuarioDePessoa(usuarioComAtivoNull);
            verify(usuarioRepository).deleteById(usuarioComAtivoNull.getId());
        }
    }

    @Nested
    @DisplayName("Inativar usuário por ID")
    class InativarUsuarioPorId {

        @Test
        @DisplayName("Deve inativar usuário ativo com sucesso")
        void deveInativarUsuarioAtivoComSucesso() {
            Usuario usuarioAtivo = new Usuario();
            usuarioAtivo.setId(1L);
            usuarioAtivo.setAtivo(true);

            when(usuarioRepository.findById(usuarioAtivo.getId())).thenReturn(Optional.of(usuarioAtivo));
            when(usuarioRepository.save(any(Usuario.class))).thenAnswer(invocation -> invocation.getArgument(0));

            usuarioService.inativarPorId(usuarioAtivo.getId());

            assertFalse(usuarioAtivo.getAtivo());
            verify(usuarioRepository).findById(usuarioAtivo.getId());
            verify(usuarioRepository).save(usuarioAtivo);
        }

        @Test
        @DisplayName("Não deve alterar usuário que já está inativo")
        void naoDeveAlterarUsuarioJaInativo() {
            Usuario usuarioInativo = new Usuario();
            usuarioInativo.setId(2L);
            usuarioInativo.setAtivo(false);

            when(usuarioRepository.findById(usuarioInativo.getId())).thenReturn(Optional.of(usuarioInativo));

            usuarioService.inativarPorId(usuarioInativo.getId());

            assertFalse(usuarioInativo.getAtivo());
            verify(usuarioRepository).findById(usuarioInativo.getId());
            verify(usuarioRepository, never()).save(any(Usuario.class));
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se usuário não existir")
        void deveLancarExceptionSeUsuarioNaoExistir() {
            when(usuarioRepository.findById(ID_INEXISTENTE)).thenReturn(Optional.empty());

            Executable acao = () -> usuarioService.inativarPorId(ID_INEXISTENTE);
            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, acao);

            assertEquals("Usuário com ID [" + ID_INEXISTENTE + "] não encontrado", ex.getMessage());

            verify(usuarioRepository).findById(ID_INEXISTENTE);
            verify(usuarioRepository, never()).save(any());
        }
    }

    @Nested
    @DisplayName("Ativar usuário por ID")
    class AtivarUsuarioPorId {

        @Test
        @DisplayName("Deve ativar usuário inativo com sucesso")
        void deveAtivarUsuarioInativoComSucesso() {
            Usuario usuarioInativo = new Usuario();
            usuarioInativo.setId(1L);
            usuarioInativo.setAtivo(false);

            when(usuarioRepository.findById(usuarioInativo.getId())).thenReturn(Optional.of(usuarioInativo));
            when(usuarioRepository.save(any(Usuario.class))).thenAnswer(invocation -> invocation.getArgument(0));

            usuarioService.ativarPorId(usuarioInativo.getId());

            assertTrue(usuarioInativo.getAtivo());
            verify(usuarioRepository).findById(usuarioInativo.getId());
            verify(usuarioRepository).save(usuarioInativo);
        }

        @Test
        @DisplayName("Não deve alterar usuário que já está ativo")
        void naoDeveAlterarUsuarioJaAtivo() {
            Usuario usuarioAtivo = new Usuario();
            usuarioAtivo.setId(2L);
            usuarioAtivo.setAtivo(true);

            when(usuarioRepository.findById(usuarioAtivo.getId())).thenReturn(Optional.of(usuarioAtivo));

            usuarioService.ativarPorId(usuarioAtivo.getId());

            assertTrue(usuarioAtivo.getAtivo());
            verify(usuarioRepository).findById(usuarioAtivo.getId());
            verify(usuarioRepository, never()).save(any(Usuario.class));
        }

        @Test
        @DisplayName("Deve lançar EntityNotFoundException se usuário não existir")
        void deveLancarExceptionSeUsuarioNaoExistir() {
            when(usuarioRepository.findById(ID_INEXISTENTE)).thenReturn(Optional.empty());

            Executable acao = () -> usuarioService.inativarPorId(ID_INEXISTENTE);
            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, acao);

            assertEquals("Usuário com ID [" + ID_INEXISTENTE + "] não encontrado", ex.getMessage());

            verify(usuarioRepository).findById(ID_INEXISTENTE);
            verify(usuarioRepository, never()).save(any());
        }
    }
}
