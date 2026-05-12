package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.mappers.ConsultaMapper;
import com.consultas.api_consultas.mappers.ConsultaMapperImpl;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.services.rules.ConsultaRules;
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
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.access.AccessDeniedException;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de Consulta")
class ConsultaServiceImplTest {

    @InjectMocks
    private ConsultaServiceImpl consultaService;

    @Mock
    private ConsultaRepository consultaRepository;

    @Mock
    private MedicoService medicoService;

    @Mock
    private PacienteService pacienteService;

    @Mock
    private ConsultaRules consultaRules;

    @Mock
    private SecurityUtil securityUtil;

    @Spy
    private ConsultaMapper consultaMapper = new ConsultaMapperImpl();

    private LocalDate clockDataBase;
    private Medico medicoAtivo;
    private Medico medicoInativo;
    private Medico medicoInexistente;
    private Paciente pacienteAtivo;
    private Paciente pacienteInativo;
    private Paciente pacienteInexistente;
    private Consulta consultaValida;
    private Consulta consultaConflito;
    private Consulta consultaAntiga;
    private Consulta consultaDeOutroPaciente;

    @BeforeEach
    void setUp() {
        inicializarClockBase(); // 01/07/2025 - 10:00

        criarMedicoAtivo();
        criarMedicoInativo();
        criarMedicoInexistente();

        criarPacienteAtivo();
        criarPacienteInativo();
        criarPacienteInexistente();

        criarConsultaAgendadaValida(); // 02/07/2025 - 10:00 - 30min
        criarConsultaAgendadaInvalida(); // Mesmo horário acima

        criarConsultaRealizadaAntiga(); // 24/06/2025 - 09:00 - 30min

        criarConsultaDeOutroPaciente(); // 02/07/2025 - 11:00 - 30min
    }

    private void inicializarClockBase() {
        Clock clockFixo = Clock.fixed(
                LocalDateTime.of(2025, 7, 1, 10, 0).toInstant(ZoneOffset.UTC),
                ZoneId.of("UTC")
        );
        this.clockDataBase = LocalDate.now(clockFixo);
    }

    private void criarMedicoAtivo() {
        medicoAtivo = new Medico("João", "joao@med.com", "11111111111", SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA);
        this.medicoAtivo.setId(1L);
        this.medicoAtivo.setAtivo(true);
    }

    private void criarMedicoInativo() {
        medicoInativo = new Medico("Maria", "maria@med.com", "22222222222", SiglaCrm.RJ, "654321", Especialidade.ORTOPEDIA);
        medicoInativo.setId(2L);
        medicoInativo.setAtivo(false);
    }

    private void criarMedicoInexistente() {
        medicoInexistente = new Medico();
        medicoInexistente.setId(999L);
    }

    private void criarPacienteAtivo() {
        pacienteAtivo = new Paciente("Ana", "ana@paciente.com", "33333333333", "98765432100", Sexo.FEMININO, LocalDate.of(1980, 5, 10));
        pacienteAtivo.setId(1L);
        pacienteAtivo.setId(1L);
        pacienteAtivo.setAtivo(true);
    }

    private void criarPacienteInativo() {
        pacienteInativo = new Paciente("Carlos", "carlos@paciente.com", "44444444444", "12345678900", Sexo.MASCULINO, LocalDate.of(1970, 1, 1));
        pacienteInativo.setId(2L);
        pacienteInativo.setAtivo(false);
    }

    private void criarPacienteInexistente() {
        pacienteInexistente = new Paciente();
        pacienteInexistente.setId(999L);
    }

    private void criarConsultaAgendadaValida() {
        consultaValida = new Consulta(
                clockDataBase.plusDays(1),
                LocalTime.of(10, 0),
                Duration.ofMinutes(30),
                new BigDecimal("150.00"),
                "Consulta de rotina",
                medicoAtivo,
                pacienteAtivo
        );
        consultaValida.setId(1L);
        consultaValida.setStatus(StatusConsulta.AGENDADA);
    }

    private void criarConsultaAgendadaInvalida() {
        consultaConflito = new Consulta(
                clockDataBase.plusDays(1),
                LocalTime.of(10, 0),
                Duration.ofMinutes(30),
                new BigDecimal("150.00"),
                "Consulta conflitante",
                medicoAtivo,
                pacienteAtivo
        );
        consultaConflito.setId(2L);
        consultaConflito.setStatus(StatusConsulta.AGENDADA);
    }

    private void criarConsultaRealizadaAntiga() {
        consultaAntiga = new Consulta(
                clockDataBase.minusWeeks(1),
                LocalTime.of(9, 0),
                Duration.ofMinutes(30),
                new BigDecimal("120.00"),
                "Consulta antiga",
                medicoAtivo,
                pacienteAtivo
        );
        consultaAntiga.setId(3L);
        consultaAntiga.setStatus(StatusConsulta.REALIZADA);
    }

    private void criarConsultaDeOutroPaciente() {
        consultaDeOutroPaciente = new Consulta(
                clockDataBase.plusDays(1),
                LocalTime.of(11, 0),
                Duration.ofMinutes(30),
                new BigDecimal("160.00"),
                "Consulta de outro paciente",
                medicoAtivo,
                pacienteInativo
        );
        consultaDeOutroPaciente.setId(999L);
    }

    private Consulta clonarConsulta(Consulta original) {
        Consulta copia = new Consulta(
                original.getDataAtendimento(),
                original.getHorarioAtendimento(),
                original.getDuracaoEmMinutos(),
                original.getPreco(),
                original.getMotivo(),
                original.getMedico(),
                original.getPaciente()
        );
        copia.setStatus(original.getStatus());
        copia.setId(original.getId());
        return copia;
    }


    @Nested
    @DisplayName("Salvar consulta")
    class SalvarConsulta {

        @Test
        @DisplayName("Deve salvar consulta com sucesso se for paciente logado e for ele mesmo")
        void deveSalvarConsultaSePacienteForODono() {
            when(securityUtil.isPatient()).thenReturn(true);
            when(securityUtil.isSamePatient(pacienteAtivo)).thenReturn(true);
            when(medicoService.buscarPorId(medicoAtivo.getId())).thenReturn(medicoAtivo);
            when(pacienteService.buscarPorId(pacienteAtivo.getId())).thenReturn(pacienteAtivo);
            doNothing().when(consultaRules).validarCadastro(any(Consulta.class));
            when(consultaRepository.save(any(Consulta.class))).thenReturn(consultaValida);

            Consulta salva = consultaService.salvar(consultaValida);

            assertNotNull(salva);
            assertEquals(pacienteAtivo, salva.getPaciente());
            assertEquals(medicoAtivo, salva.getMedico());
        }

        @Test
        @DisplayName("Deve lançar exceção se o paciente tentar agendar para outro paciente")
        void deveLancarExcecaoSePacienteAgendarParaOutro() {
            when(securityUtil.isPatient()).thenReturn(true);
            when(securityUtil.isSamePatient(pacienteInativo)).thenReturn(false);
            consultaDeOutroPaciente.setPaciente(pacienteInativo);

            AccessDeniedException e = assertThrows(AccessDeniedException.class, () ->
                    consultaService.salvar(consultaDeOutroPaciente)
            );

            assertEquals("Você só pode agendar consultas para você mesmo.", e.getMessage());
        }

        @Test
        @DisplayName("Deve salvar consulta se não for paciente (ex: admin ou recepcionista)")
        void deveSalvarConsultaSeNaoForPaciente() {
            when(securityUtil.isPatient()).thenReturn(false);
            when(medicoService.buscarPorId(medicoAtivo.getId())).thenReturn(medicoAtivo);
            when(pacienteService.buscarPorId(pacienteAtivo.getId())).thenReturn(pacienteAtivo);
            doNothing().when(consultaRules).validarCadastro(any(Consulta.class));
            when(consultaRepository.save(any(Consulta.class))).thenReturn(consultaValida);

            Consulta salva = consultaService.salvar(consultaValida);

            assertNotNull(salva);
            assertEquals(pacienteAtivo, salva.getPaciente());
        }

        @Test
        @DisplayName("Deve lançar exceção se regra de cadastro não for satisfeita")
        void deveLancarExcecaoSeRegraDeCadastroFalhar() {
            when(securityUtil.isPatient()).thenReturn(true);
            when(securityUtil.isSamePatient(pacienteAtivo)).thenReturn(true);
            when(medicoService.buscarPorId(medicoAtivo.getId())).thenReturn(medicoAtivo);
            when(pacienteService.buscarPorId(pacienteAtivo.getId())).thenReturn(pacienteAtivo);
            doThrow(new IllegalArgumentException("Conflito de horário")).when(consultaRules).validarCadastro(any(Consulta.class));

            IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () ->
                    consultaService.salvar(consultaValida)
            );

            assertEquals("Conflito de horário", e.getMessage());
        }
    }

    @Nested
    @DisplayName("Buscar consulta por ID")
    class BuscarConsultaPorId {

        @Test
        @DisplayName("Deve retornar consulta se encontrada e acesso permitido")
        void deveRetornarConsultaSeAcessoPermitido() {
            when(consultaRepository.findById(consultaValida.getId())).thenReturn(Optional.of(consultaValida));
            when(securityUtil.canAccessAppointment(consultaValida)).thenReturn(true);

            Consulta consulta = consultaService.buscarPorId(consultaValida.getId());

            assertNotNull(consulta);
            assertEquals(consultaValida, consulta);
        }

        @Test
        @DisplayName("Deve lançar exceção se a consulta não for encontrada")
        void deveLancarExcecaoSeConsultaNaoEncontrada() {
            Long idInexistente = 999L;
            when(consultaRepository.findById(idInexistente)).thenReturn(Optional.empty());

            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, () ->
                    consultaService.buscarPorId(idInexistente)
            );

            assertEquals("Consulta com ID [999] não encontrada", e.getMessage());
        }

        @Test
        @DisplayName("Deve lançar exceção se acesso à consulta for negado")
        void deveLancarExcecaoSeAcessoNegado() {
            when(consultaRepository.findById(consultaDeOutroPaciente.getId())).thenReturn(Optional.of(consultaDeOutroPaciente));
            when(securityUtil.canAccessAppointment(consultaDeOutroPaciente)).thenReturn(false);

            Executable acao = () -> consultaService.buscarPorId(consultaDeOutroPaciente.getId());
            AccessDeniedException exp = assertThrows(AccessDeniedException.class, acao);

            assertEquals("Você não tem permissão para acessar esta consulta.", exp.getMessage());
        }
    }

    @Nested
    @DisplayName("Buscar todas as consultas")
    class BuscarTodasAsConsultas {

        @Test
        @DisplayName("Deve retornar todas as consultas se for admin")
        void deveRetornarConsultasSeForAdmin() {
            when(securityUtil.isAdmin()).thenReturn(true);
            when(consultaRepository.findAll()).thenReturn(List.of(consultaValida, consultaConflito));

            List<Consulta> consultas = consultaService.buscarTodos();

            assertEquals(2, consultas.size());
            assertTrue(consultas.contains(consultaValida));
            verify(consultaRepository).findAll();
        }

        @Test
        @DisplayName("Deve retornar todas as consultas se for recepcionista")
        void deveRetornarConsultasSeForRecepcionista() {
            when(securityUtil.isAdmin()).thenReturn(false);
            when(securityUtil.isReceptionist()).thenReturn(true);
            when(consultaRepository.findAll()).thenReturn(List.of(consultaValida));

            List<Consulta> consultas = consultaService.buscarTodos();

            assertEquals(1, consultas.size());
            assertEquals(consultaValida, consultas.get(0));
            verify(consultaRepository).findAll();
        }

        @Test
        @DisplayName("Deve lançar exceção se usuário não for admin nem recepcionista")
        void deveLancarExcecaoSeNaoAutorizado() {
            when(securityUtil.isAdmin()).thenReturn(false);
            when(securityUtil.isReceptionist()).thenReturn(false);

            AccessDeniedException e = assertThrows(AccessDeniedException.class, () ->
                    consultaService.buscarTodos()
            );

            assertEquals("Apenas administradores ou recepcionistas podem acessar todas as consultas.", e.getMessage());
            verify(consultaRepository, never()).findAll();
        }
    }

    @Nested
    @DisplayName("Buscar consultas com filtros")
    class BuscarConsultasComFiltros {

        private static final int PAGINA = 0;
        private static final int TAMANHO = 5;

        private PageImpl<Consulta> pageOf(Consulta... consultas) {
            return new PageImpl<>(List.of(consultas));
        }

        @Test
        @DisplayName("Deve combinar todos os filtros informados via Specification")
        void deveCombinarTodosOsFiltros() {
            LocalDate data = clockDataBase.plusDays(1);
            when(securityUtil.isDoctor()).thenReturn(false);
            when(securityUtil.isPatient()).thenReturn(false);
            when(consultaRepository.findAll(any(Specification.class), any(Pageable.class)))
                    .thenReturn(pageOf(consultaValida));

            PageResponse<ConsultaResposta> result = consultaService.buscarConsultas(
                    PAGINA, TAMANHO, medicoAtivo.getId(), pacienteAtivo.getId(), data, StatusConsulta.AGENDADA);

            assertEquals(1, result.totalElements());
            verify(consultaRepository).findAll(any(Specification.class), any(Pageable.class));
        }

        @Test
        @DisplayName("Sem filtros: chama findAll com Specification vazia (sem default AGENDADA)")
        void semFiltrosListaTodos() {
            when(securityUtil.isDoctor()).thenReturn(false);
            when(securityUtil.isPatient()).thenReturn(false);
            when(consultaRepository.findAll(any(Specification.class), any(Pageable.class)))
                    .thenReturn(pageOf(consultaValida));

            PageResponse<ConsultaResposta> result = consultaService.buscarConsultas(PAGINA, TAMANHO, null, null, null, null);

            assertEquals(1, result.totalElements());
            verify(consultaRepository).findAll(any(Specification.class), any(Pageable.class));
        }

        @Test
        @DisplayName("Usuário médico: sobrescreve medicoId com o médico logado antes de aplicar a Specification")
        void medicoLogadoSobrescreveParametro() {
            when(securityUtil.isDoctor()).thenReturn(true);
            when(securityUtil.getLoggedDoctor()).thenReturn(medicoAtivo);
            when(consultaRepository.findAll(any(Specification.class), any(Pageable.class)))
                    .thenReturn(pageOf(consultaValida));

            PageResponse<ConsultaResposta> result = consultaService.buscarConsultas(PAGINA, TAMANHO, 99L, null, null, null);

            assertEquals(1, result.totalElements());
            verify(securityUtil).getLoggedDoctor();
            verify(consultaRepository).findAll(any(Specification.class), any(Pageable.class));
        }

        @Test
        @DisplayName("Usuário paciente: sobrescreve pacienteId com o paciente logado antes de aplicar a Specification")
        void pacienteLogadoSobrescreveParametro() {
            when(securityUtil.isDoctor()).thenReturn(false);
            when(securityUtil.isPatient()).thenReturn(true);
            when(securityUtil.getLoggedPatient()).thenReturn(pacienteAtivo);
            when(consultaRepository.findAll(any(Specification.class), any(Pageable.class)))
                    .thenReturn(pageOf(consultaValida));

            PageResponse<ConsultaResposta> result = consultaService.buscarConsultas(PAGINA, TAMANHO, null, 99L, null, null);

            assertEquals(1, result.totalElements());
            verify(securityUtil).getLoggedPatient();
            verify(consultaRepository).findAll(any(Specification.class), any(Pageable.class));
        }
    }

    @Nested
    @DisplayName("Atualizar consulta")
    class AtualizarConsulta {

        @Test
        @DisplayName("Sucesso ao atualizar consulta existente")
        void sucessoAtualizarConsulta() {
            Long consultaId = consultaAntiga.getId();
            ConsultaAtualizacaoDto dto = dtoFrom(consultaAntiga);
            setField(dto, "motivo", "Consulta atualizada");

            when(securityUtil.canAccessAppointment(any(Consulta.class))).thenReturn(true);
            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaAntiga));
            when(medicoService.buscarPorId(medicoAtivo.getId())).thenReturn(medicoAtivo);
            when(pacienteService.buscarPorId(pacienteAtivo.getId())).thenReturn(pacienteAtivo);
            when(consultaRepository.save(any(Consulta.class))).thenAnswer(i -> i.getArgument(0));

            Consulta resultado = consultaService.atualizar(consultaId, dto);

            assertNotNull(resultado);
            assertEquals("Consulta atualizada", resultado.getMotivo());
            verify(consultaRepository).save(argThat(consultaSalva ->
                    consultaSalva.getId().equals(consultaId) &&
                            consultaSalva.getMotivo().equals("Consulta atualizada") &&
                            consultaSalva.getDataAtendimento().equals(consultaAntiga.getDataAtendimento())
            ));
        }

        @Test
        @DisplayName("Falha ao atualizar consulta inexistente")
        void falhaAtualizarConsultaNaoEncontrada() {
            Long consultaId = 999L;
            ConsultaAtualizacaoDto dto = dtoFrom(consultaValida);

            when(consultaRepository.findById(consultaId)).thenReturn(Optional.empty());

            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, () ->
                    consultaService.atualizar(consultaId, dto)
            );

            assertEquals("Consulta com ID [999] não encontrada", e.getMessage());
        }

        @Test
        @DisplayName("Falha ao atualizar com médico inativo")
        void falhaAtualizarMedicoInativo() {
            Long consultaId = consultaValida.getId();
            ConsultaAtualizacaoDto dto = dtoFrom(consultaValida);
            setField(dto, "medicoId", medicoInativo.getId());

            when(securityUtil.canAccessAppointment(any(Consulta.class))).thenReturn(true);
            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaValida));
            when(medicoService.buscarPorId(medicoInativo.getId())).thenReturn(medicoInativo);

            doThrow(new BusinessRuleException("Não é possível agendar consulta com médico inativo."))
                    .when(consultaRules).validarAtualizacao(any(Consulta.class), any(Consulta.class));

            BusinessRuleException e = assertThrows(BusinessRuleException.class, () ->
                    consultaService.atualizar(consultaId, dto)
            );

            assertEquals("Não é possível agendar consulta com médico inativo.", e.getMessage());
        }

        @Test
        @DisplayName("Falha ao atualizar com paciente inativo")
        void falhaAtualizarPacienteInativo() {
            Long consultaId = consultaValida.getId();
            ConsultaAtualizacaoDto dto = dtoFrom(consultaValida);
            setField(dto, "pacienteId", pacienteInativo.getId());

            when(securityUtil.canAccessAppointment(any(Consulta.class))).thenReturn(true);
            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaValida));
            when(medicoService.buscarPorId(medicoAtivo.getId())).thenReturn(medicoAtivo);
            when(pacienteService.buscarPorId(pacienteInativo.getId())).thenReturn(pacienteInativo);

            doThrow(new BusinessRuleException("Não é possível agendar consulta com paciente inativo."))
                    .when(consultaRules).validarAtualizacao(any(Consulta.class), any(Consulta.class));

            BusinessRuleException e = assertThrows(BusinessRuleException.class, () ->
                    consultaService.atualizar(consultaId, dto)
            );

            assertEquals("Não é possível agendar consulta com paciente inativo.", e.getMessage());
        }

        @Test
        @DisplayName("Falha ao atualizar se acesso à consulta for negado")
        void falhaAtualizarAcessoNegado() {
            Long consultaId = consultaValida.getId();
            ConsultaAtualizacaoDto dto = dtoFrom(consultaValida);

            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaValida));
            when(securityUtil.canAccessAppointment(consultaValida)).thenReturn(false);

            AccessDeniedException ex = assertThrows(AccessDeniedException.class, () ->
                    consultaService.atualizar(consultaId, dto)
            );

            assertEquals("Você não tem permissão para acessar esta consulta.", ex.getMessage());
        }

        @Test
        @DisplayName("Falha ao atualizar se médico não for encontrado")
        void falhaAtualizarMedicoNaoEncontrado() {
            Long consultaId = consultaAntiga.getId();
            ConsultaAtualizacaoDto dto = dtoFrom(consultaAntiga);
            setField(dto, "motivo", "Atualização com médico inexistente");
            setField(dto, "medicoId", medicoInexistente.getId());

            when(securityUtil.canAccessAppointment(any(Consulta.class))).thenReturn(true);
            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaAntiga));
            when(medicoService.buscarPorId(999L)).thenThrow(new EntityNotFoundException("Médico com ID [999] não encontrado"));

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    consultaService.atualizar(consultaId, dto)
            );

            assertEquals("Médico com ID [999] não encontrado", ex.getMessage());
            verify(consultaRepository, never()).save(any(Consulta.class));
            verify(medicoService).buscarPorId(999L);
            verify(securityUtil).canAccessAppointment(any(Consulta.class));
        }

        @Test
        @DisplayName("Falha ao atualizar se paciente não for encontrado")
        void falhaAtualizarPacienteNaoEncontrado() {
            Long consultaId = consultaAntiga.getId();
            ConsultaAtualizacaoDto dto = dtoFrom(consultaAntiga);
            setField(dto, "motivo", "Atualização com paciente inexistente");
            setField(dto, "pacienteId", pacienteInexistente.getId());

            when(securityUtil.canAccessAppointment(any(Consulta.class))).thenReturn(true);
            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaAntiga));
            when(medicoService.buscarPorId(medicoAtivo.getId())).thenReturn(medicoAtivo);
            when(pacienteService.buscarPorId(999L)).thenThrow(new EntityNotFoundException("Paciente com ID [999] não encontrado"));

            EntityNotFoundException ex = assertThrows(EntityNotFoundException.class, () ->
                    consultaService.atualizar(consultaId, dto)
            );

            assertEquals("Paciente com ID [999] não encontrado", ex.getMessage());
            verify(consultaRepository, never()).save(any(Consulta.class));
            verify(securityUtil).canAccessAppointment(any(Consulta.class));
            verify(medicoService).buscarPorId(medicoAtivo.getId());
            verify(pacienteService).buscarPorId(999L);
        }

        private ConsultaAtualizacaoDto dtoFrom(Consulta c) {
            ConsultaAtualizacaoDto dto = new ConsultaAtualizacaoDto();
            setField(dto, "dataAtendimento", c.getDataAtendimento());
            setField(dto, "horarioAtendimento",
                    c.getHorarioAtendimento() != null ? c.getHorarioAtendimento().toString() : null);
            setField(dto, "duracaoEmMinutos",
                    c.getDuracaoEmMinutos() != null ? (int) c.getDuracaoEmMinutos().toMinutes() : null);
            setField(dto, "preco", c.getPreco());
            setField(dto, "motivo", c.getMotivo());
            setField(dto, "status", c.getStatus());
            setField(dto, "medicoId", c.getMedico() != null ? c.getMedico().getId() : null);
            setField(dto, "pacienteId", c.getPaciente() != null ? c.getPaciente().getId() : null);
            return dto;
        }

        private void setField(Object target, String name, Object value) {
            try {
                java.lang.reflect.Field f = target.getClass().getDeclaredField(name);
                f.setAccessible(true);
                f.set(target, value);
            } catch (ReflectiveOperationException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Nested
    @DisplayName("Remover consulta")
    class RemoverConsulta {

        @Test
        @DisplayName("Sucesso ao remover consulta existente")
        void sucessoRemoverConsulta() {
            Long consultaId = consultaValida.getId();

            when(securityUtil.canAccessAppointment(consultaValida)).thenReturn(true);
            when(securityUtil.isAdmin()).thenReturn(true); // ou .isReceptionist()

            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaValida));
            doNothing().when(consultaRepository).deleteById(consultaId);

            assertDoesNotThrow(() -> consultaService.removerPorId(consultaId));

            verify(consultaRepository).deleteById(consultaId);
        }

        @Test
        @DisplayName("Falha ao remover consulta inexistente")
        void falhaRemoverConsultaNaoEncontrada() {
            Long consultaId = 999L;

            when(consultaRepository.findById(consultaId)).thenReturn(Optional.empty());

            EntityNotFoundException e = assertThrows(EntityNotFoundException.class, () ->
                    consultaService.removerPorId(consultaId)
            );

            assertEquals("Consulta com ID [999] não encontrada", e.getMessage());
        }

        @Test
        @DisplayName("Falha ao remover consulta sem permissão")
        void falhaAoRemoverSemPermissao() {
            Long consultaId = consultaValida.getId();

            when(securityUtil.canAccessAppointment(consultaValida)).thenReturn(true);
            when(securityUtil.isAdmin()).thenReturn(false);
            when(securityUtil.isReceptionist()).thenReturn(false);

            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaValida));

            AccessDeniedException exception = assertThrows(AccessDeniedException.class, () ->
                    consultaService.removerPorId(consultaId)
            );

            assertEquals("Você não tem permissão para excluir esta consulta.", exception.getMessage());
            verify(consultaRepository, never()).deleteById(any());
        }

        @Test
        @DisplayName("Falha ao remover se usuário não pode acessar a consulta")
        void falhaRemoverSemAcessoALeitura() {
            Long consultaId = consultaValida.getId();

            when(consultaRepository.findById(consultaId)).thenReturn(Optional.of(consultaValida));
            when(securityUtil.canAccessAppointment(consultaValida)).thenReturn(false);

            AccessDeniedException ex = assertThrows(AccessDeniedException.class, () ->
                    consultaService.removerPorId(consultaId)
            );

            assertEquals("Você não tem permissão para acessar esta consulta.", ex.getMessage());
            verify(consultaRepository, never()).deleteById(any());
        }
    }
}
