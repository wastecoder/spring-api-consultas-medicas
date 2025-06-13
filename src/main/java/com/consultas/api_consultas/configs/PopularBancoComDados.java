package com.consultas.api_consultas.configs;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.repositories.PacienteRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.DayOfWeek;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.temporal.TemporalAdjusters;
import java.util.List;

/**
 * Classe responsável por popular o banco de dados com dados iniciais para desenvolvimento e testes.
 *
 * 1. Verifica se a carga de dados está habilitada via a propriedade 'popular.banco' e se o banco está vazio.
 * 2. Cadastra médicos, pacientes e consultas, controlando o status "ativo" de metade de cada grupo.
 * 3. Garante que consultas sejam agendadas apenas com médicos e pacientes ativos, em horários e datas válidas.
 * 4. Além disso, cadastra consultas com status CANCELADA e REALIZADA para testar diferentes filtros.
 */

@Component
@Slf4j
@RequiredArgsConstructor
public class PopularBancoComDados implements CommandLineRunner {

    @Value("${popular.banco:false}")
    private boolean popularBanco;

    private final MedicoRepository medicoRepository;
    private final PacienteRepository pacienteRepository;
    private final ConsultaRepository consultaRepository;


    @Override
    public void run(String... args) {
        boolean temDados = medicoRepository.count() > 0 || pacienteRepository.count() > 0;
        boolean naoDevePopular = !popularBanco || temDados;

        if (naoDevePopular) {
            log.info("Seed desativada ou banco de dados já contém dados.");
            return;
        }

        List<Medico> medicosCadastrados = cadastrarMedicos();
        List<Medico> medicosAtivos = inativarMedicos(medicosCadastrados);

        List<Paciente> pacientesCadastrados = cadastrarPacientes();
        List<Paciente> pacientesAtivos = inativarPacientes(pacientesCadastrados);

        cadastrarConsultas(medicosAtivos, pacientesAtivos);
        cadastrarConsultasCanceladasERealizadas(medicosAtivos, pacientesAtivos);

        log.info("Seed concluída com sucesso.");
        log.info("Médicos ativos: {}", medicosAtivos.size());
        log.info("Pacientes ativos: {}", pacientesAtivos.size());
    }


    private List<Medico> cadastrarMedicos() {
        return medicoRepository.saveAll(List.of(
                new Medico("Ana Cardoso", "ana.cardoso@medexample.com", "11987654321", SiglaCrm.SP, "123456", Especialidade.PEDIATRIA),
                new Medico("Carla Mendes", "carla.mendes@medexample.com", "31988776655", SiglaCrm.MG, "112233", Especialidade.CARDIOLOGIA),
                new Medico("Elisa Martins", "elisa.martins@medexample.com", "41922334455", SiglaCrm.PR, "334422", Especialidade.ORTOPEDIA),
                new Medico("Gabriela Lima", "gabriela.lima@medexample.com", "71955667788", SiglaCrm.BA, "556677", Especialidade.DERMATOLOGIA),
                new Medico("Isabela Torres", "isabela.torres@medexample.com", "85977889900", SiglaCrm.CE, "778899", Especialidade.PSIQUIATRIA),
                new Medico("Leonardo Meireles", "leonardo.meireles@medexample.com", "21901122334", SiglaCrm.RJ, "011223", Especialidade.NEUROLOGIA),
                new Medico("Nelson Duarte", "nelson.duarte@medexample.com", "51923344556", SiglaCrm.RS, "233445", Especialidade.OFTALMOLOGIA),
                new Medico("Paulo Antunes", "paulo.antunes@medexample.com", "48945566778", SiglaCrm.SC, "455667", Especialidade.RADIOLOGIA),
                new Medico("Samuel Costa", "samuel.costa@medexample.com", "81967788990", SiglaCrm.PE, "677889", Especialidade.HEMATOLOGIA),
                new Medico("Vinícius Ramos", "vinicius.ramos@medexample.com", "61989900112", SiglaCrm.DF, "899001", Especialidade.INFECTOLOGIA),

                new Medico("Bruno Almeida", "bruno.almeida@medexample.com", "21999887766", SiglaCrm.RJ, "654321", Especialidade.PEDIATRIA),
                new Medico("Diego Souza", "diego.souza@medexample.com", "51933445566", SiglaCrm.RS, "221133", Especialidade.CARDIOLOGIA),
                new Medico("Fábio Rocha", "fabio.rocha@medexample.com", "48911223344", SiglaCrm.SC, "443322", Especialidade.ORTOPEDIA),
                new Medico("Henrique Castro", "henrique.castro@medexample.com", "81966778899", SiglaCrm.PE, "667788", Especialidade.DERMATOLOGIA),
                new Medico("João Pereira", "joao.pereira@medexample.com", "61988990011", SiglaCrm.DF, "889900", Especialidade.PSIQUIATRIA),
                new Medico("Karla Nunes", "karla.nunes@medexample.com", "11990011223", SiglaCrm.SP, "900112", Especialidade.NEUROLOGIA),
                new Medico("Mariana Reis", "mariana.reis@medexample.com", "31912233445", SiglaCrm.MG, "122334", Especialidade.OFTALMOLOGIA),
                new Medico("Olivia Braga", "olivia.braga@medexample.com", "41934455667", SiglaCrm.PR, "344556", Especialidade.RADIOLOGIA),
                new Medico("Renata Silva", "renata.silva@medexample.com", "71956677889", SiglaCrm.BA, "566778", Especialidade.HEMATOLOGIA),
                new Medico("Tatiane Lopes", "tatiane.lopes@medexample.com", "85978899001", SiglaCrm.CE, "788990", Especialidade.INFECTOLOGIA)
        ));
    }

    private List<Paciente> cadastrarPacientes() {
        return pacienteRepository.saveAll(List.of(
                new Paciente("Lucas Ferreira", "lucas.ferreira@paciente.com", "11999887766", "12345678901", Sexo.MASCULINO, LocalDate.of(1990, 8, 15)),
                new Paciente("Bruno Oliveira", "bruno.oliveira@paciente.com", "21988776655", "23456789012", Sexo.MASCULINO, LocalDate.of(1988, 4, 10)),
                new Paciente("Carlos Souza", "carlos.souza@paciente.com", "31977665544", "34567890123", Sexo.MASCULINO, LocalDate.of(1975, 11, 3)),
                new Paciente("Diego Lima", "diego.lima@paciente.com", "41966554433", "45678901234", Sexo.MASCULINO, LocalDate.of(1995, 7, 25)),
                new Paciente("Eduardo Martins", "eduardo.martins@paciente.com", "51955443322", "56789012345", Sexo.MASCULINO, LocalDate.of(1982, 2, 14)),
                new Paciente("Mariana Silva", "mariana.silva@paciente.com", "21988776655", "98765432100", Sexo.FEMININO, LocalDate.of(1985, 3, 22)),
                new Paciente("Amanda Costa", "amanda.costa@paciente.com", "11987655443", "87654321001", Sexo.FEMININO, LocalDate.of(1992, 9, 18)),
                new Paciente("Beatriz Souza", "beatriz.souza@paciente.com", "31976544332", "76543210012", Sexo.FEMININO, LocalDate.of(1989, 11, 5)),
                new Paciente("Camila Duarte", "camila.duarte@paciente.com", "41965433221", "65432100123", Sexo.FEMININO, LocalDate.of(1994, 8, 27)),
                new Paciente("Daniela Rocha", "daniela.rocha@paciente.com", "51954322110", "54321001234", Sexo.FEMININO, LocalDate.of(1983, 4, 11)),

                new Paciente("Fábio Ramos", "fabio.ramos@paciente.com", "61944332211", "67890123456", Sexo.MASCULINO, LocalDate.of(1993, 5, 9)),
                new Paciente("Gustavo Rocha", "gustavo.rocha@paciente.com", "71933221100", "78901234567", Sexo.MASCULINO, LocalDate.of(1987, 12, 19)),
                new Paciente("Henrique Almeida", "henrique.almeida@paciente.com", "81922110099", "89012345678", Sexo.MASCULINO, LocalDate.of(1991, 6, 8)),
                new Paciente("Igor Melo", "igor.melo@paciente.com", "85911009988", "90123456789", Sexo.MASCULINO, LocalDate.of(1980, 10, 2)),
                new Paciente("João Pedro", "joao.pedro@paciente.com", "61900998877", "01234567890", Sexo.MASCULINO, LocalDate.of(1997, 1, 30)),
                new Paciente("Elaine Ribeiro", "elaine.ribeiro@paciente.com", "61943211009", "43210012345", Sexo.FEMININO, LocalDate.of(1996, 12, 2)),
                new Paciente("Fernanda Lima", "fernanda.lima@paciente.com", "71932100988", "32100123456", Sexo.FEMININO, LocalDate.of(1990, 6, 17)),
                new Paciente("Gabriela Martins", "gabriela.martins@paciente.com", "81921009877", "21001234567", Sexo.FEMININO, LocalDate.of(1986, 5, 6)),
                new Paciente("Helena Nogueira", "helena.nogueira@paciente.com", "85910988766", "10012345678", Sexo.FEMININO, LocalDate.of(1998, 7, 24)),
                new Paciente("Isabela Torres", "isabela.torres@paciente.com", "61909877655", "00123456789", Sexo.FEMININO, LocalDate.of(1993, 2, 13))
        ));
    }

    // Inativa da metade ao final os médicos cadastrados
    private List<Medico> inativarMedicos(List<Medico> medicosCadastrados) {
        int metade = medicosCadastrados.size() / 2;

        for (int i = metade; i < medicosCadastrados.size(); i++) {
            medicosCadastrados.get(i).setAtivo(false);
        }

        // Atualiza no banco
        medicoRepository.saveAll(medicosCadastrados);

        // Retorna apenas os ativos
        return medicosCadastrados.stream()
                .filter(Medico::getAtivo)
                .toList();
    }

    private List<Paciente> inativarPacientes(List<Paciente> pacientesCadastrados) {
        int metade = pacientesCadastrados.size() / 2;

        for (int i = metade; i < pacientesCadastrados.size(); i++) {
            pacientesCadastrados.get(i).setAtivo(false);
        }

        pacienteRepository.saveAll(pacientesCadastrados);

        return pacientesCadastrados.stream()
                .filter(Paciente::getAtivo)
                .toList();
    }

    private LocalDate pegarProximaSegundaFeira() {
        return LocalDate.now().with(TemporalAdjusters.nextOrSame(DayOfWeek.MONDAY));
    }

    private void cadastrarConsultas(List<Medico> medicos, List<Paciente> pacientes) {
        LocalDate proximaSegundaFeira = pegarProximaSegundaFeira();

        consultaRepository.saveAll(List.of(
                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(8, 0), Duration.ofMinutes(30), new BigDecimal("200.50"), "Consulta infantil de rotina", medicos.get(0), pacientes.get(0)),
                new Consulta(proximaSegundaFeira.plusDays(2), LocalTime.of(10, 0), Duration.ofMinutes(45), new BigDecimal("225.75"), "Consulta infantil de rotina", medicos.get(0), pacientes.get(0)),

                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(9, 0), Duration.ofMinutes(30), new BigDecimal("210.00"), "Avaliação de dor torácica", medicos.get(1), pacientes.get(1)),
                new Consulta(proximaSegundaFeira.plusDays(2), LocalTime.of(11, 0), Duration.ofMinutes(60), new BigDecimal("240.25"), "Avaliação de dor torácica", medicos.get(1), pacientes.get(1)),

                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(10, 0), Duration.ofMinutes(30), new BigDecimal("195.99"), "Dor no joelho ao caminhar", medicos.get(2), pacientes.get(2)),
                new Consulta(proximaSegundaFeira.plusDays(2), LocalTime.of(14, 0), Duration.ofMinutes(45), new BigDecimal("220.00"), "Dor no joelho ao caminhar", medicos.get(2), pacientes.get(2)),

                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(11, 0), Duration.ofMinutes(30), new BigDecimal("180.50"), "Manchas na pele", medicos.get(3), pacientes.get(3)),
                new Consulta(proximaSegundaFeira.plusDays(3), LocalTime.of(9, 30), Duration.ofMinutes(30), new BigDecimal("205.75"), "Manchas na pele", medicos.get(3), pacientes.get(3)),

                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(14, 0), Duration.ofMinutes(30), new BigDecimal("210.00"), "Avaliação de ansiedade", medicos.get(4), pacientes.get(4)),
                new Consulta(proximaSegundaFeira.plusDays(3), LocalTime.of(10, 30), Duration.ofMinutes(45), new BigDecimal("230.99"), "Avaliação de ansiedade", medicos.get(4), pacientes.get(4)),

                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(15, 0), Duration.ofMinutes(30), new BigDecimal("190.00"), "Dores de cabeça frequentes", medicos.get(5), pacientes.get(5)),
                new Consulta(proximaSegundaFeira.plusDays(3), LocalTime.of(11, 30), Duration.ofMinutes(60), new BigDecimal("215.25"), "Dores de cabeça frequentes", medicos.get(5), pacientes.get(5)),

                new Consulta(proximaSegundaFeira.plusDays(1), LocalTime.of(16, 0), Duration.ofMinutes(30), new BigDecimal("185.00"), "Dificuldade para enxergar", medicos.get(6), pacientes.get(6)),
                new Consulta(proximaSegundaFeira.plusDays(4), LocalTime.of(9, 0), Duration.ofMinutes(45), new BigDecimal("205.80"), "Dificuldade para enxergar", medicos.get(6), pacientes.get(6)),

                new Consulta(proximaSegundaFeira.plusDays(2), LocalTime.of(8, 30), Duration.ofMinutes(30), new BigDecimal("175.25"), "Requisição de exame de imagem", medicos.get(7), pacientes.get(7)),
                new Consulta(proximaSegundaFeira.plusDays(4), LocalTime.of(10, 0), Duration.ofMinutes(45), new BigDecimal("198.50"), "Requisição de exame de imagem", medicos.get(7), pacientes.get(7)),

                new Consulta(proximaSegundaFeira.plusDays(2), LocalTime.of(9, 30), Duration.ofMinutes(30), new BigDecimal("220.30"), "Alterações no hemograma", medicos.get(8), pacientes.get(8)),
                new Consulta(proximaSegundaFeira.plusDays(4), LocalTime.of(11, 0), Duration.ofMinutes(60), new BigDecimal("250.99"), "Alterações no hemograma", medicos.get(8), pacientes.get(8)),

                new Consulta(proximaSegundaFeira.plusDays(2), LocalTime.of(10, 30), Duration.ofMinutes(30), new BigDecimal("210.15"), "Sintomas de infecção recorrente", medicos.get(9), pacientes.get(9)),
                new Consulta(proximaSegundaFeira.plusDays(4), LocalTime.of(14, 0), Duration.ofMinutes(45), new BigDecimal("240.60"), "Sintomas de infecção recorrente", medicos.get(9), pacientes.get(9))
        ));
    }

    private void cadastrarConsultasCanceladasERealizadas(List<Medico> medicos, List<Paciente> pacientes) {
        LocalDate proximaSegundaFeira = pegarProximaSegundaFeira();

        Consulta realizada1 = new Consulta(
                proximaSegundaFeira.minusDays(10),
                LocalTime.of(10, 0),
                Duration.ofMinutes(30),
                new BigDecimal("220.50"),
                "Avaliação psiquiátrica de rotina",
                medicos.get(0),
                pacientes.get(0)
        );

        Consulta realizada2 = new Consulta(
                proximaSegundaFeira.minusDays(8),
                LocalTime.of(14, 0),
                Duration.ofMinutes(45),
                new BigDecimal("250.75"),
                "Acompanhamento neurológico",
                medicos.get(1),
                pacientes.get(1)
        );

        Consulta cancelada1 = new Consulta(
                proximaSegundaFeira.minusDays(6),
                LocalTime.of(9, 30),
                Duration.ofMinutes(30),
                new BigDecimal("180.00"),
                "Exame radiológico",
                medicos.get(2),
                pacientes.get(2)
        );

        Consulta cancelada2 = new Consulta(
                proximaSegundaFeira.minusDays(4),
                LocalTime.of(15, 0),
                Duration.ofMinutes(60),
                new BigDecimal("310.90"),
                "Consulta hematológica",
                medicos.get(3),
                pacientes.get(3)
        );

        // Salva inicialmente como AGENDADA (o padrão via @PrePersist)
        List<Consulta> consultas = consultaRepository.saveAll(List.of(realizada1, realizada2, cancelada1, cancelada2));

        // Atualiza status para realizadas e canceladas
        consultas.get(0).setStatus(StatusConsulta.REALIZADA);
        consultas.get(1).setStatus(StatusConsulta.REALIZADA);
        consultas.get(2).setStatus(StatusConsulta.CANCELADA);
        consultas.get(3).setStatus(StatusConsulta.CANCELADA);

        // Salva novamente todas as consultas com status atualizado
        consultaRepository.saveAll(consultas);
    }

}
