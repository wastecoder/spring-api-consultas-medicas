package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.mappers.ConsultaMapper;
import com.consultas.api_consultas.repositories.ConsultaRepository;
import com.consultas.api_consultas.repositories.specifications.ConsultaSpecifications;
import com.consultas.api_consultas.services.ConsultaService;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.PacienteService;
import com.consultas.api_consultas.services.rules.ConsultaRules;
import com.consultas.api_consultas.utils.SecurityUtil;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Service
@AllArgsConstructor
@Slf4j
public class ConsultaServiceImpl implements ConsultaService {

    private final ConsultaRepository repository;
    private final MedicoService medicoService;
    private final PacienteService pacienteService;
    private final ConsultaRules consultaRules;
    private final SecurityUtil securityUtil;
    private final ConsultaMapper consultaMapper;


    @Override
    public Consulta salvar(Consulta consultaNova) {
        log.info("Salvando nova consulta para médico ID {} e paciente ID {}",
                consultaNova.getMedico().getId(), consultaNova.getPaciente().getId());

        if (securityUtil.isPatient() && !securityUtil.isSamePatient(consultaNova.getPaciente())) {
            throw new AccessDeniedException("Você só pode agendar consultas para você mesmo.");
        }

        validarRelacionamentos(consultaNova);
        consultaRules.validarCadastro(consultaNova);

        return repository.save(consultaNova);
    }

    @Override
    public List<Consulta> buscarTodos() {
        log.info("Buscando todas as consultas");

        if (!securityUtil.isAdmin() && !securityUtil.isReceptionist()) {
            throw new AccessDeniedException("Apenas administradores ou recepcionistas podem acessar todas as consultas.");
        }

        return repository.findAll();
    }

    @Override
    public Consulta buscarPorId(Long id) {
        log.info("Buscando consulta por ID: {}", id);

        Consulta consulta = repository.findById(id)
                .orElseThrow(() -> {
                    log.warn("Consulta com ID [{}] não encontrada", id);
                    return new EntityNotFoundException("Consulta com ID [" + id + "] não encontrada");
                });

        if (!securityUtil.canAccessAppointment(consulta)) {
            throw new AccessDeniedException("Você não tem permissão para acessar esta consulta.");
        }

        return consulta;
    }

    @Override
    public PageResponse<ConsultaResposta> buscarConsultas(int pagina, int tamanho, Long medicoId, Long pacienteId, LocalDate dataAtendimento, StatusConsulta status) {
        log.info("Buscando consultas - Página: {}, Tamanho: {}, Médico ID: {}, Paciente ID: {}, Data: {}, Status: {}",
                pagina, tamanho, medicoId, pacienteId, dataAtendimento, status);

        if (securityUtil.isDoctor()) {
            medicoId = securityUtil.getLoggedDoctor().getId();
        } else if (securityUtil.isPatient()) {
            pacienteId = securityUtil.getLoggedPatient().getId();
        }

        Pageable pageable = PageRequest.of(pagina, tamanho, Sort.by(Sort.Direction.ASC, "dataAtendimento"));
        Specification<Consulta> spec = Specification
                .where(ConsultaSpecifications.comStatus(status))
                .and(ConsultaSpecifications.comMedicoId(medicoId))
                .and(ConsultaSpecifications.comPacienteId(pacienteId))
                .and(ConsultaSpecifications.comDataAtendimento(dataAtendimento));

        return PageResponse.from(repository.findAll(spec, pageable).map(consultaMapper::paraResposta));
    }

    @Override
    public Consulta atualizar(Long id, ConsultaAtualizacaoDto requisicao) {
        log.info("Atualizando consulta ID: {}", id);

        Consulta consultaExistente = this.buscarPorId(id);
        LocalDate dataAtendimentoAntiga = consultaExistente.getDataAtendimento();

        consultaMapper.aplicarAtualizacao(requisicao, consultaExistente);
        validarRelacionamentos(consultaExistente);
        consultaRules.verificarReagendamentoNoPassado(dataAtendimentoAntiga, consultaExistente);
        consultaRules.validarAtualizacao(consultaExistente, consultaExistente);

        Consulta consultaSalva = repository.save(consultaExistente);
        log.info("Consulta ID {} atualizada com sucesso", id);
        return consultaSalva;
    }

    @Override
    public void removerPorId(Long id) {
        log.info("Removendo consulta ID: {}", id);

        this.buscarPorId(id); // lança exceção se não encontrar e valida autorização
        if (!podeEditarConsulta()) {
            throw new AccessDeniedException("Você não tem permissão para excluir esta consulta.");
        }

        repository.deleteById(id);
        log.info("Consulta ID {} removida com sucesso", id);
    }

    private void validarRelacionamentos(Consulta consulta) {
        Long medicoId = consulta.getMedico().getId();
        Long pacienteId = consulta.getPaciente().getId();

        log.debug("Validando relacionamentos - Médico ID: {}, Paciente ID: {}", medicoId, pacienteId);

        consulta.setMedico(medicoService.buscarPorId(medicoId));
        consulta.setPaciente(pacienteService.buscarPorId(pacienteId));
    }

    private boolean podeEditarConsulta() {
        return securityUtil.isAdmin() ||
                securityUtil.isReceptionist();
    }

}
