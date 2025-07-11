package com.consultas.api_consultas.services.rules;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.exceptions.BusinessRuleException;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.services.MedicoService;
import com.consultas.api_consultas.services.PacienteService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UsuarioRules {

    private final UsuarioRepository usuarioRepository;
    private final MedicoService medicoService;
    private final PacienteService pacienteService;


    public void validarUsernameDuplicado(String username, Long idAtual) {
        boolean existe = (idAtual == null)
                ? usuarioRepository.existsByUsername(username)
                : usuarioRepository.existsByUsernameAndIdNot(username, idAtual);

        if (existe) {
            throw new BusinessRuleException("Nome de usuário já está em uso: " + username);
        }
    }

    public void validarRegrasDeAssociacao(UsuarioCadastroDto req) {
        if (req.getFuncao() == Funcao.RECEPCIONISTA && req.getIdAssociado() != null) {
            throw new BusinessRuleException("Recepcionista não pode estar associado a médico ou paciente.");
        }

        if ((req.getFuncao() == Funcao.MEDICO || req.getFuncao() == Funcao.PACIENTE) && req.getIdAssociado() == null) {
            throw new BusinessRuleException("Função " + req.getFuncao() + " exige ID de associação.");
        }
    }

    public void associarUsuarioAoMedicoOuPaciente(UsuarioCadastroDto req, Usuario usuario) {
        if (req.getFuncao() == Funcao.MEDICO) {
            Medico medico = medicoService.buscarPorId(req.getIdAssociado());
            if (medico.getUsuario() != null) {
                throw new BusinessRuleException("Este médico já está associado a um usuário.");
            }
            medico.setUsuario(usuario);
            medicoService.salvar(medico);
        }

        if (req.getFuncao() == Funcao.PACIENTE) {
            Paciente paciente = pacienteService.buscarPorId(req.getIdAssociado());
            if (paciente.getUsuario() != null) {
                throw new BusinessRuleException("Este paciente já está associado a um usuário.");
            }
            paciente.setUsuario(usuario);
            pacienteService.salvar(paciente);
        }
    }

    public void desassociarUsuarioDePessoa(Usuario usuario) {
        if (usuario.getFuncao() == Funcao.MEDICO) {
            Medico medico = medicoService.buscarPorUsuario(usuario);
            medico.setUsuario(null);
            medicoService.salvar(medico);
        }

        if (usuario.getFuncao() == Funcao.PACIENTE) {
            Paciente paciente = pacienteService.buscarPorUsuario(usuario);
            paciente.setUsuario(null);
            pacienteService.salvar(paciente);
        }
    }

}
