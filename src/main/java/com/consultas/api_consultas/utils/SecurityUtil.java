package com.consultas.api_consultas.utils;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.repositories.MedicoRepository;
import com.consultas.api_consultas.repositories.PacienteRepository;
import jakarta.persistence.EntityNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class SecurityUtil {

    private final MedicoRepository medicoRepository;
    private final PacienteRepository pacienteRepository;


    public String getLoggedUsername() {
        Authentication auth = getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            throw new AccessDeniedException("Usuário não autenticado");
        }
        return auth.getName();
    }

    public Medico getLoggedDoctor() {
        if (!isDoctor()) {
            throw new AccessDeniedException("Usuário logado não é um médico.");
        }

        return medicoRepository
                .findByUsuarioUsername(getLoggedUsername())
                .orElseThrow(() -> new EntityNotFoundException("Médico não encontrado para usuário logado"));
    }

    public Paciente getLoggedPatient() {
        if (!isPatient()) {
            throw new AccessDeniedException("Usuário logado não é um paciente.");
        }

        return pacienteRepository
                .findByUsuarioUsername(getLoggedUsername())
                .orElseThrow(() -> new EntityNotFoundException("Paciente não encontrado para usuário logado"));
    }

    public boolean temRole(String role) {
        Authentication auth = getAuthentication();
        return auth.getAuthorities().stream()
                .anyMatch(grantedAuthority -> grantedAuthority.getAuthority().equals("ROLE_" + role));
    }

    public boolean isAdmin() {
        return temRole("ADMIN");
    }

    public boolean isReceptionist() {
        return temRole("RECEPCIONISTA");
    }

    public boolean isPatient() {
        return temRole("PACIENTE");
    }

    public boolean isDoctor() {
        return temRole("MEDICO");
    }

    public boolean isSamePatient(Paciente paciente) {
        if (paciente == null || paciente.getUsuario() == null) return false;

        return isPatient() &&
                paciente.getUsuario().getUsername().equals(getLoggedUsername());
    }

    public boolean canAccessPatient(Paciente paciente) {
        return isAdmin() ||
                isReceptionist() ||
                isSamePatient(paciente);
    }

    public boolean isSameDoctor(Medico medico) {
        if (medico == null || medico.getUsuario() == null) return false;

        return isDoctor() &&
                medico.getUsuario().getUsername().equals(getLoggedUsername());
    }

    public boolean canAccessDoctor(Medico medico) {
        return isAdmin() ||
                isReceptionist() ||
                isSameDoctor(medico);
    }

    public boolean canAccessAppointment(Consulta consulta) {
        return isAdmin() ||
                isReceptionist() ||
                isSameDoctor(consulta.getMedico()) ||
                isSamePatient(consulta.getPaciente());
    }

    private Authentication getAuthentication() {
        return SecurityContextHolder.getContext().getAuthentication();
    }

}
