package com.consultas.api_consultas.utils;

import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Paciente;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class SecurityUtil {

    public String getLoggedUsername() {
        Authentication auth = getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            throw new AccessDeniedException("Usuário não autenticado");
        }
        return auth.getName();
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
