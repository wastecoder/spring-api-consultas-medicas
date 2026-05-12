package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.enums.Sexo;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDate;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
class PacienteRepositoryTest {

    @Autowired
    PacienteRepository pacienteRepository;

    @Autowired
    UsuarioRepository usuarioRepository;

    @Test
    @DisplayName("findByUsuarioUsernameAndAtivo: retorna paciente ativo associado ao username")
    void deveBuscarPorUsernameEAtivo() {
        Usuario usuario = usuarioRepository.save(Usuario.builder()
                .username("ana.paciente")
                .email("ana@email.com")
                .senha("hash")
                .funcao(Funcao.PACIENTE)
                .ativo(true)
                .build());

        Paciente paciente = new Paciente("Ana", "ana@email.com", "11988887777",
                "12345678901", Sexo.FEMININO, LocalDate.of(1990, 5, 15));
        paciente.setUsuario(usuario);
        pacienteRepository.save(paciente);

        Optional<Paciente> achado = pacienteRepository.findByUsuarioUsernameAndAtivo("ana.paciente", true);

        assertTrue(achado.isPresent());
        assertEquals("Ana", achado.get().getNome());
    }

}
