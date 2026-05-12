package com.consultas.api_consultas.repositories;

import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Especialidade;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.enums.SiglaCrm;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@DataJpaTest
@ActiveProfiles("test")
class MedicoRepositoryTest {

    @Autowired
    MedicoRepository medicoRepository;

    @Autowired
    UsuarioRepository usuarioRepository;

    @Test
    @DisplayName("findByUsuarioUsernameAndAtivo: retorna médico ativo associado ao username")
    void deveBuscarPorUsernameEAtivo() {
        Usuario usuario = usuarioRepository.save(Usuario.builder()
                .username("dr.joao")
                .email("joao@clinica.com")
                .senha("hash")
                .funcao(Funcao.MEDICO)
                .ativo(true)
                .build());

        Medico medico = new Medico("Joao", "joao@clinica.com", "11912345678",
                SiglaCrm.SP, "123456", Especialidade.CARDIOLOGIA);
        medico.setUsuario(usuario);
        medicoRepository.save(medico);

        Optional<Medico> achado = medicoRepository.findByUsuarioUsernameAndAtivo("dr.joao", true);

        assertTrue(achado.isPresent());
        assertEquals("Joao", achado.get().getNome());
    }

}
