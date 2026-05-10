package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.repositories.UsuarioRepository;
import com.consultas.api_consultas.security.UserAuthenticated;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("Service de UserDetails")
class UserDetailsServiceImplTest {

    @InjectMocks
    private UserDetailsServiceImpl service;

    @Mock
    private UsuarioRepository repository;


    @Test
    @DisplayName("Deve devolver UserAuthenticated com mesmas credenciais e role quando o usuário existe")
    void deveCarregarUsuarioExistente() {
        Usuario usuario = Usuario.builder()
                .id(1L)
                .username("admin")
                .senha("hash-bcrypt")
                .funcao(Funcao.ADMIN)
                .ativo(true)
                .build();
        when(repository.findByUsername("admin")).thenReturn(Optional.of(usuario));

        UserDetails carregado = service.loadUserByUsername("admin");

        UserAuthenticated authenticated = assertInstanceOf(UserAuthenticated.class, carregado);
        assertEquals("admin", authenticated.getUsername());
        assertEquals("hash-bcrypt", authenticated.getPassword());
        assertTrue(
                authenticated.getAuthorities().stream()
                        .anyMatch(a -> a.getAuthority().equals("ROLE_ADMIN")),
                "esperava authority ROLE_ADMIN"
        );
    }

    @Test
    @DisplayName("Deve lançar UsernameNotFoundException quando o usuário não existe")
    void deveLancarQuandoNaoEncontrado() {
        when(repository.findByUsername("nao-existe")).thenReturn(Optional.empty());

        UsernameNotFoundException ex = assertThrows(
                UsernameNotFoundException.class,
                () -> service.loadUserByUsername("nao-existe")
        );
        assertEquals("Usuário [nao-existe] não encontrado.", ex.getMessage());
    }

    @Test
    @DisplayName("Deve repassar o username recebido (mesmo null/vazio) ao repositório e tratar como não-encontrado quando vazio")
    void deveTratarUsernameVazio() {
        when(repository.findByUsername("")).thenReturn(Optional.empty());

        UsernameNotFoundException ex = assertThrows(
                UsernameNotFoundException.class,
                () -> service.loadUserByUsername("")
        );
        assertEquals("Usuário [] não encontrado.", ex.getMessage());
    }
}
