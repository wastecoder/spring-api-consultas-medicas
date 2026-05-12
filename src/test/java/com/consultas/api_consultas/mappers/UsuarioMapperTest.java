package com.consultas.api_consultas.mappers;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@DisplayName("UsuarioMapper")
class UsuarioMapperTest {

    private final UsuarioMapper mapper = new UsuarioMapperImpl();

    @Test
    @DisplayName("paraEntidade: NÃO copia a senha (criptografia continua no service)")
    void deveConverterCadastroEmEntidadeIgnorandoSenha() {
        UsuarioCadastroDto dto = UsuarioCadastroDto.builder()
                .username("usuario_teste")
                .email("usuario_teste@example.com")
                .senha("senha123")
                .funcao(Funcao.RECEPCIONISTA)
                .idAssociado(null)
                .build();

        Usuario usuario = mapper.paraEntidade(dto);

        assertNotNull(usuario);
        assertNull(usuario.getId());
        assertEquals("usuario_teste", usuario.getUsername());
        assertEquals("usuario_teste@example.com", usuario.getEmail());
        assertEquals(Funcao.RECEPCIONISTA, usuario.getFuncao());
        assertNull(usuario.getSenha(), "Mapper não deve copiar senha — service trata BCrypt");
    }

    @Test
    @DisplayName("paraResposta: copia campos e NÃO expõe senha")
    void deveConverterEntidadeEmResposta() {
        Usuario usuario = Usuario.builder()
                .id(5L)
                .username("admin")
                .email("admin@clinica.com")
                .senha("hash-bcrypt")
                .funcao(Funcao.ADMIN)
                .ativo(true)
                .build();

        UsuarioResposta resposta = mapper.paraResposta(usuario);

        assertEquals(5L, resposta.getId());
        assertEquals("admin", resposta.getUsername());
        assertEquals("admin@clinica.com", resposta.getEmail());
        assertEquals(Funcao.ADMIN, resposta.getFuncao());
        assertEquals(true, resposta.isAtivo());
        assertNull(resposta.getAuditoria());
    }

    @Test
    @DisplayName("aplicarAtualizacao: atualiza username/email preservando id, ativo, funcao e senha")
    void deveAplicarAtualizacaoPreservandoSenhaEFuncao() {
        Usuario existente = Usuario.builder()
                .id(7L)
                .username("antigo")
                .email("antigo@clinica.com")
                .senha("hash-antigo")
                .funcao(Funcao.MEDICO)
                .ativo(true)
                .build();

        UsuarioAtualizacaoDto dto = UsuarioAtualizacaoDto.builder()
                .username("novo")
                .email("novo@clinica.com")
                .senha("ignorada-pelo-mapper")
                .build();

        mapper.aplicarAtualizacao(dto, existente);

        assertEquals(7L, existente.getId());
        assertEquals("novo", existente.getUsername());
        assertEquals("novo@clinica.com", existente.getEmail());
        assertEquals("hash-antigo", existente.getSenha(), "mapper não deve mexer na senha");
        assertEquals(Funcao.MEDICO, existente.getFuncao(), "mapper não deve mexer na função");
        assertEquals(true, existente.getAtivo());
    }

    @Test
    @DisplayName("paraResposta: usuário inativo é refletido no DTO")
    void deveRefletirAtivoFalseNoDto() {
        Usuario usuario = Usuario.builder()
                .id(6L)
                .username("desligado")
                .email("desligado@clinica.com")
                .senha("hash")
                .funcao(Funcao.MEDICO)
                .ativo(false)
                .build();

        UsuarioResposta resposta = mapper.paraResposta(usuario);

        assertFalse(resposta.isAtivo());
    }
}
