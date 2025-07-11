package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.services.UsuarioService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/usuarios")
@RequiredArgsConstructor
@Tag(name = "Usuários", description = "Operações relacionadas ao gerenciamento de usuários do sistema")
public class UsuarioController {

    private final UsuarioService usuarioService;


    @PostMapping
    @Operation(summary = "Cadastrar novo usuário e associar a médico/paciente")
    public ResponseEntity<UsuarioResposta> criarUsuario(@RequestBody @Valid UsuarioCadastroDto requisicao) {
        UsuarioResposta resposta = usuarioService.salvar(requisicao);
        return ResponseEntity.status(HttpStatus.CREATED).body(resposta);
    }

    @GetMapping
    @Operation(summary = "Listar todos os usuários")
    @ApiResponse(responseCode = "200", description = "Lista de usuários retornada com sucesso")
    public ResponseEntity<List<UsuarioResposta>> listarUsuarios() {
        List<Usuario> usuarios = usuarioService.buscarTodos();
        List<UsuarioResposta> dtos = usuarios.stream()
                .map(UsuarioResposta::new)
                .toList();

        return ResponseEntity.ok(dtos);
    }

    @GetMapping("/{id}")
    @Operation(summary = "Buscar usuário por ID")
    @ApiResponse(responseCode = "200", description = "Usuário encontrado")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<UsuarioResposta> buscarUsuarioPorId(@PathVariable Long id) {
        Usuario usuario = usuarioService.buscarPorId(id);
        UsuarioResposta dto = new UsuarioResposta(usuario);
        return ResponseEntity.ok(dto);
    }

    @PutMapping("/{id}")
    @Operation(summary = "Editar dados de um usuário por ID")
    @ApiResponse(responseCode = "200", description = "Usuário atualizado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<UsuarioResposta> editarUsuarioPorId(
            @PathVariable Long id,
            @RequestBody @Valid UsuarioAtualizacaoDto requisicao
    ) {
        Usuario usuarioAtualizado = usuarioService.atualizar(id, requisicao);
        UsuarioResposta dto = new UsuarioResposta(usuarioAtualizado);
        return ResponseEntity.ok(dto);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Excluir usuário por ID")
    @ApiResponse(responseCode = "204", description = "Usuário excluído com sucesso")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> excluirUsuarioPorId(@PathVariable Long id) {
        usuarioService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/inativar/{id}")
    @Operation(summary = "Inativar usuário por ID")
    @ApiResponse(responseCode = "204", description = "Usuário inativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> inativarUsuarioPorId(@PathVariable Long id) {
        usuarioService.inativarPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/ativar/{id}")
    @Operation(summary = "Ativar usuário por ID")
    @ApiResponse(responseCode = "204", description = "Usuário ativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> ativarUsuarioPorId(@PathVariable Long id) {
        usuarioService.ativarPorId(id);
        return ResponseEntity.noContent().build();
    }

}
