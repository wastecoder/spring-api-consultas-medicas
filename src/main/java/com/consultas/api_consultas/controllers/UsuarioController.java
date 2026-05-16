package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.UsuarioResposta;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.mappers.UsuarioMapper;
import com.consultas.api_consultas.services.UsuarioService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/usuarios")
@RequiredArgsConstructor
@Validated
@Tag(name = "Usuários", description = "Operações relacionadas ao gerenciamento de usuários do sistema")
public class UsuarioController {

    private final UsuarioService usuarioService;
    private final UsuarioMapper usuarioMapper;


    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Cadastrar novo usuário e associar a médico/paciente")
    public ResponseEntity<UsuarioResposta> criarUsuario(@RequestBody @Valid UsuarioCadastroDto requisicao) {
        UsuarioResposta resposta = usuarioService.salvar(requisicao);
        return ResponseEntity.status(HttpStatus.CREATED).body(resposta);
    }

    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Listar usuários com filtros e ordenação")
    @ApiResponse(responseCode = "200", description = "Lista de usuários retornada com sucesso")
    public ResponseEntity<PageResponse<UsuarioResposta>> listarUsuarios(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(value = "username", required = false) String username,
            @RequestParam(value = "funcao", required = false) Funcao funcao,
            @RequestParam(value = "ativo", required = false) Boolean ativo,
            @RequestParam(value = "ordenarPor", defaultValue = "username") String ordenarPor,
            @RequestParam(value = "direcao", defaultValue = "asc") String direcao
    ) {
        return ResponseEntity.ok(
                usuarioService.buscarUsuarios(pagina, tamanho, username, funcao, ativo, ordenarPor, direcao)
        );
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Buscar usuário por ID")
    @ApiResponse(responseCode = "200", description = "Usuário encontrado")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<UsuarioResposta> buscarUsuarioPorId(@PathVariable @Min(1) Long id) {
        Usuario usuario = usuarioService.buscarPorId(id);
        UsuarioResposta dto = usuarioMapper.paraRespostaComAuditoria(usuario);
        return ResponseEntity.ok(dto);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Editar dados de um usuário por ID")
    @ApiResponse(responseCode = "200", description = "Usuário atualizado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<UsuarioResposta> editarUsuarioPorId(
            @PathVariable @Min(1) Long id,
            @RequestBody @Valid UsuarioAtualizacaoDto requisicao
    ) {
        Usuario usuarioAtualizado = usuarioService.atualizar(id, requisicao);
        UsuarioResposta dto = usuarioMapper.paraResposta(usuarioAtualizado);
        return ResponseEntity.ok(dto);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Excluir usuário por ID")
    @ApiResponse(responseCode = "204", description = "Usuário excluído com sucesso")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> excluirUsuarioPorId(@PathVariable @Min(1) Long id) {
        usuarioService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/inativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Inativar usuário por ID")
    @ApiResponse(responseCode = "204", description = "Usuário inativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> inativarUsuarioPorId(@PathVariable @Min(1) Long id) {
        usuarioService.inativarPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/ativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Ativar usuário por ID")
    @ApiResponse(responseCode = "204", description = "Usuário ativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Usuário não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> ativarUsuarioPorId(@PathVariable @Min(1) Long id) {
        usuarioService.ativarPorId(id);
        return ResponseEntity.noContent().build();
    }

}
