package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.PacienteRequisicao;
import com.consultas.api_consultas.dtos.respostas.PacienteResposta;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Sexo;
import com.consultas.api_consultas.services.PacienteService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/pacientes")
@RequiredArgsConstructor
@Tag(name = "Pacientes", description = "Operações relacionadas ao gerenciamento de pacientes")
public class PacienteController {

    private final PacienteService pacienteService;


    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Cadastrar novo paciente")
    @ApiResponse(responseCode = "201", description = "Paciente cadastrado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para cadastro", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PacienteResposta> salvarCadastroPaciente(@RequestBody @Valid final PacienteRequisicao requisicao) {
        Paciente pacienteNovo = requisicao.dtoParaPaciente();
        Paciente pacienteSalvo = pacienteService.salvar(pacienteNovo);
        PacienteResposta dto = new PacienteResposta(pacienteSalvo);
        return ResponseEntity.status(HttpStatus.CREATED).body(dto);
    }

    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Listar pacientes, podendo filtrar por nome, CPF, sexo e ativo")
    @ApiResponse(responseCode = "200", description = "Lista de pacientes retornada com sucesso")
    public ResponseEntity<List<PacienteResposta>> listarTodosPacientes(
            @RequestParam(required = false) String nome,
            @RequestParam(required = false) String cpf,
            @RequestParam(required = false) Sexo sexo,
            @RequestParam(required = false) Boolean ativo
    ) {
        List<Paciente> pacientes = pacienteService.buscarPacientes(nome, cpf, sexo, ativo);
        List<PacienteResposta> dtos = pacientes.stream()
                .map(PacienteResposta::new)
                .toList();
        return ResponseEntity.ok(dtos);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA', 'PACIENTE')")
    @Operation(summary = "Buscar paciente por ID")
    @ApiResponse(responseCode = "200", description = "Paciente encontrado")
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PacienteResposta> buscarPacientePorId(@PathVariable Long id) {
        Paciente pacienteRetornado = pacienteService.buscarPorId(id);
        PacienteResposta dto = new PacienteResposta(pacienteRetornado);
        return ResponseEntity.ok(dto);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA', 'PACIENTE')")
    @Operation(summary = "Editar dados de um paciente por ID")
    @ApiResponse(responseCode = "200", description = "Paciente atualizado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "409", description = "Já existe um paciente cadastrado com o mesmo e-mail", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<PacienteResposta> editarPacientePorId(@PathVariable Long id, @RequestBody @Valid final PacienteRequisicao requisicao) {
        Paciente pacienteAtualizado = requisicao.dtoParaPaciente();
        Paciente pacienteSalvo = pacienteService.atualizar(id, pacienteAtualizado);
        PacienteResposta dto = new PacienteResposta(pacienteSalvo);
        return ResponseEntity.ok(dto);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Excluir paciente por ID")
    @ApiResponse(responseCode = "204", description = "Paciente excluído com sucesso")
    @ApiResponse(responseCode = "400", description = "Paciente deve estar inativo para ser excluído")
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado")
    public ResponseEntity<Void> excluirPacientePorId(@PathVariable Long id) {
        pacienteService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/inativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Inativar paciente por ID")
    @ApiResponse(responseCode = "204", description = "Paciente inativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado")
    public ResponseEntity<Void> inativarPacientePorId(@PathVariable Long id) {
        pacienteService.inativarPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/ativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Ativar paciente por ID")
    @ApiResponse(responseCode = "204", description = "Paciente ativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Paciente não encontrado")
    public ResponseEntity<Void> ativarPacientePorId(@PathVariable Long id) {
        pacienteService.ativarPorId(id);
        return ResponseEntity.noContent().build();
    }

}
