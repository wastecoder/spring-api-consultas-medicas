package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoResposta;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.enums.SiglaCrm;
import com.consultas.api_consultas.services.MedicoService;
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
@RequestMapping("/medicos")
@RequiredArgsConstructor
@Tag(name = "Médicos", description = "Operações relacionadas ao gerenciamento de médicos")
public class MedicoController {

    private final MedicoService medicoService;


    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Cadastrar novo médico")
    @ApiResponse(responseCode = "201", description = "Médico cadastrado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para cadastro", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<MedicoResposta> salvarCadastroMedico(@RequestBody @Valid final MedicoRequisicao requisicao) {
        Medico medicoNovo = requisicao.dtoParaMedico();
        Medico medicoSalvo = medicoService.salvar(medicoNovo);
        MedicoResposta dto = new MedicoResposta(medicoSalvo);
        return ResponseEntity.status(HttpStatus.CREATED).body(dto);
    }

    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Listar médicos, podendo filtrar por nome, CRM (sigla + dígitos) e ativo")
    @ApiResponse(responseCode = "200", description = "Lista de médicos retornada com sucesso")
    public ResponseEntity<List<MedicoResposta>> listarMedicos(
            @RequestParam(value = "nome", required = false) String nome,
            @RequestParam(value = "crmSigla", required = false) SiglaCrm crmSigla,
            @RequestParam(value = "crmDigitos", required = false) String crmDigitos,
            @RequestParam(value = "ativo", required = false) Boolean ativo
    ) {
        List<Medico> medicos = medicoService.buscarMedicos(nome, crmSigla, crmDigitos, ativo);
        List<MedicoResposta> dtos = medicos.stream()
                .map(MedicoResposta::new)
                .toList();
        return ResponseEntity.ok(dtos);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Buscar médico por ID")
    @ApiResponse(responseCode = "200", description = "Médico encontrado")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<MedicoResposta> buscarMedicoPorId(@PathVariable Long id) {
        Medico medico = medicoService.buscarPorId(id);
        MedicoResposta dto = new MedicoResposta(medico);
        return ResponseEntity.ok(dto);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Editar dados de um médico por ID")
    @ApiResponse(responseCode = "200", description = "Médico atualizado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Médico não encontrado", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "409", description = "Já existe um médico cadastrado com o mesmo CRM ou e-mail", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<MedicoResposta> editarMedicoPorId(@PathVariable Long id, @RequestBody @Valid final MedicoRequisicao requisicao) {
        Medico medicoAtualizado = requisicao.dtoParaMedico();
        Medico medicoSalvo = medicoService.atualizar(id, medicoAtualizado);
        MedicoResposta dto = new MedicoResposta(medicoSalvo);
        return ResponseEntity.ok(dto);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Excluir médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico excluído com sucesso")
    @ApiResponse(responseCode = "400", description = "Médico deve estar inativo para ser excluído")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public ResponseEntity<Void> excluirMedicoPorId(@PathVariable Long id) {
        medicoService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/inativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Inativar médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico inativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public ResponseEntity<Void> inativarMedicoPorId(@PathVariable Long id) {
        medicoService.inativarPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/ativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Ativar médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico ativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public ResponseEntity<Void> ativarMedicoPorId(@PathVariable Long id) {
        medicoService.ativarPorId(id);
        return ResponseEntity.noContent().build();
    }

}
