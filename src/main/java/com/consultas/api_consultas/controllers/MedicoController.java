package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
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
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/medicos")
@RequiredArgsConstructor
@Validated
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
    public ResponseEntity<PageResponse<MedicoResposta>> listarMedicos(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(value = "nome", required = false) String nome,
            @RequestParam(value = "crmSigla", required = false) SiglaCrm crmSigla,
            @RequestParam(value = "crmDigitos", required = false) String crmDigitos,
            @RequestParam(value = "ativo", required = false) Boolean ativo
    ) {
        PageResponse<MedicoResposta> medicos = medicoService.buscarMedicos(pagina, tamanho, nome, crmSigla, crmDigitos, ativo);
        return ResponseEntity.ok(medicos);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Buscar médico por ID")
    @ApiResponse(responseCode = "200", description = "Médico encontrado")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<MedicoResposta> buscarMedicoPorId(@PathVariable @Min(1) Long id) {
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
    public ResponseEntity<MedicoResposta> editarMedicoPorId(@PathVariable @Min(1) Long id, @RequestBody @Valid final MedicoRequisicao requisicao) {
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
    public ResponseEntity<Void> excluirMedicoPorId(@PathVariable @Min(1) Long id) {
        medicoService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/inativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Inativar médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico inativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public ResponseEntity<Void> inativarMedicoPorId(@PathVariable @Min(1) Long id) {
        medicoService.inativarPorId(id);
        return ResponseEntity.noContent().build();
    }

    @PatchMapping("/ativar/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Ativar médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico ativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public ResponseEntity<Void> ativarMedicoPorId(@PathVariable @Min(1) Long id) {
        medicoService.ativarPorId(id);
        return ResponseEntity.noContent().build();
    }

}
