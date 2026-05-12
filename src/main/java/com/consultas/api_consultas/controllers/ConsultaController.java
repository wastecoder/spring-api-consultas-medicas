package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.PageResponse;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaCadastroDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.mappers.ConsultaMapper;
import com.consultas.api_consultas.services.ConsultaService;
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

import java.time.LocalDate;

@RestController
@RequestMapping("/consultas")
@RequiredArgsConstructor
@Validated
@Tag(name = "Consultas", description = "Operações relacionadas ao gerenciamento de consultas")
public class ConsultaController {

    private final ConsultaService consultaService;
    private final ConsultaMapper consultaMapper;


    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA', 'PACIENTE')")
    @Operation(summary = "Cadastrar nova consulta")
    @ApiResponse(responseCode = "201", description = "Consulta cadastrada com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para cadastro", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaResposta> salvarCadastroConsulta(@RequestBody @Valid final ConsultaCadastroDto requisicao) {
        Consulta consultaNova = consultaMapper.paraEntidade(requisicao);
        Consulta consultaSalva = consultaService.salvar(consultaNova);
        ConsultaResposta resposta = consultaMapper.paraResposta(consultaSalva);
        return ResponseEntity.status(HttpStatus.CREATED).body(resposta);
    }

    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA', 'MEDICO', 'PACIENTE')")
    @Operation(summary = "Listar consultas, podendo filtrar por data atendimento, medico, paciente e status")
    @ApiResponse(responseCode = "200", description = "Lista de consultas retornada com sucesso")
    public ResponseEntity<PageResponse<ConsultaResposta>> listarTodasConsultas(
            @RequestParam(defaultValue = AppConstants.PAGINACAO_PAGINA_DEFAULT) @Min(0) int pagina,
            @RequestParam(defaultValue = AppConstants.PAGINACAO_TAMANHO_DEFAULT) @Min(1) int tamanho,
            @RequestParam(required = false) LocalDate dataAtendimento,
            @RequestParam(required = false) @Min(1) Long medicoId,
            @RequestParam(required = false) @Min(1) Long pacienteId,
            @RequestParam(required = false) StatusConsulta status
    ) {
        PageResponse<ConsultaResposta> consultas = consultaService.buscarConsultas(pagina, tamanho, medicoId, pacienteId, dataAtendimento, status);
        return ResponseEntity.ok(consultas);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA', 'MEDICO', 'PACIENTE')")
    @Operation(summary = "Buscar consultas por ID")
    @ApiResponse(responseCode = "200", description = "Consulta encontrada")
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaResposta> buscarConsultaPorId(@PathVariable @Min(1) Long id) {
        Consulta consultaRetornada = consultaService.buscarPorId(id);
        ConsultaResposta resposta = consultaMapper.paraRespostaComAuditoria(consultaRetornada);
        return ResponseEntity.ok(resposta);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Editar dados de uma consulta por ID")
    @ApiResponse(responseCode = "200", description = "Consulta atualizada com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaResposta> editarConsultaPorId(@PathVariable @Min(1) Long id, @RequestBody @Valid final ConsultaAtualizacaoDto requisicao) {
        Consulta consultaSalva = consultaService.atualizar(id, requisicao);
        ConsultaResposta resposta = consultaMapper.paraResposta(consultaSalva);
        return ResponseEntity.ok(resposta);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'RECEPCIONISTA')")
    @Operation(summary = "Excluir consulta por ID")
    @ApiResponse(responseCode = "204", description = "Consulta excluída com sucesso")
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada")
    public ResponseEntity<Void> excluirConsultaPorId(@PathVariable @Min(1) Long id) {
        consultaService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

}
