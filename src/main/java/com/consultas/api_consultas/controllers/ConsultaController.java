package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.ConsultaAtualizacaoDto;
import com.consultas.api_consultas.dtos.requisicoes.ConsultaCadastroDto;
import com.consultas.api_consultas.dtos.respostas.ConsultaResposta;
import com.consultas.api_consultas.entities.Consulta;
import com.consultas.api_consultas.enums.StatusConsulta;
import com.consultas.api_consultas.services.ConsultaService;
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

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/consultas")
@RequiredArgsConstructor
@Tag(name = "Consultas", description = "Operações relacionadas ao gerenciamento de consultas")
public class ConsultaController {

    private final ConsultaService consultaService;


    @PostMapping
    @Operation(summary = "Cadastrar nova consulta")
    @ApiResponse(responseCode = "201", description = "Consulta cadastrada com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para cadastro", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaResposta> salvarCadastroConsulta(@RequestBody @Valid final ConsultaCadastroDto requisicao) {
        Consulta consultaNova = requisicao.dtoParaConsulta();
        Consulta consultaSalva = consultaService.salvar(consultaNova);
        ConsultaResposta resposta = new ConsultaResposta(consultaSalva);
        return ResponseEntity.status(HttpStatus.CREATED).body(resposta);
    }

    @GetMapping
    @Operation(summary = "Listar consultas, podendo filtrar por data atendimento, medico, paciente e status")
    @ApiResponse(responseCode = "200", description = "Lista de consultas retornada com sucesso")
    public ResponseEntity<List<ConsultaResposta>> listarTodasConsultas(
            @RequestParam(required = false) LocalDate dataAtendimento,
            @RequestParam(required = false) Long medicoId,
            @RequestParam(required = false) Long pacienteId,
            @RequestParam(required = false) StatusConsulta status
            ) {
        List<Consulta> consultas = consultaService.buscarConsultas(medicoId, pacienteId, dataAtendimento, status);
        List<ConsultaResposta> reposta = consultas.stream()
                .map(ConsultaResposta::new)
                .toList();
        return ResponseEntity.ok(reposta);
    }

    @GetMapping("/{id}")
    @Operation(summary = "Buscar consultas por ID")
    @ApiResponse(responseCode = "200", description = "Consulta encontrada")
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaResposta> buscarConsultaPorId(@PathVariable Long id) {
        Consulta consultaRetornada = consultaService.buscarPorId(id);
        ConsultaResposta reposta = new ConsultaResposta(consultaRetornada);
        return ResponseEntity.ok(reposta);
    }

    @PutMapping("/{id}")
    @Operation(summary = "Editar dados de uma consulta por ID")
    @ApiResponse(responseCode = "200", description = "Consulta atualizada com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaResposta> editarConsultaPorId(@PathVariable Long id, @RequestBody @Valid final ConsultaAtualizacaoDto requisicao) {
        Consulta consultaAtualizada = requisicao.dtoParaConsulta();
        Consulta consultaSalva = consultaService.atualizar(id, consultaAtualizada);
        ConsultaResposta reposta = new ConsultaResposta(consultaSalva);
        return ResponseEntity.ok(reposta);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Excluir consulta por ID")
    @ApiResponse(responseCode = "204", description = "Consulta excluída com sucesso")
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada")
    public ResponseEntity<Void> excluirConsultaPorId(@PathVariable Long id) {
        consultaService.removerPorId(id);
        return ResponseEntity.noContent().build();
    }

}
