package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.ConsultaRequisicao;
import com.consultas.api_consultas.dtos.respostas.ConsultaRespostaFormatada;
import com.consultas.api_consultas.entities.Consulta;
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
    public ResponseEntity<ConsultaRespostaFormatada> salvarCadastroConsulta(@RequestBody @Valid final ConsultaRequisicao requisicao) {
        Consulta consultaNova = requisicao.dtoParaConsulta();
        Consulta consultaSalva = consultaService.salvar(consultaNova);
        ConsultaRespostaFormatada resposta = new ConsultaRespostaFormatada(consultaSalva);
        return ResponseEntity.status(HttpStatus.CREATED).body(resposta);
    }

    @GetMapping
    @Operation(summary = "Listar todas as consultas")
    @ApiResponse(responseCode = "200", description = "Lista de consultas retornada com sucesso")
    public ResponseEntity<List<ConsultaRespostaFormatada>> listarTodasConsultas() {
        List<Consulta> consultas = consultaService.buscarTodos();
        List<ConsultaRespostaFormatada> reposta = consultas.stream()
                .map(ConsultaRespostaFormatada::new)
                .toList();
        return ResponseEntity.ok(reposta);
    }

    @GetMapping("/{id}")
    @Operation(summary = "Buscar consultas por ID")
    @ApiResponse(responseCode = "200", description = "Consulta encontrada")
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaRespostaFormatada> buscarConsultaPorId(@PathVariable Long id) {
        Consulta consultaRetornada = consultaService.buscarPorId(id);
        ConsultaRespostaFormatada reposta = new ConsultaRespostaFormatada(consultaRetornada);
        return ResponseEntity.ok(reposta);
    }

    @PutMapping("/{id}")
    @Operation(summary = "Editar dados de uma consulta por ID")
    @ApiResponse(responseCode = "200", description = "Consulta atualizada com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Consulta não encontrada", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<ConsultaRespostaFormatada> editarConsultaPorId(@PathVariable Long id, @RequestBody @Valid final ConsultaRequisicao requisicao) {
        Consulta consultaAtualizada = requisicao.dtoParaConsulta();
        Consulta consultaSalva = consultaService.atualizar(id, consultaAtualizada);
        ConsultaRespostaFormatada reposta = new ConsultaRespostaFormatada(consultaSalva);
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
