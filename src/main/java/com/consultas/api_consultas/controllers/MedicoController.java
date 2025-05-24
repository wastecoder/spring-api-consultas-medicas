package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoDetalhadoDto;
import com.consultas.api_consultas.dtos.respostas.MedicoListaDto;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.services.MedicoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/medicos")
@AllArgsConstructor
@Tag(name = "Médicos", description = "Operações relacionadas ao gerenciamento de médicos")
public class MedicoController {

    private final MedicoService medicoService;


    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Operation(summary = "Cadastrar novo médico")
    @ApiResponse(responseCode = "201", description = "Médico cadastrado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para cadastro", content = @Content(schema = @Schema(hidden = true)))
    public MedicoDetalhadoDto salvarCadastroMedico(@RequestBody @Valid final MedicoRequisicao requisicao) {
        Medico medicoNovo = requisicao.dtoParaMedico();
        Medico medicoSalvo = medicoService.salvar(medicoNovo);
        return new MedicoDetalhadoDto(medicoSalvo);
    }

    @GetMapping
    @ResponseStatus(HttpStatus.OK)
    @Operation(summary = "Listar todos os médicos")
    @ApiResponse(responseCode = "200", description = "Lista de médicos retornada com sucesso")
    public List<MedicoListaDto> listarTodosMedicos() {
        List<Medico> medicos = medicoService.buscarTodos();
        return medicos.stream()
                .map(MedicoListaDto::new)
                .toList();
    }

    @GetMapping("/{id}")
    @ResponseStatus(HttpStatus.OK)
    @Operation(summary = "Buscar médico por ID")
    @ApiResponse(responseCode = "200", description = "Médico encontrado")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado", content = @Content(schema = @Schema(hidden = true)))
    public MedicoDetalhadoDto buscarMedicoPorId(@PathVariable Long id) {
        Medico medico = medicoService.buscarPorId(id);
        return new MedicoDetalhadoDto(medico);
    }

    @PutMapping("/{id}")
    @ResponseStatus(HttpStatus.OK)
    @Operation(summary = "Editar dados de um médico por ID")
    @ApiResponse(responseCode = "200", description = "Médico atualizado com sucesso")
    @ApiResponse(responseCode = "400", description = "Dados inválidos para edição", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "404", description = "Médico não encontrado", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "409", description = "Já existe um médico cadastrado com o mesmo CRM ou e-mail", content = @Content(schema = @Schema(hidden = true)))
    public MedicoDetalhadoDto editarPorId(@PathVariable Long id, @RequestBody @Valid final MedicoRequisicao requisicao) {
        Medico medicoAtualizado = requisicao.dtoParaMedico();
        Medico medicoSalvo = medicoService.atualizar(id, medicoAtualizado);
        return new MedicoDetalhadoDto(medicoSalvo);
    }

    @DeleteMapping("/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @Operation(summary = "Excluir médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico excluído com sucesso")
    @ApiResponse(responseCode = "400", description = "Médico deve estar inativo para ser excluído")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public void excluirMedicoPorId(@PathVariable Long id) {
        medicoService.removerPorId(id);
    }

    @PatchMapping("/inativar/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @Operation(summary = "Inativar médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico inativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public void inativarMedicoPorId(@PathVariable Long id) {
        medicoService.inativarPorId(id);
    }

    @PatchMapping("/ativar/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @Operation(summary = "Ativar médico por ID")
    @ApiResponse(responseCode = "204", description = "Médico ativado com sucesso")
    @ApiResponse(responseCode = "404", description = "Médico não encontrado")
    public void ativarMedicoPorId(@PathVariable Long id) {
        medicoService.ativarPorId(id);
    }

}
