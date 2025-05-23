package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.MedicoRequisicao;
import com.consultas.api_consultas.dtos.respostas.MedicoDetalhadoDto;
import com.consultas.api_consultas.dtos.respostas.MedicoListaDto;
import com.consultas.api_consultas.entities.Medico;
import com.consultas.api_consultas.services.MedicoService;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/medicos")
@AllArgsConstructor
public class MedicoController {

    private final MedicoService medicoService;


    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public MedicoDetalhadoDto salvarCadastroMedico(@RequestBody @Valid final MedicoRequisicao requisicao) {
        Medico medicoNovo = requisicao.dtoParaMedico();
        Medico medicoSalvo = medicoService.salvar(medicoNovo);
        return new MedicoDetalhadoDto(medicoSalvo);
    }

    @GetMapping
    @ResponseStatus(HttpStatus.OK)
    public List<MedicoListaDto> listarTodosMedicos() {
        List<Medico> medicos = medicoService.buscarTodos();
        return medicos.stream()
                .map(MedicoListaDto::new)
                .toList();
    }

    @GetMapping("/{id}")
    @ResponseStatus(HttpStatus.OK)
    public MedicoDetalhadoDto buscarMedicoPorId(@PathVariable Long id) {
        Medico medico = medicoService.buscarPorId(id);
        return new MedicoDetalhadoDto(medico);
    }

    @PutMapping("/{id}")
    @ResponseStatus(HttpStatus.OK)
    public MedicoDetalhadoDto editarPorId(@PathVariable Long id, @RequestBody @Valid final MedicoRequisicao requisicao) {
        Medico medicoAtualizado = requisicao.dtoParaMedico();
        Medico medicoSalvo = medicoService.atualizar(id, medicoAtualizado);
        return new MedicoDetalhadoDto(medicoSalvo);
    }

    @DeleteMapping("/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void excluirMedicoPorId(@PathVariable Long id) {
        medicoService.removerPorId(id);
    }

    @PatchMapping("/inativar/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void inativarMedicoPorId(@PathVariable Long id) {
        medicoService.inativarPorId(id);
    }

    @PatchMapping("/ativar/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void ativarMedicoPorId(@PathVariable Long id) {
        medicoService.ativarPorId(id);
    }

}
