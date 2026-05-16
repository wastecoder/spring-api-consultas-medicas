package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.dtos.requisicoes.SignupDto;
import com.consultas.api_consultas.dtos.requisicoes.UsuarioCadastroDto;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;
import com.consultas.api_consultas.entities.Paciente;
import com.consultas.api_consultas.enums.Funcao;
import com.consultas.api_consultas.repositories.PacienteRepository;
import com.consultas.api_consultas.security.AuthenticationService;
import com.consultas.api_consultas.services.SignupService;
import com.consultas.api_consultas.services.UsuarioService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class SignupServiceImpl implements SignupService {

    private final PacienteRepository pacienteRepository;
    private final UsuarioService usuarioService;
    private final AuthenticationService authenticationService;

    @Override
    @Transactional
    public AuthTokenDTO cadastrarPaciente(SignupDto dto) {
        log.info("Cadastro self-service de paciente: username={}", dto.username());

        // 1) Persiste o Paciente primeiro: UsuarioService.salvar associa pelo idAssociado,
        //    e a regra UsuarioRules.associarUsuarioAoMedicoOuPaciente busca o Paciente
        //    por id antes de setar a referencia. Sem o id persistido, esse lookup falha.
        Paciente paciente = new Paciente(
                dto.nome(),
                dto.email(),
                dto.telefone(),
                dto.cpf(),
                dto.sexo(),
                dto.dataNascimento()
        );
        paciente.setAtivo(true);
        Paciente pacienteSalvo = pacienteRepository.save(paciente);

        // 2) Delega a criacao do Usuario(funcao=PACIENTE) ao service ja existente,
        //    que cuida de hash de senha, validacao de unicidade (username/email) e
        //    da associacao bidirecional Usuario <-> Paciente. Se qualquer passo falhar,
        //    @Transactional faz rollback do save do Paciente acima.
        UsuarioCadastroDto usuarioDto = UsuarioCadastroDto.builder()
                .username(dto.username())
                .email(dto.email())
                .senha(dto.senha())
                .funcao(Funcao.PACIENTE)
                .idAssociado(pacienteSalvo.getId())
                .build();
        usuarioService.salvar(usuarioDto);

        // 3) Auto-login: emite par de tokens reusando o mesmo caminho do /auth/login.
        Authentication auth = new UsernamePasswordAuthenticationToken(
                dto.username(),
                null,
                List.of(new SimpleGrantedAuthority("ROLE_" + Funcao.PACIENTE.name()))
        );
        return authenticationService.authenticate(auth);
    }

}
