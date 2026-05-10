package com.consultas.api_consultas.security;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;
import com.consultas.api_consultas.entities.RefreshToken;
import com.consultas.api_consultas.entities.Usuario;
import com.consultas.api_consultas.exceptions.RefreshTokenInvalidoException;
import com.consultas.api_consultas.repositories.UsuarioRepository;
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
public class AuthenticationService {

    private static final String TOKEN_TYPE_BEARER = "Bearer";

    private final JwtService jwtService;
    private final RefreshTokenService refreshTokenService;
    private final UsuarioRepository usuarioRepository;

    public AuthTokenDTO authenticate(Authentication authentication) {
        log.debug("Gerando par de tokens (access + refresh) para o usuário {}", authentication.getName());

        Usuario usuario = usuarioRepository.findByUsername(authentication.getName())
                .orElseThrow(() -> new IllegalStateException(
                        "Usuário autenticado não encontrado no banco: " + authentication.getName()));

        String accessToken = jwtService.generateToken(authentication);
        RefreshToken refreshToken = refreshTokenService.criar(usuario);

        return new AuthTokenDTO(
                accessToken,
                refreshToken.getToken(),
                AppConstants.JWT_EXPIRACAO_SEGUNDOS,
                TOKEN_TYPE_BEARER
        );
    }

    public AuthTokenDTO refresh(String refreshTokenValor) {
        RefreshToken novo = refreshTokenService.validarERotacionar(refreshTokenValor);
        Usuario usuario = novo.getUsuario();

        if (Boolean.FALSE.equals(usuario.getAtivo())) {
            refreshTokenService.revogar(novo.getToken());
            throw new RefreshTokenInvalidoException("Usuário inativo.");
        }

        Authentication auth = new UsernamePasswordAuthenticationToken(
                usuario.getUsername(),
                null,
                List.of(new SimpleGrantedAuthority("ROLE_" + usuario.getFuncao().name()))
        );

        String accessToken = jwtService.generateToken(auth);

        return new AuthTokenDTO(
                accessToken,
                novo.getToken(),
                AppConstants.JWT_EXPIRACAO_SEGUNDOS,
                TOKEN_TYPE_BEARER
        );
    }

}
