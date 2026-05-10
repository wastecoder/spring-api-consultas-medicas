package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.consultas.api_consultas.dtos.requisicoes.RefreshTokenRequestDTO;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;
import com.consultas.api_consultas.security.AuthenticationService;
import com.consultas.api_consultas.security.LoginRateLimitService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
@Tag(name = "Autenticação", description = "Operações de login e autenticação de usuários")
public class AuthController {

    private final AuthenticationManager authenticationManager;
    private final AuthenticationService authService;
    private final LoginRateLimitService loginRateLimitService;


    @PostMapping("/login")
    @Operation(summary = "Autenticar usuário e gerar par de tokens (access + refresh)")
    @ApiResponse(responseCode = "200", description = "Autenticação bem-sucedida")
    @ApiResponse(responseCode = "401", description = "Credenciais inválidas", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "429", description = "Muitas tentativas de login", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<AuthTokenDTO> login(@RequestBody @Valid LoginDTO dto, HttpServletRequest request) {
        String ip = LoginRateLimitService.extrairIpCliente(request);
        loginRateLimitService.verificar(ip, dto.username());

        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(dto.username(), dto.senha())
        );

        AuthTokenDTO tokens = authService.authenticate(authentication);
        return ResponseEntity.ok(tokens);
    }

    @PostMapping("/refresh")
    @Operation(summary = "Trocar refresh token por um novo par (rotação)")
    @ApiResponse(responseCode = "200", description = "Novo par de tokens emitido")
    @ApiResponse(responseCode = "401", description = "Refresh token inválido ou expirado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<AuthTokenDTO> refresh(@RequestBody @Valid RefreshTokenRequestDTO dto) {
        AuthTokenDTO tokens = authService.refresh(dto.refreshToken());
        return ResponseEntity.ok(tokens);
    }

    @PostMapping("/logout")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Encerrar sessão: revoga o refresh token e adiciona o jti do access à blacklist")
    @ApiResponse(responseCode = "204", description = "Logout efetuado")
    @ApiResponse(responseCode = "401", description = "Sem autenticação válida", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> logout(
            @AuthenticationPrincipal Jwt jwt,
            @RequestBody @Valid RefreshTokenRequestDTO dto
    ) {
        authService.logout(jwt, dto.refreshToken());
        return ResponseEntity.noContent().build();
    }

}
