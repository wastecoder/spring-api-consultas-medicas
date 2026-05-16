package com.consultas.api_consultas.controllers;

import com.consultas.api_consultas.dtos.requisicoes.ForgotPasswordDto;
import com.consultas.api_consultas.dtos.requisicoes.LoginDTO;
import com.consultas.api_consultas.dtos.requisicoes.RefreshTokenRequestDTO;
import com.consultas.api_consultas.dtos.requisicoes.ResetPasswordDto;
import com.consultas.api_consultas.dtos.requisicoes.SignupDto;
import com.consultas.api_consultas.dtos.respostas.AuthTokenDTO;
import com.consultas.api_consultas.security.AuthenticationService;
import com.consultas.api_consultas.security.LoginRateLimitService;
import com.consultas.api_consultas.security.PasswordRecoveryRateLimitService;
import com.consultas.api_consultas.security.SignupRateLimitService;
import com.consultas.api_consultas.services.PasswordRecoveryService;
import com.consultas.api_consultas.services.SignupService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
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
    private final PasswordRecoveryService passwordRecoveryService;
    private final PasswordRecoveryRateLimitService passwordRecoveryRateLimitService;
    private final SignupService signupService;
    private final SignupRateLimitService signupRateLimitService;


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

    @PostMapping("/forgot-password")
    @Operation(summary = "Solicitar email com link de redefinição de senha")
    @ApiResponse(responseCode = "204", description = "Solicitação recebida (não confirma existência do email)")
    @ApiResponse(responseCode = "400", description = "Email inválido", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "429", description = "Muitas solicitações", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> forgotPassword(@RequestBody @Valid ForgotPasswordDto dto, HttpServletRequest request) {
        String ip = LoginRateLimitService.extrairIpCliente(request);
        passwordRecoveryRateLimitService.verificar(ip, dto.email());
        passwordRecoveryService.solicitarRedefinicao(dto.email());
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/reset-password")
    @Operation(summary = "Redefinir senha usando token recebido por email")
    @ApiResponse(responseCode = "204", description = "Senha redefinida")
    @ApiResponse(responseCode = "400", description = "Payload inválido", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "401", description = "Token inválido ou expirado", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<Void> resetPassword(@RequestBody @Valid ResetPasswordDto dto) {
        passwordRecoveryService.redefinirSenha(dto.token(), dto.novaSenha());
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/signup")
    @Operation(summary = "Cadastro self-service de paciente com auto-login (cria Usuario + Paciente)")
    @ApiResponse(responseCode = "201", description = "Paciente cadastrado e par de tokens emitido")
    @ApiResponse(responseCode = "400", description = "Dados inválidos ou regra de negócio violada", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "409", description = "Username, e-mail ou CPF já cadastrados", content = @Content(schema = @Schema(hidden = true)))
    @ApiResponse(responseCode = "429", description = "Muitas tentativas de cadastro", content = @Content(schema = @Schema(hidden = true)))
    public ResponseEntity<AuthTokenDTO> signup(@RequestBody @Valid SignupDto dto, HttpServletRequest request) {
        String ip = LoginRateLimitService.extrairIpCliente(request);
        signupRateLimitService.verificar(ip, dto.email());

        AuthTokenDTO tokens = signupService.cadastrarPaciente(dto);
        return ResponseEntity.status(HttpStatus.CREATED).body(tokens);
    }

}
