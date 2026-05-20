package com.consultas.api_consultas.security;

import com.consultas.api_consultas.constants.AppConstants;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.jwt.JwtEncoderParameters;
import org.springframework.stereotype.Service;
import org.springframework.security.oauth2.jwt.JwtEncoder;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class JwtService {

    private final JwtEncoder encoder;

    /**
     * Gera o access token. O {@code sub} carrega o ID do usuário — identidade
     * estável e comum a todos os perfis — enquanto o username vai no claim
     * {@code username}.
     */
    public String generateToken(Authentication authentication, Long usuarioId) {
        Instant now = Instant.now();

        List<String> scopes = authentication.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .toList();

        var claims = JwtClaimsSet.builder()
                .issuer("spring-security-jwt")
                .id(UUID.randomUUID().toString())
                .issuedAt(now)
                .expiresAt(now.plusSeconds(AppConstants.JWT_EXPIRACAO_SEGUNDOS))
                .subject(String.valueOf(usuarioId))
                .claim("username", authentication.getName())
                .claim("scope", scopes)
                .build();

        return encoder.encode(JwtEncoderParameters.from(claims)).getTokenValue();
    }

}
