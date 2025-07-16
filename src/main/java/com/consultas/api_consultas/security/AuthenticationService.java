package com.consultas.api_consultas.security;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class AuthenticationService {

    private final JwtService jwtService;

    public String authenticate(Authentication authentication) {
        log.debug("Gerando token JWT para o usuário {}", authentication.getName());
        return jwtService.generateToken(authentication);
    }

}
