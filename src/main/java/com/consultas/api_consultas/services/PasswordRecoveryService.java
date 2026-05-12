package com.consultas.api_consultas.services;

public interface PasswordRecoveryService {

    // Inicia o fluxo de redefinicao: gera token e dispara email.
    // Sempre retorna em silencio (sem exceção) quando o email nao existe, para evitar oracle.
    void solicitarRedefinicao(String email);

    // Consome o token e troca a senha. Revoga refresh tokens existentes do usuario.
    // Lanca PasswordResetTokenInvalidoException quando o token nao existe, ja foi usado ou expirou.
    void redefinirSenha(String token, String novaSenha);

}
