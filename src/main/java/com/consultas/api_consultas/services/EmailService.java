package com.consultas.api_consultas.services;

public interface EmailService {

    // Envia o email com link de redefinicao de senha.
    // Falhas de SMTP sao engolidas (apenas logadas) para nao revelar se o email existe no sistema.
    void enviarEmailRedefinicaoSenha(String destinatario, String linkRedefinicao);

}
