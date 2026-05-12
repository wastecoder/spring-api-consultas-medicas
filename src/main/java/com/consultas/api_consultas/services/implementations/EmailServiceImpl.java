package com.consultas.api_consultas.services.implementations;

import com.consultas.api_consultas.constants.AppConstants;
import com.consultas.api_consultas.services.EmailService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.MailException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class EmailServiceImpl implements EmailService {

    private final JavaMailSender mailSender;
    private final String remetente;

    public EmailServiceImpl(
            JavaMailSender mailSender,
            @Value("${app.password-recovery.email-remetente}") String remetente
    ) {
        this.mailSender = mailSender;
        this.remetente = remetente;
    }

    @Override
    public void enviarEmailRedefinicaoSenha(String destinatario, String linkRedefinicao) {
        SimpleMailMessage mensagem = new SimpleMailMessage();
        mensagem.setFrom(remetente);
        mensagem.setTo(destinatario);
        mensagem.setSubject("Redefinição de senha — API de Consultas Médicas");
        mensagem.setText(montarCorpo(linkRedefinicao));

        try {
            mailSender.send(mensagem);
            log.info("Email de redefinição enviado para destinatário (mascarado): {}", mascarar(destinatario));
        } catch (MailException ex) {
            // Falha de SMTP não pode quebrar o fluxo do controller: o cliente sempre recebe 204.
            // Apenas log de erro para diagnóstico operacional.
            log.error("Falha ao enviar email de redefinição para {}: {}", mascarar(destinatario), ex.getMessage(), ex);
        }
    }

    private String montarCorpo(String linkRedefinicao) {
        return """
                Olá,

                Recebemos uma solicitação para redefinir a senha da sua conta.
                Para continuar, acesse o link abaixo (válido por %d minutos):

                %s

                Se você não solicitou essa redefinição, ignore este e-mail — sua senha permanecerá inalterada.

                Atenciosamente,
                API de Consultas Médicas
                """.formatted(AppConstants.PASSWORD_RESET_TOKEN_EXPIRACAO_MINUTOS, linkRedefinicao);
    }

    private String mascarar(String email) {
        if (email == null || email.isBlank()) {
            return "<vazio>";
        }
        int arroba = email.indexOf('@');
        if (arroba <= 1) {
            return "***" + email.substring(Math.max(arroba, 0));
        }
        return email.charAt(0) + "***" + email.substring(arroba);
    }

}
