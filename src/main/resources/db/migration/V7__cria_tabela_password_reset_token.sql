CREATE TABLE password_reset_token (
    id         BIGSERIAL    PRIMARY KEY,
    token      VARCHAR(36)  NOT NULL,
    usuario_id BIGINT       NOT NULL,
    criado_em  TIMESTAMP(6) WITHOUT TIME ZONE NOT NULL,
    expira_em  TIMESTAMP(6) WITHOUT TIME ZONE NOT NULL,
    usado_em   TIMESTAMP(6) WITHOUT TIME ZONE,
    CONSTRAINT fk_password_reset_token_usuario FOREIGN KEY (usuario_id) REFERENCES usuario (id)
);

CREATE UNIQUE INDEX uk_password_reset_token_token ON password_reset_token (token);
