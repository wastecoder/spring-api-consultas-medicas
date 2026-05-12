CREATE TABLE refresh_token (
    id         BIGSERIAL    PRIMARY KEY,
    token      VARCHAR(36)  NOT NULL,
    usuario_id BIGINT       NOT NULL,
    expira_em  TIMESTAMP(6) WITHOUT TIME ZONE NOT NULL,
    criado_em  TIMESTAMP(6) WITHOUT TIME ZONE NOT NULL,
    CONSTRAINT fk_refresh_token_usuario FOREIGN KEY (usuario_id) REFERENCES usuario (id)
);

CREATE UNIQUE INDEX uk_refresh_token_token ON refresh_token (token);
