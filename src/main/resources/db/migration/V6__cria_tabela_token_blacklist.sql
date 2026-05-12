CREATE TABLE token_blacklist (
    id          BIGSERIAL    PRIMARY KEY,
    jti         VARCHAR(36)  NOT NULL,
    expira_em   TIMESTAMP(6) WITHOUT TIME ZONE NOT NULL,
    revogado_em TIMESTAMP(6) WITHOUT TIME ZONE NOT NULL
);

CREATE UNIQUE INDEX uk_token_blacklist_jti ON token_blacklist (jti);
