CREATE TABLE usuario (
    id                 BIGSERIAL    PRIMARY KEY,
    username           VARCHAR(30)  NOT NULL,
    email              VARCHAR(100) NOT NULL,
    senha              VARCHAR(255) NOT NULL,
    funcao             VARCHAR(20)  NOT NULL,
    ativo              BOOLEAN      NOT NULL,
    created_by         VARCHAR(100),
    created_date       TIMESTAMP(6) WITHOUT TIME ZONE,
    last_modified_by   VARCHAR(100),
    last_modified_date TIMESTAMP(6) WITHOUT TIME ZONE,
    CONSTRAINT uk_usuario_username UNIQUE (username),
    CONSTRAINT uk_usuario_email    UNIQUE (email)
);
