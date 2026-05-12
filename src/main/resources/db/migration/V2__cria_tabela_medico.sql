CREATE TABLE medico (
    id                 BIGSERIAL    PRIMARY KEY,
    nome               VARCHAR(100) NOT NULL,
    email              VARCHAR(50)  NOT NULL,
    telefone           VARCHAR(11),
    ativo              BOOLEAN      NOT NULL,
    crm_sigla          VARCHAR(2)   NOT NULL,
    crm_digitos        VARCHAR(6)   NOT NULL,
    especialidade      VARCHAR(50)  NOT NULL,
    usuario_id         BIGINT,
    created_by         VARCHAR(100),
    created_date       TIMESTAMP(6) WITHOUT TIME ZONE,
    last_modified_by   VARCHAR(100),
    last_modified_date TIMESTAMP(6) WITHOUT TIME ZONE,
    CONSTRAINT uk_medico_email   UNIQUE (email),
    CONSTRAINT uk_medico_crm     UNIQUE (crm_sigla, crm_digitos),
    CONSTRAINT uk_medico_usuario UNIQUE (usuario_id),
    CONSTRAINT fk_medico_usuario FOREIGN KEY (usuario_id) REFERENCES usuario (id)
);
