CREATE TABLE paciente (
    id                 BIGSERIAL    PRIMARY KEY,
    nome               VARCHAR(100) NOT NULL,
    email              VARCHAR(50)  NOT NULL,
    telefone           VARCHAR(11),
    ativo              BOOLEAN      NOT NULL,
    cpf                VARCHAR(11)  NOT NULL,
    sexo               VARCHAR(9)   NOT NULL,
    data_nascimento    DATE         NOT NULL,
    usuario_id         BIGINT,
    created_by         VARCHAR(100),
    created_date       TIMESTAMP(6) WITHOUT TIME ZONE,
    last_modified_by   VARCHAR(100),
    last_modified_date TIMESTAMP(6) WITHOUT TIME ZONE,
    CONSTRAINT uk_paciente_email   UNIQUE (email),
    CONSTRAINT uk_paciente_cpf     UNIQUE (cpf),
    CONSTRAINT uk_paciente_usuario UNIQUE (usuario_id),
    CONSTRAINT fk_paciente_usuario FOREIGN KEY (usuario_id) REFERENCES usuario (id)
);
