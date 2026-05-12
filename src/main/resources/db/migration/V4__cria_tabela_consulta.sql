CREATE TABLE consulta (
    id                   BIGSERIAL      PRIMARY KEY,
    data_atendimento     DATE           NOT NULL,
    horario_atendimento  TIME           NOT NULL,
    duracao_em_minutos   BIGINT         NOT NULL,
    data_agendamento     TIMESTAMP(6)   WITHOUT TIME ZONE NOT NULL,
    preco                NUMERIC(10, 2) NOT NULL,
    motivo               VARCHAR(200)   NOT NULL,
    status               VARCHAR(9)     NOT NULL,
    medico_id            BIGINT         NOT NULL,
    paciente_id          BIGINT         NOT NULL,
    created_by           VARCHAR(100),
    created_date         TIMESTAMP(6)   WITHOUT TIME ZONE,
    last_modified_by     VARCHAR(100),
    last_modified_date   TIMESTAMP(6)   WITHOUT TIME ZONE,
    CONSTRAINT fk_consulta_medico   FOREIGN KEY (medico_id)   REFERENCES medico (id),
    CONSTRAINT fk_consulta_paciente FOREIGN KEY (paciente_id) REFERENCES paciente (id)
);
