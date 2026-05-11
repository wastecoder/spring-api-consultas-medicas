# Testes de carga com k6

Scripts para exercitar os relatórios pesados da API e medir o ganho do cache
introduzido em §11 do `progresso.md`.

> Esta pasta **não** entra na imagem Docker (está no `.dockerignore`) e **não**
> roda no CI — é só para uso local.

## Pré-requisitos

- [k6](https://k6.io) instalado:
  - Windows: `winget install k6` (ou baixar binário em <https://k6.io/docs/get-started/installation>).
  - macOS: `brew install k6`.
  - Linux: ver instruções no site oficial.
- API rodando localmente em `http://localhost:8080` (perfil `dev` com seed
  ligado é o cenário mais útil — já existem ~40 consultas e ~20 médicos para
  os relatórios processarem):
  ```
  ./mvnw spring-boot:run
  ```
- Usuário admin disponível (o default `admin / 123456` criado pelo
  `SecurityConfig#criarUsuarioInicial` já serve).

## Scripts

### `smoke-relatorios.js`

Verificação leve: 1 VU, 20 iterações, batendo nos 4 relatórios pesados.
Serve para confirmar que o ambiente está de pé antes de tocar a carga.

```
k6 run loadtest/smoke-relatorios.js
```

Espera-se: status 200 em todas, `http_req_failed rate < 1%`,
`http_req_duration p(95) < 1000ms`.

### `load-relatorios.js`

Rampa até 20 VU em 2 minutos, escolhendo aleatoriamente entre 4 endpoints
pesados:

- `GET /relatorios/financeiro/faturamento-mensal`
- `GET /relatorios/financeiro/faturamento-por-medico`
- `GET /relatorios/medico/taxa-cancelamento`
- `GET /relatorios/produtividade/taxa-comparecimento`

```
k6 run loadtest/load-relatorios.js
```

Thresholds: `http_req_failed rate < 1%`, `http_req_duration p(95) < 500ms`.

## Variáveis de ambiente opcionais

| Variável    | Default                  | Uso                       |
|-------------|--------------------------|---------------------------|
| `BASE_URL`  | `http://localhost:8080`  | Apontar para outro host   |
| `USERNAME`  | `admin`                  | Trocar de usuário         |
| `SENHA`     | `123456`                 | Trocar de senha           |

Exemplo:

```
k6 run -e BASE_URL=http://localhost:9090 -e USERNAME=foo -e SENHA=bar loadtest/load-relatorios.js
```

## Comparar antes / depois do cache

1. Em um commit **sem** o cache (ou desligando `@EnableCaching` temporariamente
   no `CacheConfig`), suba a app e rode `k6 run loadtest/load-relatorios.js`.
   Anote `http_req_duration{p(95)}` e `iterations`.
2. Volte para o commit com o cache, suba a app, rode de novo, anote.

A expectativa é uma queda perceptível no p95 (cada endpoint só executa o JPQL
agregado uma vez a cada 60 s — o resto vem do Caffeine in-memory).
