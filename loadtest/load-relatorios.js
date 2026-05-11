// Load test: rampa ate ~20 VU em 2 min, batendo nos 4 relatorios pesados que
// receberam @Cacheable. Serve para comparar p95 antes e depois do cache estar
// ligado.
//
// Como rodar (app no perfil dev, em localhost:8080):
//   k6 run loadtest/load-relatorios.js
//
// Para comparar antes/depois do cache:
//   1. Checkout no commit anterior (ou desligar @EnableCaching temporariamente),
//      subir a app, rodar este script, anotar p95 e iteracoes.
//   2. Voltar pro commit com cache, subir a app, rodar de novo, anotar.

import http from 'k6/http';
import { check } from 'k6';
import { login, authHeaders } from './lib/auth.js';

const BASE_URL = __ENV.BASE_URL || 'http://localhost:8080';
const USERNAME = __ENV.USERNAME || 'admin';
const SENHA = __ENV.SENHA || '123456';

export const options = {
  stages: [
    { duration: '30s', target: 10 },
    { duration: '1m',  target: 20 },
    { duration: '30s', target: 0 },
  ],
  thresholds: {
    http_req_failed: ['rate<0.01'],
    http_req_duration: ['p(95)<500'],
  },
};

export function setup() {
  const token = login(BASE_URL, USERNAME, SENHA);
  return { token };
}

const ENDPOINTS = [
  '/relatorios/financeiro/faturamento-mensal',
  '/relatorios/financeiro/faturamento-por-medico',
  '/relatorios/medico/taxa-cancelamento',
  '/relatorios/produtividade/taxa-comparecimento',
];

export default function (data) {
  const headers = authHeaders(data.token);
  const url = `${BASE_URL}${ENDPOINTS[Math.floor(Math.random() * ENDPOINTS.length)]}`;
  const res = http.get(url, headers);

  check(res, {
    'status 200': (r) => r.status === 200,
    'payload nao vazio': (r) => r.body && r.body.length > 0,
  });
}
