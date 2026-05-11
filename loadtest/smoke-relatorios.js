// Smoke test: 1 VU, poucas iteracoes, valida que cada endpoint pesado responde 200.
// Roda em ~30s. Serve pra confirmar que o ambiente esta de pe antes de partir
// pra carga de verdade.
//
// Como rodar (app no perfil dev, em localhost:8080):
//   k6 run loadtest/smoke-relatorios.js

import http from 'k6/http';
import { check, sleep } from 'k6';
import { login, authHeaders } from './lib/auth.js';

const BASE_URL = __ENV.BASE_URL || 'http://localhost:8080';
const USERNAME = __ENV.USERNAME || 'admin';
const SENHA = __ENV.SENHA || '123456';

export const options = {
  vus: 1,
  iterations: 20,
  thresholds: {
    http_req_failed: ['rate<0.01'],
    http_req_duration: ['p(95)<1000'],
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
  const url = `${BASE_URL}${ENDPOINTS[__ITER % ENDPOINTS.length]}`;
  const res = http.get(url, headers);

  check(res, {
    'status 200': (r) => r.status === 200,
    'payload nao vazio': (r) => r.body && r.body.length > 0,
  });

  sleep(0.2);
}
