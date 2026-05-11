// Helper de autenticacao para os scripts k6.
// Faz POST /auth/login e devolve o accessToken.

import http from 'k6/http';
import { check } from 'k6';

export function login(baseUrl, username, senha) {
  const res = http.post(
    `${baseUrl}/auth/login`,
    JSON.stringify({ username, senha }),
    { headers: { 'Content-Type': 'application/json' } }
  );

  check(res, {
    'login retornou 200': (r) => r.status === 200,
    'resposta tem accessToken': (r) => !!r.json('accessToken'),
  });

  if (res.status !== 200) {
    throw new Error(`Login falhou: status=${res.status}, body=${res.body}`);
  }

  return res.json('accessToken');
}

export function authHeaders(token) {
  return {
    headers: {
      Authorization: `Bearer ${token}`,
      'Content-Type': 'application/json',
    },
  };
}
