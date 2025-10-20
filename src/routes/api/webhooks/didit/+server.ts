import type { RequestHandler } from '@sveltejs/kit';
import crypto from 'node:crypto';
import { env as privateEnv } from '$env/dynamic/private';

const WEBHOOK_SECRET = privateEnv.DIDIT_WEBHOOK_SECRET ?? process.env.DIDIT_WEBHOOK_SECRET ?? '';

function verifySignature(raw: string, signature: string | null, timestamp: string | null) {
  if (!WEBHOOK_SECRET || !signature || !timestamp) return false;
  const toSign = `${raw}_${timestamp}`;
  const hmac = crypto.createHmac('sha256', WEBHOOK_SECRET).update(toSign).digest('hex');
  return signature.split(',').some((piece) => piece.trim().endsWith(hmac));
}

export const POST: RequestHandler = async ({ request }) => {
  const raw = await request.text();
  const sig = request.headers.get('x-signature');
  const ts = request.headers.get('x-timestamp');

  if (!verifySignature(raw, sig, ts)) {
    return new Response('Invalid signature', { status: 401 });
  }

  // Reparse JSON
  let event: any;
  try {
    event = JSON.parse(raw);
  } catch {
    return new Response('Bad JSON', { status: 400 });
  }

  // Example: { sessionId, status: 'completed' | 'failed' | ..., externalUserId }
  // Hook: update internal state store or DB here if needed.
  // This project is client-first; we can later add persistence bridge if requested.

  return new Response('ok');
};
