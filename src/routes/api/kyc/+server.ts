import type { RequestHandler } from '@sveltejs/kit';
import { env as privateEnv } from '$env/dynamic/private';

// Read from SvelteKit dynamic private env (works in dev/adapter) with process.env fallback
const BASE = privateEnv.DIDIT_BASE_URL ?? process.env.DIDIT_BASE_URL ?? 'https://verification.didit.me';
const API_KEY = privateEnv.DIDIT_API_KEY ?? process.env.DIDIT_API_KEY ?? '';
const WORKFLOW_ID = privateEnv.DIDIT_WORKFLOW_ID ?? process.env.DIDIT_WORKFLOW_ID ?? '';

export const POST: RequestHandler = async ({ request, fetch }) => {
  try {
    if (!API_KEY || !WORKFLOW_ID) {
      return new Response(
        JSON.stringify({ error: 'Didit not configured', missing: { apiKey: !API_KEY, workflowId: !WORKFLOW_ID } }),
        { status: 500 }
      );
    }

    const { externalUserId, metadata } = await request.json();
    const res = await fetch(`${BASE}/v2/session/`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'X-Api-Key': API_KEY
      },
      body: JSON.stringify({
        workflow_id: WORKFLOW_ID,
        external_user_id: externalUserId,
        metadata
      })
    });

    if (!res.ok) {
      const text = await res.text();
      return new Response(JSON.stringify({ error: text || 'Create session failed' }), { status: 500 });
    }

    const data = await res.json();
    return new Response(
      JSON.stringify({ url: data.url, sessionId: data.session_id }),
      { headers: { 'Content-Type': 'application/json' } }
    );
  } catch (e: any) {
    return new Response(JSON.stringify({ error: e?.message ?? 'unknown' }), { status: 500 });
  }
};
