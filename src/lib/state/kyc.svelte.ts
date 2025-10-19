import { browser } from '$app/environment';
import { writable } from 'svelte/store';
import { USE_HOLSTER_AUTH } from '$lib/config';
import {
  KycRecordSchema,
  KycSelfAttestationSchema,
  KycStatusSchema,
  type KycRecord,
  type KycStatus,
  type KycSelfAttestation
} from '$lib/schema';
import { user as gunUser } from '$lib/state/gun.svelte';
import { holsterUser } from '$lib/state/holster.svelte';

export const kycStatus = writable<KycStatus>('unverified');
export const kycRecord = writable<KycRecord | null>(null);

export function loadKycFromLocal() {
  if (!browser) return;
  try {
    const raw = localStorage.getItem('profile:kyc');
    if (!raw) return;
    const parsed = JSON.parse(raw);
    const v = KycRecordSchema.safeParse(parsed);
    if (v.success) {
      kycRecord.set(v.data);
      kycStatus.set(v.data.status);
    }
  } catch {}
}

export async function submitSelfKyc(data: Omit<KycSelfAttestation, 'method' | 'submittedAt'>) {
  const payload: KycSelfAttestation = {
    method: 'self',
    submittedAt: Date.now(),
    ...data
  };

  const rec: KycRecord = {
    status: shouldAutoApprove() ? 'verified' : 'pending',
    provider: 'self',
    data: payload,
    updatedAt: Date.now()
  };

  await persistKyc(rec);
  kycRecord.set(rec);
  kycStatus.set(rec.status);
}

export async function setKycStatus(status: KycStatus) {
  const current = getCurrentRecord();
  const next: KycRecord = {
    status,
    provider: current?.provider || 'self',
    data: current?.data,
    updatedAt: Date.now()
  };
  await persistKyc(next);
  kycRecord.set(next);
  kycStatus.set(next.status);
}

function getCurrentRecord(): KycRecord | null {
  let val: KycRecord | null = null;
  kycRecord.update((curr) => {
    val = curr;
    return curr;
  });
  return val;
}

export async function persistKyc(rec: KycRecord) {
  const validated = KycRecordSchema.parse(rec);
  try {
    if (USE_HOLSTER_AUTH) {
      holsterUser.get('profile').next('kyc').put(validated, (err: any) => {
        if (err) console.error('[HOLSTER KYC] put error', err);
      });
    } else {
      gunUser.get('profile').get('kyc').put(validated);
    }
    if (browser) localStorage.setItem('profile:kyc', JSON.stringify(validated));
  } catch (e) {
    console.error('[KYC] persist error', e);
    throw e;
  }
}

function shouldAutoApprove(): boolean {
  if (!browser) return false;
  try {
    return (import.meta as any).env?.VITE_KYC_AUTO_APPROVE_SELF === 'true' ||
      localStorage.getItem('KYC_AUTO_APPROVE_SELF') === 'true';
  } catch { return false; }
}

// Load initial state
loadKycFromLocal();

