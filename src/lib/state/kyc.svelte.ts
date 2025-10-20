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

// Load KYC from Holster SEA user space (or Gun) on module load
function loadKycFromRemote() {
  if (!browser) return;
  try {
    if (USE_HOLSTER_AUTH) {
      const chain: any = (holsterUser as any).get('profile').next('kyc');
      const cb = (k: any) => {
        const v = KycRecordSchema.safeParse(k);
        if (v.success) {
          kycRecord.set(v.data);
          kycStatus.set(v.data.status);
        }
      };
      chain.once ? chain.once(cb) : chain.on(cb, true);
    } else {
      gunUser.get('profile').get('kyc').once((k: any) => {
        const v = KycRecordSchema.safeParse(k);
        if (v.success) {
          kycRecord.set(v.data);
          kycStatus.set(v.data.status);
        }
      });
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
  } catch (e) {
    console.error('[KYC] persist error', e);
    throw e;
  }
}

function shouldAutoApprove(): boolean {
  if (!browser) return false;
  try {
    return (import.meta as any).env?.VITE_KYC_AUTO_APPROVE_SELF === 'true';
  } catch { return false; }
}

// Load initial state from remote
loadKycFromRemote();
