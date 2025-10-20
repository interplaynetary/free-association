import { browser } from '$app/environment';
import { writable, type Writable } from 'svelte/store';
import { USE_HOLSTER_AUTH } from '$lib/config';
import { holster, holsterUser, holsterUserAlias, holsterUserPub } from '$lib/state/holster.svelte';
import { BankDetailsSchema, type BankDetails } from '$lib/schema';

type BankEncryptedRecord = {
  ciphertext: any;
  alg: 'SEA-v1';
  ownerEpub: string;
  updatedAt: number;
};

export const ownBankDetails: Writable<BankDetails | null> = writable(null);
export const isSavingBankDetails: Writable<boolean> = writable(false);
export const bankError: Writable<string> = writable('');

// Requests incoming to current user
export type BankAccessRequest = {
  requesterPub: string;
  requesterAlias?: string;
  requesterEpub?: string;
  message?: string;
  status: 'pending' | 'approved' | 'rejected';
  ts: number;
};

export const incomingBankRequests: Writable<Record<string, BankAccessRequest>> = writable({});

// Grants that others have approved for us
export type BankGrantRecord = {
  ownerPub: string;
  ownerEpub: string;
  ciphertext: any;
  ts: number;
};

export const grantedBankDetails: Writable<Record<string, BankDetails>> = writable({});

async function getSEA() {
  const mod = await import('gun/sea');
  return mod.default;
}

function now() { return Date.now(); }

export async function saveOwnBankDetails(input: Omit<BankDetails, 'updatedAt'>) {
  if (!USE_HOLSTER_AUTH) throw new Error('Holster must be enabled to save encrypted bank details');
  if (!browser) return;

  const validated = BankDetailsSchema.parse({ ...input, updatedAt: now() });
  const SEA = await getSEA();

  // Get current user's encryption public key
  const ownerEpub: string = (holsterUser as any)?.is?.epub || '';
  if (!ownerEpub) throw new Error('Missing encryption public key (epub) for current user');

  // Encrypt for self with own keypair
  const pair = (holsterUser as any).pair?.() || (holsterUser as any)?._?.sea;
  if (!pair) throw new Error('Missing keypair for encryption');

  isSavingBankDetails.set(true);
  bankError.set('');

  try {
    const ciphertext = await SEA.encrypt(validated, pair);
    const rec: BankEncryptedRecord = { ciphertext, alg: 'SEA-v1', ownerEpub, updatedAt: validated.updatedAt };
    (holsterUser as any)
      .get('private')
      .next('bank')
      .put(rec, (err: any) => {
        if (err) {
          bankError.set('Failed to save');
          console.error('[BANK] save error', err);
        } else {
          ownBankDetails.set(validated);
        }
        isSavingBankDetails.set(false);
      });
  } catch (e: any) {
    isSavingBankDetails.set(false);
    bankError.set(e?.message || 'Encryption failed');
    throw e;
  }
}

export async function loadOwnBankDetails(): Promise<void> {
  if (!USE_HOLSTER_AUTH || !browser) return;
  const SEA = await getSEA();
  const pair = (holsterUser as any).pair?.() || (holsterUser as any)?._?.sea;
  const chain: any = (holsterUser as any).get('private').next('bank');
  const cb = async (rec: BankEncryptedRecord) => {
    try {
      if (!rec || !rec.ciphertext) return;
      const dec = await SEA.decrypt(rec.ciphertext, pair);
      const v = BankDetailsSchema.safeParse(dec);
      if (v.success) ownBankDetails.set(v.data);
    } catch (e) {
      console.warn('[BANK] decrypt self failed', e);
    }
  };
  chain.once ? chain.once(cb) : chain.on(cb, true);
}

// Request access to someone else's bank details (public metadata only)
export async function requestBankAccess(targetPub: string, message?: string) {
  if (!USE_HOLSTER_AUTH || !browser) throw new Error('Holster must be enabled');
  const requesterPub: string = (await new Promise<string>((resolve) => {
    let current = '';
    const unsub = holsterUserPub.subscribe((v) => { current = v; });
    setTimeout(() => { unsub(); resolve(current); }, 0);
  })) || '';
  const requesterAlias: string = (await new Promise<string>((resolve) => {
    let current = '';
    const unsub = holsterUserAlias.subscribe((v) => { current = v; });
    setTimeout(() => { unsub(); resolve(current); }, 0);
  })) || '';
  const requesterEpub: string = (holsterUser as any)?.is?.epub;

  const payload: BankAccessRequest = {
    requesterPub,
    requesterAlias,
    requesterEpub,
    message,
    status: 'pending',
    ts: now()
  };
  holster.get('bank-access-requests').next(targetPub).next(requesterPub).put(payload, (err: any) => {
    if (err) console.error('[BANK] request put error', err);
  });
}

// Subscribe to incoming requests for current user
export function subscribeIncomingBankRequests() {
  if (!USE_HOLSTER_AUTH || !browser) return;
  const getPubSync = () => (holsterUser as any)?.is?.pub || '';
  const targetPub = getPubSync();
  if (!targetPub) return;

  holster.get('bank-access-requests').next(targetPub).on((val: any, key: string) => {
    if (!val || typeof val !== 'object') return;
    incomingBankRequests.update((curr) => ({ ...curr, [key]: val as BankAccessRequest }));
  });
}

// Approve a request: encrypt details for the requester using ECDH shared secret and publish grant
export async function approveBankAccess(requesterPub: string, requesterEpub?: string): Promise<void> {
  if (!USE_HOLSTER_AUTH || !browser) throw new Error('Holster must be enabled');
  const SEA = await getSEA();
  const ownerPair = (holsterUser as any).pair?.() || (holsterUser as any)?._?.sea;
  if (!ownerPair) throw new Error('Missing keypair');
  const ownerEpub: string = (holsterUser as any)?.is?.epub || '';
  if (!requesterEpub) {
    // Try to read from request object
    const reqHolder: any = await new Promise((resolve) => {
      holster.get('bank-access-requests').next((holsterUser as any)?.is?.pub).next(requesterPub).once((v: any) => resolve(v));
    });
    requesterEpub = reqHolder?.requesterEpub;
  }
  if (!requesterEpub) throw new Error('Missing requesterEpub');

  // Load our encrypted record and decrypt
  const encRec: BankEncryptedRecord = await new Promise((resolve) => {
    (holsterUser as any).get('private').next('bank').once((v: any) => resolve(v));
  });
  if (!encRec?.ciphertext) throw new Error('No saved bank details');
  const selfDec = await SEA.decrypt(encRec.ciphertext, ownerPair);
  const v = BankDetailsSchema.safeParse(selfDec);
  if (!v.success) throw new Error('Saved bank details invalid');

  // Derive shared secret and encrypt for requester
  const shared = await SEA.secret(requesterEpub, ownerPair);
  const ciphertext = await SEA.encrypt(v.data, shared);
  const grant: BankGrantRecord = { ownerPub: (holsterUser as any).is.pub, ownerEpub, ciphertext, ts: now() };
  holster.get('bank-grants').next((holsterUser as any).is.pub).next(requesterPub).put(grant, (err: any) => {
    if (err) console.error('[BANK] grant put error', err);
  });

  // Mark request approved
  holster.get('bank-access-requests').next((holsterUser as any).is.pub).next(requesterPub).put({ status: 'approved', ts: now() }, (err: any) => {
    if (err) console.error('[BANK] request status update error', err);
  });
}

export async function rejectBankAccess(requesterPub: string): Promise<void> {
  holster.get('bank-access-requests').next((holsterUser as any).is.pub).next(requesterPub).put({ status: 'rejected', ts: now() }, (err: any) => {
    if (err) console.error('[BANK] request reject error', err);
  });
}

// For requester: load and decrypt a grant from an owner
export async function loadGrantedFrom(ownerPub: string): Promise<BankDetails | null> {
  if (!USE_HOLSTER_AUTH || !browser) return null;
  const SEA = await getSEA();
  const myPair = (holsterUser as any).pair?.() || (holsterUser as any)?._?.sea;
  const grant: BankGrantRecord = await new Promise((resolve) => {
    holster.get('bank-grants').next(ownerPub).next((holsterUser as any).is.pub).once((v: any) => resolve(v));
  });
  if (!grant?.ciphertext || !grant?.ownerEpub) return null;
  const shared = await SEA.secret(grant.ownerEpub, myPair);
  const dec = await SEA.decrypt(grant.ciphertext, shared);
  const v = BankDetailsSchema.safeParse(dec);
  if (v.success) return v.data;
  return null;
}

// Initialize module on load
if (browser && USE_HOLSTER_AUTH) {
  loadOwnBankDetails();
  subscribeIncomingBankRequests();
}

