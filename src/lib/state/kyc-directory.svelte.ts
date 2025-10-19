import { writable, derived } from 'svelte/store';
import { USE_HOLSTER_AUTH } from '$lib/config';
import { gun, usersList as gunUsersList } from '$lib/state/gun.svelte';
import { holsterUser, holsterUsersList } from '$lib/state/holster.svelte';
import type { KycStatus } from '$lib/schema';

export type CommunityUser = {
  pubkey: string;
  name: string;
  wallet?: { address: string; chainId?: number } | null;
  lastSeen?: number;
  kycStatus: KycStatus | 'unverified' | 'pending' | 'rejected';
};

export const communityUsers = writable<Record<string, CommunityUser>>({});
let cleanupFns: Array<() => void> = [];

export function startKycDirectory() {
  stopKycDirectory();

  if (USE_HOLSTER_AUTH) {
    // Subscribe to players list on Holster
    try {
      const unsub = (holsterUsersList as any).map().on((val: any, key: string) => {
        if (!key || !val) return;
        const alias = val.alias || key.slice(0, 8) + '…';
        upsertCommunity({ pubkey: key, name: alias, lastSeen: val.lastSeen, kycStatus: 'unverified' });
        readHolsterPublicProfile(key, alias, val.lastSeen);
      });
      cleanupFns.push(() => (holsterUsersList as any).off(unsub));
    } catch (e) {
      console.warn('[KYC-DIR] Holster users map error', e);
    }
  } else {
    // Subscribe to players list on Gun
    const unsub = (gunUsersList as any).map().on((val: any, key: string) => {
      if (!key || !val) return;
      const alias = val.alias || key.slice(0, 8) + '…';
      upsertCommunity({ pubkey: key, name: alias, lastSeen: val.lastSeen, kycStatus: 'unverified' });
      readGunPublicProfile(key, alias, val.lastSeen);
    });
    cleanupFns.push(() => (gunUsersList as any).off(unsub));
  }
}

export function stopKycDirectory() {
  cleanupFns.forEach((fn) => {
    try { fn(); } catch {}
  });
  cleanupFns = [];
  // Keep current cache; caller can clear if desired
}

function upsertCommunity(user: Partial<CommunityUser> & { pubkey: string }) {
  communityUsers.update((curr) => {
    const prev = curr[user.pubkey] || { pubkey: user.pubkey, name: user.pubkey.slice(0, 8) + '…', kycStatus: 'unverified' } as CommunityUser;
    return { ...curr, [user.pubkey]: { ...prev, ...user } };
  });
}

function readGunPublicProfile(pubkey: string, alias: string, lastSeen?: number) {
  // KYC
  gun.user(pubkey).get('profile').get('kyc').once((kyc: any) => {
    const status: KycStatus | 'unverified' | 'pending' | 'rejected' = kyc?.status || 'unverified';
    upsertCommunity({ pubkey, name: alias, lastSeen, kycStatus: status });
  });
  // Wallet (independent of KYC)
  gun.user(pubkey).get('profile').get('wallets').get('evm').once((w: any) => {
    const wallet = w && w.address ? { address: w.address as string, chainId: w.chainId as number | undefined } : null;
    if (wallet) upsertCommunity({ pubkey, wallet });
  });
}

function readHolsterPublicProfile(pubkey: string, alias: string, lastSeen?: number) {
  // KYC
  const kycCb = (kyc: any) => {
    const status: KycStatus | 'unverified' | 'pending' | 'rejected' = kyc?.status || 'unverified';
    upsertCommunity({ pubkey, name: alias, lastSeen, kycStatus: status });
  };
  let chain = (holsterUser as any).get(pubkey).next('profile').next('kyc');
  chain.once ? chain.once(kycCb) : chain.on(kycCb, true);

  // Wallet (independent of KYC)
  const walletCb = (w: any) => {
    const wallet = w && w.address ? { address: w.address as string, chainId: w.chainId as number | undefined } : null;
    if (wallet) upsertCommunity({ pubkey, wallet });
  };
  let wChain = (holsterUser as any).get(pubkey).next('profile').next('wallets').next('evm');
  wChain.once ? wChain.once(walletCb) : wChain.on(walletCb, true);
}

export const verifiedList = derived(communityUsers, ($c) => Object.values($c).filter(u => u.kycStatus === 'verified'));
export const notVerifiedList = derived(communityUsers, ($c) => Object.values($c).filter(u => u.kycStatus !== 'verified'));
