import { browser } from '$app/environment';
import { writable } from 'svelte/store';
import { USE_HOLSTER_AUTH } from '$lib/config';
import { WalletEvmSchema, type WalletEvm } from '$lib/schema';

// Holster/Gun auth modules for persistence
import { user as gunUser } from '$lib/state/gun.svelte';
import { holsterUser } from '$lib/state/holster.svelte';

// Simple reactive state for EVM wallet
export const evmAddress = writable<string>('');
export const evmChainId = writable<number | null>(null);
export const isWalletConnected = writable<boolean>(false);

function setWalletState(addr: string, chainId: number | null) {
  evmAddress.set(addr);
  evmChainId.set(chainId);
  isWalletConnected.set(!!addr);
}

export async function connectEvmWallet(): Promise<void> {
  if (!browser) return;
  const eth = (window as any).ethereum;
  if (!eth) {
    throw new Error('No EVM wallet found. Install MetaMask or a compatible wallet.');
  }
  const accounts: string[] = await eth.request({ method: 'eth_requestAccounts' });
  const addr = (accounts && accounts[0]) ? accounts[0] : '';
  const chainIdHex: string = await eth.request({ method: 'eth_chainId' });
  const chainId = parseInt(chainIdHex, 16);

  if (!addr) {
    setWalletState('', null);
    return;
  }

  // Validate address shape
  const parsed = WalletEvmSchema.safeParse({ address: addr, chainId, connectedAt: Date.now() });
  if (!parsed.success) {
    throw new Error('Invalid wallet address received');
  }

  setWalletState(addr, chainId);
  // Persist for the current auth system if available
  persistEvmWallet(parsed.data).catch((e) => console.warn('[WALLET] Persist failed:', e));

  // Subscribe to account and chain changes
  eth.removeListener?.('accountsChanged', onAccountsChanged);
  eth.removeListener?.('chainChanged', onChainChanged);
  eth.on?.('accountsChanged', onAccountsChanged);
  eth.on?.('chainChanged', onChainChanged);
}

export async function disconnectEvmWallet(): Promise<void> {
  setWalletState('', null);
}

function onAccountsChanged(accounts: string[]) {
  const addr = accounts && accounts[0] ? accounts[0] : '';
  setWalletState(addr, null);
}

function onChainChanged(chainIdHex: string) {
  const chainId = parseInt(chainIdHex, 16);
  evmChainId.set(isNaN(chainId) ? null : chainId);
}

export async function persistEvmWallet(wallet: WalletEvm): Promise<void> {
  try {
    if (USE_HOLSTER_AUTH) {
      // Holster path: profile -> wallets -> evm
      holsterUser
        .get('profile')
        .next('wallets')
        .next('evm')
        .put(wallet, (err: any) => {
          if (err) console.error('[HOLSTER WALLET] put error', err);
        });
    } else {
      // Gun path: profile -> wallets -> evm
      gunUser
        .get('profile')
        .get('wallets')
        .get('evm')
        .put(wallet);
    }
    // Also mirror in localStorage for quick boot
    if (browser) localStorage.setItem('wallet:evm', JSON.stringify(wallet));
  } catch (e) {
    console.error('[WALLET] persist error', e);
  }
}

// Try restore from localStorage on module load
if (browser) {
  try {
    const raw = localStorage.getItem('wallet:evm');
    if (raw) {
      const parsed = JSON.parse(raw);
      const v = WalletEvmSchema.safeParse(parsed);
      if (v.success) setWalletState(v.data.address, v.data.chainId ?? null);
    }
  } catch {}
}

