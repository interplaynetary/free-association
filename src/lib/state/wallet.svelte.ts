import { writable, derived, get } from 'svelte/store';
import type { InjectedAccount, InjectedExtension } from '@polkadot/extension-inject/types';
import { web3Accounts, web3Enable, web3FromAddress } from '@polkadot/extension-dapp';
import { ApiPromise, WsProvider } from '@polkadot/api';
import { formatBalance } from '@polkadot/util';
import { toast } from 'svelte-french-toast';

// Types
export interface WalletAccount {
	address: string;
	name?: string;
	source: string;
	type?: string;
}

export interface WalletState {
	isConnected: boolean;
	isConnecting: boolean;
	selectedAccount: WalletAccount | null;
	accounts: WalletAccount[];
	extensions: InjectedExtension[];
	balance: string | null;
	api: ApiPromise | null;
	error: string | null;
}

// Store
export const walletState = writable<WalletState>({
	isConnected: false,
	isConnecting: false,
	selectedAccount: null,
	accounts: [],
	extensions: [],
	balance: null,
	api: null,
	error: null
});

// Derived stores
export const isWalletConnected = derived(walletState, ($state) => $state.isConnected);
export const selectedWalletAccount = derived(walletState, ($state) => $state.selectedAccount);
export const walletAccounts = derived(walletState, ($state) => $state.accounts);
export const walletBalance = derived(walletState, ($state) => $state.balance);

// Configuration
const POLKADOT_ENDPOINT = 'wss://rpc.polkadot.io';
const APP_NAME = 'Free Association';

// Initialize API connection
async function initializeApi(): Promise<ApiPromise> {
	const provider = new WsProvider(POLKADOT_ENDPOINT);
	const api = await ApiPromise.create({ provider });
	return api;
}

// Connect to wallet extensions
export async function connectWallet(): Promise<void> {
	walletState.update(state => ({ ...state, isConnecting: true, error: null }));

	try {
		// Enable web3 extensions
		const extensions = await web3Enable(APP_NAME);
		
		if (extensions.length === 0) {
			throw new Error('No wallet extensions found. Please install Polkadot.js extension.');
		}

		// Get all accounts
		const accounts = await web3Accounts();
		
		if (accounts.length === 0) {
			throw new Error('No accounts found. Please create an account in your wallet.');
		}

		// Initialize API
		const api = await initializeApi();

		// Update state
		walletState.update(state => ({
			...state,
			isConnected: true,
			isConnecting: false,
			extensions,
			accounts: accounts.map(account => ({
				address: account.address,
				name: account.meta.name,
				source: account.meta.source,
				type: account.type
			})),
			selectedAccount: accounts[0] ? {
				address: accounts[0].address,
				name: accounts[0].meta.name,
				source: accounts[0].meta.source,
				type: accounts[0].type
			} : null,
			api,
			error: null
		}));

		// Fetch balance for the first account
		if (accounts[0]) {
			await fetchBalance(accounts[0].address);
		}

		toast.success('Wallet connected successfully!');
	} catch (error) {
		const errorMessage = error instanceof Error ? error.message : 'Failed to connect wallet';
		walletState.update(state => ({
			...state,
			isConnecting: false,
			error: errorMessage
		}));
		toast.error(errorMessage);
		throw error;
	}
}

// Disconnect wallet
export function disconnectWallet(): void {
	walletState.update(state => ({
		...state,
		isConnected: false,
		selectedAccount: null,
		accounts: [],
		extensions: [],
		balance: null,
		api: null,
		error: null
	}));
	toast.success('Wallet disconnected');
}

// Select account
export async function selectAccount(address: string): Promise<void> {
	const state = get(walletState);
	const account = state.accounts.find(acc => acc.address === address);
	
	if (!account) {
		throw new Error('Account not found');
	}

	walletState.update(state => ({
		...state,
		selectedAccount: account
	}));

	// Fetch balance for selected account
	await fetchBalance(address);
}

// Fetch balance
async function fetchBalance(address: string): Promise<void> {
	try {
		const state = get(walletState);
		if (!state.api) {
			throw new Error('API not initialized');
		}

		const { data: balance } = await state.api.query.system.account(address);
		const formattedBalance = formatBalance(balance.free, { withUnit: 'DOT' });

		walletState.update(state => ({
			...state,
			balance: formattedBalance
		}));
	} catch (error) {
		console.error('Failed to fetch balance:', error);
		walletState.update(state => ({
			...state,
			balance: null
		}));
	}
}

// Send transaction
export async function sendTransaction(
	recipientAddress: string,
	amount: string
): Promise<string> {
	const state = get(walletState);
	
	if (!state.isConnected || !state.selectedAccount || !state.api) {
		throw new Error('Wallet not connected');
	}

	try {
		// Get injector for the selected account
		const injector = await web3FromAddress(state.selectedAccount.address);
		
		// Create transfer transaction
		const transfer = state.api.tx.balances.transfer(recipientAddress, amount);
		
		// Sign and send transaction
		const hash = await transfer.signAndSend(
			state.selectedAccount.address,
			{ signer: injector.signer }
		);

		toast.success(`Transaction sent! Hash: ${hash.toString()}`);
		
		// Refresh balance after transaction
		await fetchBalance(state.selectedAccount.address);
		
		return hash.toString();
	} catch (error) {
		const errorMessage = error instanceof Error ? error.message : 'Transaction failed';
		toast.error(errorMessage);
		throw error;
	}
}

// Check if wallet extensions are available
export function isExtensionAvailable(): boolean {
	return typeof window !== 'undefined' && !!window.injectedWeb3;
}

// Get available wallet extensions
export function getAvailableExtensions(): string[] {
	if (typeof window === 'undefined' || !window.injectedWeb3) {
		return [];
	}
	
	return Object.keys(window.injectedWeb3);
}

// Refresh wallet data
export async function refreshWalletData(): Promise<void> {
	const state = get(walletState);
	
	if (!state.isConnected || !state.selectedAccount) {
		return;
	}

	try {
		// Refresh balance
		await fetchBalance(state.selectedAccount.address);
		
		// Refresh accounts (in case new accounts were added)
		const accounts = await web3Accounts();
		walletState.update(state => ({
			...state,
			accounts: accounts.map(account => ({
				address: account.address,
				name: account.meta.name,
				source: account.meta.source,
				type: account.type
			}))
		}));
	} catch (error) {
		console.error('Failed to refresh wallet data:', error);
	}
}

// Auto-connect if previously connected
export async function autoConnectWallet(): Promise<void> {
	// Check if user was previously connected (could store in localStorage)
	const wasConnected = localStorage.getItem('wallet-connected') === 'true';
	
	if (wasConnected && isExtensionAvailable()) {
		try {
			await connectWallet();
		} catch (error) {
			console.log('Auto-connect failed:', error);
			// Clear the stored connection state
			localStorage.removeItem('wallet-connected');
		}
	}
}

// Store connection state
walletState.subscribe(state => {
	if (typeof window !== 'undefined') {
		localStorage.setItem('wallet-connected', state.isConnected.toString());
	}
});