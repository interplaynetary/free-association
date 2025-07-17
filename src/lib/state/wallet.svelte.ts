import { writable, derived, get } from 'svelte/store';
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
	balance: string | null;
	error: string | null;
}

// Store
export const walletState = writable<WalletState>({
	isConnected: false,
	isConnecting: false,
	selectedAccount: null,
	accounts: [],
	balance: null,
	error: null
});

// Derived stores
export const isWalletConnected = derived(walletState, ($state) => $state.isConnected);
export const selectedWalletAccount = derived(walletState, ($state) => $state.selectedAccount);
export const walletAccounts = derived(walletState, ($state) => $state.accounts);
export const walletBalance = derived(walletState, ($state) => $state.balance);

// Configuration
const APP_NAME = 'Free Association';

// Simple wallet connection simulation
export async function connectWallet(): Promise<void> {
	walletState.update(state => ({ ...state, isConnecting: true, error: null }));

	try {
		// Check if injectedWeb3 is available
		if (typeof window === 'undefined' || !window.injectedWeb3) {
			throw new Error('No wallet extensions found. Please install Polkadot.js extension.');
		}

		// Use dynamic import to load polkadot modules
		const { web3Enable, web3Accounts } = await import('@polkadot/extension-dapp');

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

		// Update state
		walletState.update(state => ({
			...state,
			isConnected: true,
			isConnecting: false,
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
			balance: '10.0000 DOT', // Mock balance
			error: null
		}));

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
		balance: null,
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
		selectedAccount: account,
		balance: '10.0000 DOT' // Mock balance
	}));
}

// Send transaction (simplified)
export async function sendTransaction(
	recipientAddress: string,
	amount: string
): Promise<string> {
	const state = get(walletState);
	
	if (!state.isConnected || !state.selectedAccount) {
		throw new Error('Wallet not connected');
	}

	try {
		// Simulate transaction
		await new Promise(resolve => setTimeout(resolve, 2000));

		// Mock transaction hash
		const mockHash = '0x' + Math.random().toString(16).substring(2, 66);
		
		toast.success(`Transaction sent! Hash: ${mockHash.substring(0, 8)}...`);
		
		return mockHash;
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

// Auto-connect if previously connected
export async function autoConnectWallet(): Promise<void> {
	// Check if user was previously connected
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