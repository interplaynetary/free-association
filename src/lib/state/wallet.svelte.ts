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
	api: any | null;
}

// Store
export const walletState = writable<WalletState>({
	isConnected: false,
	isConnecting: false,
	selectedAccount: null,
	accounts: [],
	balance: null,
	error: null,
	api: null
});

// Derived stores
export const isWalletConnected = derived(walletState, ($state) => $state.isConnected);
export const selectedWalletAccount = derived(walletState, ($state) => $state.selectedAccount);
export const walletAccounts = derived(walletState, ($state) => $state.accounts);
export const walletBalance = derived(walletState, ($state) => $state.balance);

// Configuration
const APP_NAME = 'Free Association';
const WESTEND_ENDPOINT = 'wss://westend-rpc.polkadot.io';

// Simple wallet connection simulation
export async function connectWallet(): Promise<void> {
	walletState.update(state => ({ ...state, isConnecting: true, error: null }));

	try {
		// Check if injectedWeb3 is available
		if (typeof window === 'undefined' || !window.injectedWeb3) {
			throw new Error('No wallet extensions found. Please install Polkadot.js extension.');
		}

		// Use dynamic import to load polkadot modules
		const [{ web3Enable, web3Accounts }, { ApiPromise, WsProvider }] = await Promise.all([
			import('@polkadot/extension-dapp'),
			import('@polkadot/api')
		]);

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

		// Connect to Westend
		const provider = new WsProvider(WESTEND_ENDPOINT);
		const api = await ApiPromise.create({ provider });
		await api.isReady;

		// Fetch real balance for first account
		let balance = null;
		if (accounts[0]) {
			try {
				const { data: { free } } = await api.query.system.account(accounts[0].address);
				const decimals = api.registry.chainDecimals[0] || 12;
				const symbol = api.registry.chainTokens[0] || 'WND';
				const balanceInWND = free.toNumber() / Math.pow(10, decimals);
				balance = `${balanceInWND.toFixed(4)} ${symbol}`;
			} catch (err) {
				console.warn('Failed to fetch balance:', err);
				balance = '0.0000 WND';
			}
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
			balance,
			api,
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
	walletState.update(state => {
		// Disconnect API if it exists
		if (state.api) {
			try {
				state.api.disconnect();
			} catch (err) {
				console.warn('Error disconnecting API:', err);
			}
		}
		
		return {
			...state,
			isConnected: false,
			selectedAccount: null,
			accounts: [],
			balance: null,
			api: null,
			error: null
		};
	});
	toast.success('Wallet disconnected');
}

// Select account
export async function selectAccount(address: string): Promise<void> {
	const state = get(walletState);
	const account = state.accounts.find(acc => acc.address === address);
	
	if (!account) {
		throw new Error('Account not found');
	}

	// Fetch real balance for selected account
	let balance = null;
	if (state.api) {
		try {
			const { data: { free } } = await state.api.query.system.account(address);
			const decimals = state.api.registry.chainDecimals[0] || 12;
			const symbol = state.api.registry.chainTokens[0] || 'WND';
			const balanceInWND = free.toNumber() / Math.pow(10, decimals);
			balance = `${balanceInWND.toFixed(4)} ${symbol}`;
		} catch (err) {
			console.warn('Failed to fetch balance:', err);
			balance = '0.0000 WND';
		}
	}

	walletState.update(state => ({
		...state,
		selectedAccount: account,
		balance
	}));
}

// Send transaction
export async function sendTransaction(
	recipientAddress: string,
	amount: string
): Promise<string> {
	const state = get(walletState);
	
	if (!state.isConnected || !state.selectedAccount || !state.api) {
		throw new Error('Wallet not connected or API not available');
	}

	try {
		// Import web3FromSource for signing
		const { web3FromSource } = await import('@polkadot/extension-dapp');
		
		// Get injector for signing
		const injector = await web3FromSource(state.selectedAccount.source);
		
		// Create transfer transaction
		const transfer = state.api.tx.balances.transfer(recipientAddress, amount);
		
		// Sign and send transaction
		const txHash = await transfer.signAndSend(
			state.selectedAccount.address,
			{ signer: injector.signer },
			(result: any) => {
				if (result.status.isInBlock) {
					console.log('Transaction in block:', result.status.asInBlock.toString());
				} else if (result.status.isFinalized) {
					console.log('Transaction finalized:', result.status.asFinalized.toString());
				}
			}
		);
		
		toast.success(`Transaction sent! Hash: ${txHash.toString().substring(0, 8)}...`);
		
		// Refresh balance after transaction
		setTimeout(() => {
			if (state.selectedAccount) {
				selectAccount(state.selectedAccount.address);
			}
		}, 3000);
		
		return txHash.toString();
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

// Get token decimals from chain
export async function getTokenDecimals(): Promise<number> {
	const state = get(walletState);
	if (!state.api) {
		throw new Error('API not available');
	}
	return state.api.registry.chainDecimals[0] || 12;
}

// Get token symbol from chain
export async function getTokenSymbol(): Promise<string> {
	const state = get(walletState);
	if (!state.api) {
		throw new Error('API not available');
	}
	return state.api.registry.chainTokens[0] || 'WND';
}

// Store connection state
walletState.subscribe(state => {
	if (typeof window !== 'undefined') {
		localStorage.setItem('wallet-connected', state.isConnected.toString());
	}
});