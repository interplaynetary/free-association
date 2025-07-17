<script lang="ts">
	import { onMount } from 'svelte';
	import {
		walletState,
		connectWallet,
		disconnectWallet,
		selectAccount,
		autoConnectWallet,
		isExtensionAvailable,
		getAvailableExtensions
	} from '$lib/state/wallet.svelte';
	import { toast } from 'svelte-french-toast';

	let showAccountSelector = false;
	let availableExtensions: string[] = [];

	onMount(() => {
		availableExtensions = getAvailableExtensions();
		// Auto-connect on mount
		autoConnectWallet();
	});

	async function handleConnect() {
		try {
			await connectWallet();
		} catch (error) {
			console.error('Connection failed:', error);
		}
	}

	function handleDisconnect() {
		disconnectWallet();
		showAccountSelector = false;
	}

	async function handleAccountSelect(address: string) {
		try {
			await selectAccount(address);
			showAccountSelector = false;
		} catch (error) {
			console.error('Account selection failed:', error);
			toast.error('Failed to select account');
		}
	}

	function truncateAddress(address: string, length: number = 6): string {
		return `${address.slice(0, length)}...${address.slice(-length)}`;
	}
</script>

<div class="wallet-connection">
	{#if !isExtensionAvailable()}
		<div class="bg-amber-50 border border-amber-200 rounded-lg p-4 mb-4">
			<div class="flex items-center">
				<div class="flex-shrink-0">
					<svg class="h-5 w-5 text-amber-400" viewBox="0 0 20 20" fill="currentColor">
						<path
							fill-rule="evenodd"
							d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z"
							clip-rule="evenodd"
						/>
					</svg>
				</div>
				<div class="ml-3">
					<h3 class="text-sm font-medium text-amber-800">No Polkadot Wallet Found</h3>
					<p class="text-sm text-amber-700 mt-1">
						Please install a Polkadot wallet extension like
						<a
							href="https://polkadot.js.org/extension/"
							target="_blank"
							rel="noopener noreferrer"
							class="underline hover:text-amber-900"
						>
							Polkadot.js
						</a>
						to connect your wallet.
					</p>
				</div>
			</div>
		</div>
	{:else if availableExtensions.length > 0}
		<div class="bg-green-50 border border-green-200 rounded-lg p-4 mb-4">
			<div class="flex items-center">
				<div class="flex-shrink-0">
					<svg class="h-5 w-5 text-green-400" viewBox="0 0 20 20" fill="currentColor">
						<path
							fill-rule="evenodd"
							d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
							clip-rule="evenodd"
						/>
					</svg>
				</div>
				<div class="ml-3">
					<h3 class="text-sm font-medium text-green-800">Wallet Extensions Available</h3>
					<p class="text-sm text-green-700 mt-1">
						Found: {availableExtensions.join(', ')}
					</p>
				</div>
			</div>
		</div>
	{/if}

	{#if $walletState.isConnected && $walletState.selectedAccount}
		<div class="connected-wallet">
			<div class="flex items-center justify-between p-4 bg-green-50 border border-green-200 rounded-lg">
				<div class="flex items-center space-x-3">
					<div class="flex-shrink-0">
						<div class="w-8 h-8 bg-green-100 rounded-full flex items-center justify-center">
							<svg class="w-5 h-5 text-green-600" fill="currentColor" viewBox="0 0 20 20">
								<path
									fill-rule="evenodd"
									d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z"
									clip-rule="evenodd"
								/>
							</svg>
						</div>
					</div>
					<div>
						<p class="text-sm font-medium text-green-800">
							{$walletState.selectedAccount.name || 'Unnamed Account'}
						</p>
						<p class="text-sm text-green-600">
							{truncateAddress($walletState.selectedAccount.address)}
						</p>
						{#if $walletState.balance}
							<p class="text-xs text-green-600">Balance: {$walletState.balance}</p>
						{/if}
					</div>
				</div>
				<div class="flex space-x-2">
					{#if $walletState.accounts.length > 1}
						<button
							on:click={() => (showAccountSelector = !showAccountSelector)}
							class="px-3 py-1 text-sm bg-green-100 hover:bg-green-200 text-green-700 rounded transition-colors"
						>
							Switch Account
						</button>
					{/if}
					<button
						on:click={handleDisconnect}
						class="px-3 py-1 text-sm bg-red-100 hover:bg-red-200 text-red-700 rounded transition-colors"
					>
						Disconnect
					</button>
				</div>
			</div>

			{#if showAccountSelector && $walletState.accounts.length > 1}
				<div class="mt-2 bg-white border border-gray-200 rounded-lg shadow-sm">
					<div class="p-3 border-b border-gray-200">
						<h4 class="text-sm font-medium text-gray-900">Select Account</h4>
					</div>
					<div class="max-h-60 overflow-y-auto">
						{#each $walletState.accounts as account}
							<button
								on:click={() => handleAccountSelect(account.address)}
								class="w-full px-3 py-2 text-left hover:bg-gray-50 flex items-center justify-between group"
								class:bg-blue-50={$walletState.selectedAccount?.address === account.address}
							>
								<div>
									<p class="text-sm font-medium text-gray-900">
										{account.name || 'Unnamed Account'}
									</p>
									<p class="text-xs text-gray-500">{truncateAddress(account.address)}</p>
								</div>
								{#if $walletState.selectedAccount?.address === account.address}
									<svg class="w-4 h-4 text-blue-600" fill="currentColor" viewBox="0 0 20 20">
										<path
											fill-rule="evenodd"
											d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
											clip-rule="evenodd"
										/>
									</svg>
								{/if}
							</button>
						{/each}
					</div>
				</div>
			{/if}
		</div>
	{:else}
		<div class="connect-wallet">
			<button
				on:click={handleConnect}
				disabled={$walletState.isConnecting || !isExtensionAvailable()}
				class="w-full px-4 py-2 bg-blue-600 hover:bg-blue-700 disabled:bg-gray-300 disabled:cursor-not-allowed text-white font-medium rounded-lg transition-colors flex items-center justify-center"
			>
				{#if $walletState.isConnecting}
					<svg class="animate-spin -ml-1 mr-3 h-5 w-5 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
						<circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
						<path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
					</svg>
					Connecting...
				{:else}
					<svg class="w-5 h-5 mr-2" fill="currentColor" viewBox="0 0 20 20">
						<path
							fill-rule="evenodd"
							d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z"
							clip-rule="evenodd"
						/>
					</svg>
					Connect Polkadot Wallet
				{/if}
			</button>
		</div>
	{/if}

	{#if $walletState.error}
		<div class="mt-4 bg-red-50 border border-red-200 rounded-lg p-4">
			<div class="flex">
				<div class="flex-shrink-0">
					<svg class="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
						<path
							fill-rule="evenodd"
							d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
							clip-rule="evenodd"
						/>
					</svg>
				</div>
				<div class="ml-3">
					<h3 class="text-sm font-medium text-red-800">Connection Error</h3>
					<p class="text-sm text-red-700 mt-1">{$walletState.error}</p>
				</div>
			</div>
		</div>
	{/if}
</div>

<style>
	.wallet-connection {
		@apply max-w-md mx-auto;
	}
</style>