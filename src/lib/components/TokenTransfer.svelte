<script lang="ts">
	import { walletState, sendTransaction, getTokenDecimals, getTokenSymbol } from '$lib/state/wallet.svelte';
	import { getContactByWalletAddress } from '$lib/state/users.svelte';
	import { toast } from 'svelte-french-toast';
	
	export let recipientAddress: string = '';
	export let recipientName: string = '';
	export let onTransactionComplete: ((txHash: string) => void) | undefined = undefined;
	export let showRecipientInput: boolean = true;

	let amount: string = '';
	let isTransferring = false;
	let transferError: string | null = null;
	let tokenSymbol: string = 'DOT'; // Default, will be updated when wallet connects

	// Validation
	$: isValidAmount = amount && parseFloat(amount) > 0;
	$: isValidRecipient = recipientAddress.trim().length > 0;
	$: canTransfer = $walletState.isConnected && 
		$walletState.selectedAccount && 
		isValidAmount && 
		isValidRecipient && 
		!isTransferring;

	// Get recipient info
	$: recipientContact = recipientAddress ? getContactByWalletAddress(recipientAddress) : null;
	$: displayRecipientName = recipientName || recipientContact?.name || truncateAddress(recipientAddress);

	// Update token symbol when wallet connects
	$: if ($walletState.isConnected && $walletState.api) {
		getTokenSymbol().then(symbol => {
			tokenSymbol = symbol;
		}).catch(err => {
			console.error('Failed to get token symbol:', err);
		});
	}

	async function handleTransfer() {
		if (!canTransfer) return;

		isTransferring = true;
		transferError = null;

		try {
			// Get token decimals from chain metadata
			const decimals = await getTokenDecimals();
			
			// Convert amount to planck (smallest unit)
			const amountInPlanck = parseFloat(amount) * Math.pow(10, decimals);
			
			const txHash = await sendTransaction(recipientAddress, amountInPlanck.toString());
			
			// Reset form
			amount = '';
			if (showRecipientInput) {
				recipientAddress = '';
				recipientName = '';
			}
			
			// Call completion callback
			if (onTransactionComplete) {
				onTransactionComplete(txHash);
			}
			
		} catch (error) {
			const errorMessage = error instanceof Error ? error.message : 'Transaction failed';
			transferError = errorMessage;
			console.error('Transfer error:', error);
		} finally {
			isTransferring = false;
		}
	}

	function truncateAddress(address: string, length: number = 6): string {
		if (!address) return '';
		return `${address.slice(0, length)}...${address.slice(-length)}`;
	}

	function handleAmountInput(event: Event) {
		const target = event.target as HTMLInputElement;
		const value = target.value;
		
		// Only allow valid decimal numbers
		if (value === '' || /^\d*\.?\d*$/.test(value)) {
			amount = value;
		}
	}
</script>

<div class="token-transfer">
	{#if !$walletState.isConnected}
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
					<h3 class="text-sm font-medium text-amber-800">Wallet Required</h3>
					<p class="text-sm text-amber-700 mt-1">
						Please connect your Polkadot wallet to send {tokenSymbol} tokens.
					</p>
				</div>
			</div>
		</div>
	{:else}
		<div class="bg-white border border-gray-200 rounded-lg p-4">
			<div class="flex items-center mb-4">
				<svg class="w-5 h-5 text-purple-600 mr-2" fill="currentColor" viewBox="0 0 20 20">
					<path d="M8 5a1 1 0 100 2h5.586L2 18.586A1 1 0 003.414 20L15 8.414V14a1 1 0 102 0V6a1 1 0 00-1-1H8z"/>
				</svg>
				<h3 class="text-lg font-semibold text-gray-900">Send {tokenSymbol} Tokens</h3>
			</div>

			<form on:submit|preventDefault={handleTransfer} class="space-y-4">
				{#if showRecipientInput}
					<div>
						<label for="recipient" class="block text-sm font-medium text-gray-700 mb-1">
							Recipient Address
						</label>
						<input
							id="recipient"
							type="text"
							bind:value={recipientAddress}
							placeholder="Enter Polkadot address..."
							class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-transparent"
							required
						/>
						{#if recipientContact}
							<p class="text-sm text-gray-600 mt-1">
								Sending to: <span class="font-medium">{recipientContact.name}</span>
							</p>
						{/if}
					</div>
				{:else if recipientAddress}
					<div class="bg-gray-50 border border-gray-200 rounded-lg p-3">
						<div class="flex items-center justify-between">
							<div>
								<p class="text-sm font-medium text-gray-900">Recipient</p>
								<p class="text-sm text-gray-600">{displayRecipientName}</p>
								<p class="text-xs text-gray-500 font-mono">{truncateAddress(recipientAddress)}</p>
							</div>
						</div>
					</div>
				{/if}

				<div>
					<label for="amount" class="block text-sm font-medium text-gray-700 mb-1">
						Amount ({tokenSymbol})
					</label>
					<div class="relative">
						<input
							id="amount"
							type="text"
							value={amount}
							on:input={handleAmountInput}
							placeholder="0.00"
							class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-transparent"
							required
						/>
						<div class="absolute inset-y-0 right-0 pr-3 flex items-center">
							<span class="text-gray-500 text-sm">{tokenSymbol}</span>
						</div>
					</div>
					{#if $walletState.balance}
						<p class="text-sm text-gray-600 mt-1">
							Available: <span class="font-medium">{$walletState.balance}</span>
						</p>
					{/if}
				</div>

				{#if transferError}
					<div class="bg-red-50 border border-red-200 rounded-lg p-3">
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
								<h3 class="text-sm font-medium text-red-800">Transfer Failed</h3>
								<p class="text-sm text-red-700 mt-1">{transferError}</p>
							</div>
						</div>
					</div>
				{/if}

				<div class="flex items-center justify-between pt-4">
					<div class="text-sm text-gray-600">
						{#if $walletState.selectedAccount}
							From: <span class="font-medium">{$walletState.selectedAccount.name || 'Account'}</span>
						{/if}
					</div>
					<button
						type="submit"
						disabled={!canTransfer}
						class="px-4 py-2 bg-purple-600 hover:bg-purple-700 disabled:bg-gray-300 disabled:cursor-not-allowed text-white font-medium rounded-lg transition-colors flex items-center"
					>
						{#if isTransferring}
							<svg class="animate-spin -ml-1 mr-2 h-4 w-4 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
								<circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
								<path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
							</svg>
							Sending...
						{:else}
							<svg class="w-4 h-4 mr-2" fill="currentColor" viewBox="0 0 20 20">
								<path d="M8 5a1 1 0 100 2h5.586L2 18.586A1 1 0 003.414 20L15 8.414V14a1 1 0 102 0V6a1 1 0 00-1-1H8z"/>
							</svg>
							Send Tokens
						{/if}
					</button>
				</div>
			</form>
		</div>
	{/if}
</div>

<style>
	.token-transfer {
		max-width: 28rem;
		margin: 0 auto;
	}
</style>