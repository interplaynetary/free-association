<script lang="ts">
	import { userNetworkCapacitiesWithShares } from '$lib/state/core.svelte';
	import { userContacts, getUserName, getUserWalletAddress } from '$lib/state/users.svelte';
	import { walletState } from '$lib/state/wallet.svelte';
	import WalletConnection from './WalletConnection.svelte';
	import TokenTransfer from './TokenTransfer.svelte';
	import { toast } from 'svelte-french-toast';
	import { onMount } from 'svelte';

	let selectedRecipient: string | null = null;
	let showTransferModal = false;
	let providerNames: Record<string, string> = {};
	let tokenSymbol: string = 'WND'; // Default token symbol
	let providersWithWallets: Array<{
		providerId: string;
		name: string;
		walletAddress: string;
		capacities: Array<{
			id: string;
			name: string;
			share_percentage: number;
		}>;
	}> = [];

	// Load provider names
	onMount(async () => {
		const capacities = $userNetworkCapacitiesWithShares;
		const providerIds = new Set(Object.values(capacities).map(cap => cap.provider_id));
		
		for (const providerId of providerIds) {
			const name = await getUserName(providerId);
			providerNames[providerId] = name;
		}
	});

	// Reactive computation for providers with wallets
	$: {
		const capacities = $userNetworkCapacitiesWithShares;
		const providerMap = new Map<string, Array<{id: string; name: string; share_percentage: number}>>();
		
		// Group capacities by provider
		Object.entries(capacities).forEach(([capId, capacity]) => {
			const providerId = capacity.provider_id;
			if (!providerMap.has(providerId)) {
				providerMap.set(providerId, []);
			}
			providerMap.get(providerId)!.push({
				id: capId,
				name: capacity.name,
				share_percentage: capacity.share_percentage
			});
		});

		// Find providers with wallet addresses
		providersWithWallets = Array.from(providerMap.entries())
			.map(([providerId, capacities]) => {
				const contacts = $userContacts;
				const contact = Object.values(contacts).find(
					contact => contact.public_key === providerId
				);
				
				return {
					providerId,
					name: providerNames[providerId] || 'Loading...',
					walletAddress: contact?.wallet_address || '',
					capacities
				};
			})
			.filter(provider => provider.walletAddress.length > 0);
	}

	function openTransferModal(providerId: string) {
		selectedRecipient = providerId;
		showTransferModal = true;
	}

	function closeTransferModal() {
		selectedRecipient = null;
		showTransferModal = false;
	}

	function handleTransactionComplete(txHash: string) {
		toast.success(`Tokens sent successfully! Transaction: ${txHash.substring(0, 8)}...`);
		closeTransferModal();
	}

	function truncateAddress(address: string, length: number = 6): string {
		if (!address) return '';
		return `${address.slice(0, length)}...${address.slice(-length)}`;
	}

	function copyToClipboard(text: string) {
		navigator.clipboard.writeText(text);
		toast.success('Address copied to clipboard');
	}

	// Get selected recipient data
	$: selectedRecipientData = selectedRecipient ? 
		providersWithWallets.find(p => p.providerId === selectedRecipient) : 
		null;
</script>

<div class="inventory-wallet-integration">
	<div class="mb-6">
		<h3 class="text-lg font-semibold text-gray-900 mb-2">💰 Send Tokens to Capacity Providers</h3>
		<p class="text-gray-600 text-sm">
			Send {tokenSymbol} tokens to people who provide capacities you have shares in.
		</p>
	</div>

	<!-- Wallet Connection Section -->
	<div class="mb-6">
		<WalletConnection />
	</div>

	<!-- Providers with Wallets Section -->
	<div class="bg-white border border-gray-200 rounded-lg">
		<div class="px-4 py-3 border-b border-gray-200">
			<h4 class="text-md font-medium text-gray-900">Providers with Wallets</h4>
			<p class="text-sm text-gray-600 mt-1">
				Capacity providers who have shared their wallet addresses.
			</p>
		</div>

		{#if providersWithWallets.length === 0}
			<div class="p-6 text-center">
				<svg class="mx-auto h-12 w-12 text-gray-400 mb-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
					<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 9V7a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2m2 4h10a2 2 0 002-2v-6a2 2 0 00-2-2H9a2 2 0 00-2 2v6a2 2 0 002 2zm7-5a2 2 0 11-4 0 2 2 0 014 0z"/>
				</svg>
				<h4 class="text-lg font-medium text-gray-900 mb-2">No Providers with Wallets</h4>
				<p class="text-gray-600">
					None of your capacity providers have shared their wallet addresses yet.
				</p>
			</div>
		{:else}
			<div class="divide-y divide-gray-200">
				{#each providersWithWallets as provider}
					<div class="p-4 hover:bg-gray-50">
						<div class="flex items-center justify-between mb-2">
							<div class="flex items-center space-x-3">
								<div class="flex-shrink-0">
									<div class="w-8 h-8 bg-blue-100 rounded-full flex items-center justify-center">
										<svg class="w-5 h-5 text-blue-600" fill="currentColor" viewBox="0 0 20 20">
											<path fill-rule="evenodd" d="M10 9a3 3 0 100-6 3 3 0 000 6zm-7 9a7 7 0 1114 0H3z" clip-rule="evenodd"/>
										</svg>
									</div>
								</div>
								<div>
									<p class="text-sm font-medium text-gray-900">{provider.name}</p>
									<div class="flex items-center space-x-2">
										<p class="text-xs text-gray-500 font-mono">
											{truncateAddress(provider.walletAddress)}
										</p>
										<button
											on:click={() => copyToClipboard(provider.walletAddress)}
											class="text-xs text-blue-600 hover:text-blue-800 transition-colors"
										>
											Copy
										</button>
									</div>
								</div>
							</div>
							
							{#if $walletState.isConnected}
								<button
									on:click={() => openTransferModal(provider.providerId)}
									class="px-3 py-1 text-sm bg-blue-600 hover:bg-blue-700 text-white rounded-md transition-colors"
								>
									Send Tokens
								</button>
							{:else}
								<span class="px-3 py-1 text-sm bg-gray-100 text-gray-500 rounded-md">
									Connect Wallet
								</span>
							{/if}
						</div>
						
						<!-- Capacity shares -->
						<div class="ml-11 mt-2">
							<p class="text-xs text-gray-500 mb-1">Your shares:</p>
							<div class="flex flex-wrap gap-1">
								{#each provider.capacities as capacity}
									<span class="inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
										{capacity.name}: {(capacity.share_percentage * 100).toFixed(1)}%
									</span>
								{/each}
							</div>
						</div>
					</div>
				{/each}
			</div>
		{/if}
	</div>

	<!-- Transfer Modal -->
	{#if showTransferModal && selectedRecipientData}
		<div class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
			<div class="bg-white rounded-lg max-w-md w-full mx-4 max-h-[90vh] overflow-y-auto">
				<div class="p-4 border-b border-gray-200">
					<div class="flex items-center justify-between">
						<h3 class="text-lg font-semibold text-gray-900">
							Send Tokens to {selectedRecipientData.name}
						</h3>
						<button
							on:click={closeTransferModal}
							class="text-gray-400 hover:text-gray-600 transition-colors"
						>
							<svg class="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
								<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"/>
							</svg>
						</button>
					</div>
				</div>
				
				<div class="p-4">
					<TokenTransfer
						recipientAddress={selectedRecipientData.walletAddress}
						recipientName={selectedRecipientData.name}
						showRecipientInput={false}
						onTransactionComplete={handleTransactionComplete}
					/>
				</div>
			</div>
		</div>
	{/if}
</div>

<style>
	.inventory-wallet-integration {
		max-width: 42rem;
	}
</style>