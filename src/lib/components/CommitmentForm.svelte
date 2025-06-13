<script lang="ts">
	import { createCommitment, validateCommitment, getRemainingCommitmentCapacity } from '$lib/protocol';
	import type { CommitmentsCollection } from '$lib/schema';
	
	// Props
	let {
		commitments = {},
		committerId,
		onCommitmentCreated,
		onCancel
	} = $props<{
		commitments: CommitmentsCollection;
		committerId: string;
		onCommitmentCreated: (commitment: any) => void;
		onCancel?: () => void;
	}>();
	
	// Form state
	let name = $state('');
	let description = $state('');
	let recipientId = $state('');
	let percentage = $state(0);
	let tags = $state('');
	let validationError = $state('');
	
	// Computed values
	const availableCapacity = $derived(getRemainingCommitmentCapacity(commitments, committerId));
	const maxPercentage = $derived(Math.floor(availableCapacity * 100));
	
	// Validate form
	const isValid = $derived(
		name.trim().length > 0 && 
		recipientId.trim().length > 0 && 
		percentage > 0 && 
		percentage <= maxPercentage &&
		validationError === ''
	);
	
	// Handle percentage change
	function handlePercentageChange(event: Event) {
		const target = event.target as HTMLInputElement;
		const value = parseInt(target.value);
		percentage = value;
		
		// Validate the percentage
		const validation = validateCommitment(commitments, committerId, value / 100);
		validationError = validation.valid ? '' : validation.error || '';
	}
	
	// Handle form submission
	function handleSubmit() {
		if (!isValid) return;
		
		const commitment = createCommitment(
			crypto.randomUUID(),
			name.trim(),
			percentage / 100, // Convert to decimal
			recipientId.trim(),
			committerId,
			description.trim() || undefined,
			tags.split(',').map(t => t.trim()).filter(t => t.length > 0)
		);
		
		onCommitmentCreated(commitment);
		
		// Reset form
		name = '';
		description = '';
		recipientId = '';
		percentage = 0;
		tags = '';
		validationError = '';
	}
</script>

<div class="commitment-form">
	<h3>Create New Commitment</h3>
	<p class="available-capacity">
		Available capacity: <strong>{(availableCapacity * 100).toFixed(1)}%</strong>
	</p>
	
	<form on:submit|preventDefault={handleSubmit}>
		<div class="form-group">
			<label for="name">Commitment Name *</label>
			<input
				id="name"
				type="text"
				bind:value={name}
				placeholder="e.g., Support for Community Garden"
				required
			/>
		</div>
		
		<div class="form-group">
			<label for="recipient">Recipient ID *</label>
			<input
				id="recipient"
				type="text"
				bind:value={recipientId}
				placeholder="Enter recipient user ID"
				required
			/>
		</div>
		
		<div class="form-group">
			<label for="percentage">Percentage (%) *</label>
			<input
				id="percentage"
				type="number"
				min="1"
				max={maxPercentage}
				step="1"
				bind:value={percentage}
				on:input={handlePercentageChange}
				placeholder="e.g., 25"
				required
			/>
			<small>Max: {maxPercentage}%</small>
		</div>
		
		<div class="form-group">
			<label for="description">Description</label>
			<textarea
				id="description"
				bind:value={description}
				placeholder="Optional description of this commitment"
				rows="3"
			></textarea>
		</div>
		
		<div class="form-group">
			<label for="tags">Tags</label>
			<input
				id="tags"
				type="text"
				bind:value={tags}
				placeholder="Optional tags (comma-separated)"
			/>
		</div>
		
		{#if validationError}
			<div class="error-message">{validationError}</div>
		{/if}
		
		<div class="form-actions">
			<button type="submit" disabled={!isValid} class="create-btn">
				Create Commitment
			</button>
			{#if onCancel}
				<button type="button" on:click={onCancel} class="cancel-btn">
					Cancel
				</button>
			{/if}
		</div>
	</form>
</div>

<style>
	.commitment-form {
		background: white;
		border-radius: 8px;
		padding: 24px;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
		max-width: 500px;
		margin: 0 auto;
	}
	
	.commitment-form h3 {
		margin: 0 0 16px 0;
		color: #333;
		font-size: 1.5rem;
	}
	
	.available-capacity {
		background: #f0f8ff;
		border: 1px solid #b3d9ff;
		border-radius: 4px;
		padding: 8px 12px;
		margin-bottom: 20px;
		font-size: 0.9rem;
		color: #0066cc;
	}
	
	.form-group {
		margin-bottom: 16px;
	}
	
	.form-group label {
		display: block;
		margin-bottom: 4px;
		font-weight: 500;
		color: #555;
	}
	
	.form-group input,
	.form-group textarea {
		width: 100%;
		padding: 8px 12px;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 14px;
		box-sizing: border-box;
	}
	
	.form-group input:focus,
	.form-group textarea:focus {
		outline: none;
		border-color: #4CAF50;
		box-shadow: 0 0 0 2px rgba(76, 175, 80, 0.2);
	}
	
	.form-group small {
		display: block;
		margin-top: 4px;
		color: #666;
		font-size: 0.8rem;
	}
	
	.error-message {
		background: #ffebee;
		border: 1px solid #f44336;
		border-radius: 4px;
		padding: 8px 12px;
		margin-bottom: 16px;
		color: #d32f2f;
		font-size: 0.9rem;
	}
	
	.form-actions {
		display: flex;
		gap: 12px;
		justify-content: flex-end;
	}
	
	.create-btn {
		background: #4CAF50;
		color: white;
		border: none;
		padding: 10px 20px;
		border-radius: 4px;
		cursor: pointer;
		font-size: 14px;
		font-weight: 500;
	}
	
	.create-btn:hover:not(:disabled) {
		background: #45a049;
	}
	
	.create-btn:disabled {
		background: #ccc;
		cursor: not-allowed;
	}
	
	.cancel-btn {
		background: #f5f5f5;
		color: #333;
		border: 1px solid #ddd;
		padding: 10px 20px;
		border-radius: 4px;
		cursor: pointer;
		font-size: 14px;
	}
	
	.cancel-btn:hover {
		background: #e8e8e8;
	}
</style>