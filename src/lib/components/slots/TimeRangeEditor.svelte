<script lang="ts">
	/**
	 * TimeRangeEditor - Single time range input
	 * 
	 * Used for editing a single start_time and end_time pair
	 * Can be used standalone or as part of multi-range editors
	 * 
	 * @example
	 * <TimeRangeEditor 
	 *   startTime="09:00" 
	 *   endTime="17:00" 
	 *   onUpdate={(start, end) => {...}}
	 * />
	 */
	
	interface Props {
		startTime: string;
		endTime: string;
		onUpdate: (startTime: string, endTime: string) => void;
		onRemove?: () => void;
		removable?: boolean;
		label?: string;
	}
	
	let {
		startTime = '09:00',
		endTime = '17:00',
		onUpdate,
		onRemove,
		removable = false,
		label
	}: Props = $props();
	
	let localStartTime = $state(startTime);
	let localEndTime = $state(endTime);
	
	// Sync with props
	$effect(() => {
		localStartTime = startTime;
		localEndTime = endTime;
	});
	
	function handleStartChange(e: Event) {
		const value = (e.target as HTMLInputElement).value;
		localStartTime = value;
		onUpdate(value, localEndTime);
	}
	
	function handleEndChange(e: Event) {
		const value = (e.target as HTMLInputElement).value;
		localEndTime = value;
		onUpdate(localStartTime, value);
	}
	
	// Validate that end time is after start time
	const isValid = $derived(() => {
		if (!localStartTime || !localEndTime) return true;
		return localStartTime < localEndTime;
	});
</script>

<div class="time-range-editor">
	{#if label}
		<label class="range-label">{label}</label>
	{/if}
	
	<div class="range-inputs">
		<input
			type="time"
			value={localStartTime}
			oninput={handleStartChange}
			class="time-input"
			class:invalid={!isValid()}
		/>
		
		<span class="separator">→</span>
		
		<input
			type="time"
			value={localEndTime}
			oninput={handleEndChange}
			class="time-input"
			class:invalid={!isValid()}
		/>
		
		{#if removable && onRemove}
			<button
				type="button"
				class="remove-btn"
				onclick={onRemove}
				title="Remove time range"
			>
				×
			</button>
		{/if}
	</div>
	
	{#if !isValid()}
		<p class="error-message">End time must be after start time</p>
	{/if}
</div>

<style>
	.time-range-editor {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.range-label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #64748b;
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.range-inputs {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.time-input {
		padding: 0.5rem 0.75rem;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		font-size: 0.875rem;
		color: #1f2937;
		background: white;
		transition: all 0.2s ease;
		flex: 1;
	}
	
	.time-input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}
	
	.time-input.invalid {
		border-color: #ef4444;
	}
	
	.time-input.invalid:focus {
		box-shadow: 0 0 0 3px rgba(239, 68, 68, 0.1);
	}
	
	.separator {
		color: #94a3b8;
		font-weight: 500;
		flex-shrink: 0;
	}
	
	.remove-btn {
		width: 2rem;
		height: 2rem;
		display: flex;
		align-items: center;
		justify-content: center;
		border: 1px solid #fecaca;
		border-radius: 6px;
		background: #fef2f2;
		color: #dc2626;
		font-size: 1.25rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
		flex-shrink: 0;
	}
	
	.remove-btn:hover {
		background: #fee2e2;
		border-color: #fca5a5;
		transform: scale(1.05);
	}
	
	.error-message {
		margin: 0;
		font-size: 0.75rem;
		color: #dc2626;
		font-style: italic;
	}
</style>

