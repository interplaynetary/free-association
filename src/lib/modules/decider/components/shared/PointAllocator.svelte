<script lang="ts">
	/**
	 * @component PointAllocator
	 * UI for distributing support points
	 * 
	 * @prop {number} value - Current point value
	 * @prop {number} max - Maximum allowed points
	 * @prop {Function} onChange - Callback when value changes
	 */
	
	let { 
		value = $bindable(0), 
		max = 10,
		onChange = undefined
	}: {
		value?: number;
		max?: number;
		onChange?: (newValue: number) => void;
	} = $props();
	
	function increment() {
		if (value < max) {
			value = Math.min(value + 1, max);
			onChange?.(value);
		}
	}
	
	function decrement() {
		if (value > 0) {
			value = Math.max(value - 1, 0);
			onChange?.(value);
		}
	}
	
	function handleInput(e: Event) {
		const target = e.target as HTMLInputElement;
		const newValue = parseInt(target.value) || 0;
		value = Math.max(0, Math.min(newValue, max));
		onChange?.(value);
	}
	
	const percentage = $derived((value / max) * 100);
</script>

<div class="point-allocator">
	<button 
		class="btn-adjust" 
		onclick={decrement} 
		disabled={value <= 0}
		aria-label="Decrease points"
	>
		−
	</button>
	
	<div class="value-container">
		<input 
			type="number" 
			{value}
			oninput={handleInput}
			min="0"
			max={max}
			class="points-input"
			aria-label="Point value"
		/>
		<div class="progress-bar">
			<div class="progress-fill" style="width: {percentage}%"></div>
		</div>
	</div>
	
	<button 
		class="btn-adjust" 
		onclick={increment}
		disabled={value >= max}
		aria-label="Increase points"
	>
		+
	</button>
</div>

<style>
	.point-allocator {
		display: flex;
		align-items: center;
		gap: 0.625rem;
	}
	
	.btn-adjust {
		width: 2.25rem;
		height: 2.25rem;
		border: none;
		background: var(--primary-color, #667eea);
		color: white;
		border-radius: 0.5rem;
		font-size: 1.25rem;
		font-weight: bold;
		cursor: pointer;
		transition: all 0.2s;
		display: flex;
		align-items: center;
		justify-content: center;
	}
	
	.btn-adjust:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	.btn-adjust:disabled {
		opacity: 0.4;
		cursor: not-allowed;
	}
	
	.value-container {
		flex: 1;
		min-width: 4rem;
		display: flex;
		flex-direction: column;
		gap: 0.375rem;
	}
	
	.points-input {
		width: 100%;
		text-align: center;
		padding: 0.5rem;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 0.5rem;
		font-size: 1rem;
		font-weight: 600;
		transition: border-color 0.2s;
	}
	
	.points-input:focus {
		outline: none;
		border-color: var(--primary-color, #667eea);
	}
	
	.progress-bar {
		height: 0.25rem;
		background: var(--bg-muted, #f0f0f0);
		border-radius: 0.125rem;
		overflow: hidden;
	}
	
	.progress-fill {
		height: 100%;
		background: var(--primary-color, #667eea);
		transition: width 0.3s ease;
	}
</style>

