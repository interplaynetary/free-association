<script lang="ts">
	/**
	 * SmartChip - Usage:
	 * <SmartChip 
	 *   value={currentValue} 
	 *   options={[{label: 'Mon', value: 'mon'}]} 
	 *   onChange={val => ...} 
	 * />
	 */
	import { fade, scale } from 'svelte/transition';
	
	interface Option {
		label: string;
		value: any;
		disabled?: boolean;
		divider?: boolean;
	}

	interface Props {
		value?: any;
		label?: string; // Explicit label to show instead of derived from value
		placeholder?: string;
		options?: Option[];
		disabled?: boolean;
		onChange?: (value: any) => void;
	}

	let { 
		value = $bindable(), 
		label,
		placeholder = 'Select...',
		options = [],
		disabled = false,
        layout = 'dropdown', // 'dropdown' | 'layer'
		onChange
	}: Props & { layout?: 'dropdown' | 'layer' } = $props();

	let isOpen = $state(false);
	let chipElement = $state<HTMLElement>();
	let menuElement = $state<HTMLElement>();

	// Determine display text
	let displayText = $derived.by(() => {
		if (label) return label;
		if (value === undefined || value === null) return placeholder;
		
		const option = options.find(o => o.value === value);
		return option ? option.label : String(value);
	});

	function toggle() {
		if (!disabled) isOpen = !isOpen;
	}

	function select(opt: Option) {
		if (opt.disabled || opt.divider) return;
		
		value = opt.value;
		onChange?.(opt.value);
		isOpen = false;
	}

	function handleOutsideClick(event: MouseEvent) {
		if (isOpen && 
			chipElement && !chipElement.contains(event.target as Node) &&
			menuElement && !menuElement.contains(event.target as Node)) {
			isOpen = false;
		}
	}
</script>

<svelte:window onclick={handleOutsideClick} />

<div class="smart-chip-container {layout}" bind:this={chipElement}>
	<button 
		type="button"
		class="smart-chip" 
		class:active={isOpen}
		class:placeholder={!value && !label}
		{disabled}
		onclick={toggle}
	>
		<span class="chip-text">{displayText}</span>
		<svg class="chevron" viewBox="0 0 20 20" fill="currentColor">
			<path fill-rule="evenodd" d="M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z" clip-rule="evenodd" />
		</svg>
	</button>

	{#if isOpen}
		<div 
			class="chip-menu {layout}"
			bind:this={menuElement}
			transition:scale={{ start: 0.95, duration: 100 }}
		>
			{#each options as opt}
				{#if opt.divider}
					<div class="menu-divider"></div>
				{:else}
					<button 
						type="button" 
						class="menu-item"
						class:selected={opt.value === value}
						class:disabled={opt.disabled}
						onclick={() => select(opt)}
					>
						{opt.label}
						{#if opt.value === value}
							<svg class="check-icon" viewBox="0 0 20 20" fill="currentColor">
								<path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd" />
							</svg>
						{/if}
					</button>
				{/if}
			{/each}
		</div>
	{/if}
</div>

<style>
	/* Logic remains the same, just template structure tweaks for divider handling if needed, but mostly CSS */
    
	/* New styles for horizontal pill layout */
	.chip-menu {
		position: absolute;
		top: calc(100% + 0.5rem);
		left: 0;
		z-index: 100;
		
		/* Horizontal Layer Layout */
		display: flex;
		flex-wrap: wrap;
		gap: 0.5rem;
		
		max-width: 400px;
		padding: 0.75rem;
		
		background: white;
		border: 1px solid #e2e8f0;
		border-radius: 12px;
		box-shadow: 
			0 4px 6px -1px rgba(0, 0, 0, 0.1), 
			0 2px 4px -1px rgba(0, 0, 0, 0.06),
			0 0 0 1px rgba(0,0,0,0.02);
	}
    
    /* Layout: Dropdown (Default) */
    .chip-menu.dropdown {
        min-width: 280px;
    }
    
    /* Layout: Layer (Wide) */
    .chip-menu.layer {
        left: 0;
        right: 0;
        width: 100%;
        max-width: none;
        top: 100%;
        margin-top: 0.75rem;
    }

	.menu-item {
		display: inline-flex;
		align-items: center;
		justify-content: center;
		gap: 0.375rem;
		
		width: auto; /* Pill width */
		padding: 0.375rem 0.75rem;
		
		background: white;
		border: 1px solid #e2e8f0;
		border-radius: 9999px; /* Pill shape */
		
		color: #475569;
		font-size: 0.8125rem;
		font-weight: 500;
		
		cursor: pointer;
		transition: all 0.15s ease;
	}

	.menu-item:hover:not(.disabled) {
		background: #f8fafc;
		border-color: #cbd5e1;
		transform: translateY(-1px);
		color: #1e293b;
	}

	.menu-item.selected {
		background: #eef2ff;
		border-color: #818cf8;
		color: #4f46e5;
		font-weight: 600;
		box-shadow: 0 1px 2px rgba(79, 70, 229, 0.1);
	}
	
	.menu-item.disabled {
		opacity: 0.5;
		cursor: not-allowed;
		background: #f1f5f9;
        border-color: transparent;
	}

	/* Force dividers to break line */
	.menu-divider {
		flex-basis: 100%;
		height: 1px;
		background: #f1f5f9;
		margin: 0.25rem 0;
	}

    /* Adjust container to fit */
	.smart-chip-container {
		display: inline-block;
        /* Position relative is ONLY for dropdown mode */
	}
    
    .smart-chip-container.dropdown {
        position: relative;
    }
    
    /* In layer mode, container is static so absolute child finds next relative ancestor */
    .smart-chip-container.layer {
        position: static;
    }

	/* Keep trigger styles mostly same but maybe cleaner */
	.smart-chip {
		display: inline-flex;
		align-items: center;
		gap: 0.25rem;
		padding: 0.125rem 0.375rem;
		background: transparent;
		border: none;
		border-bottom: 2px solid #e2e8f0;
		border-radius: 4px 4px 0 0;
		color: #1e293b;
		font-family: inherit;
		font-size: inherit;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}

	.smart-chip:hover:not(:disabled) {
		background: #f8fafc;
		border-bottom-color: #818cf8;
		color: #4f46e5;
	}

	.smart-chip.active {
		border-bottom-color: #4f46e5;
		color: #4338ca;
	}
	
    /* Adjust chevron */
	.chevron {
		width: 0.875rem;
		height: 0.875rem;
		opacity: 0.4;
		margin-left: 0.125rem;
		transition: transform 0.2s;
	}
    
    .smart-chip:hover .chevron { opacity: 0.8; }
    .smart-chip.active .chevron { transform: rotate(180deg); opacity: 1; }

	.check-icon {
		display: none;
	}
</style>
