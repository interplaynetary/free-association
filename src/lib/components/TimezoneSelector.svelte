<script lang="ts">
	import { onMount } from 'svelte';
	import Svelecte from 'svelecte';
	import * as ct from 'countries-and-timezones';

	interface Props {
		value?: string;
		placeholder?: string;
		onselect?: (timezone: { name: string; utcOffsetStr: string; countries: string[] }) => void;
		disabled?: boolean;
		countryFilter?: string; // Filter timezones by country
	}

	let {
		value = '',
		placeholder = 'Select timezone...',
		onselect,
		disabled = false,
		countryFilter = ''
	}: Props = $props();

	let timezones = $state<
		Array<{ value: string; text: string; utcOffsetStr: string; countries: string[] }>
	>([]);
	let selectedValue = $state('');
	let lastProcessedValue = $state(''); // Track what we last processed to avoid loops
	let lastCountryFilter = $state('');

	onMount(() => {
		loadTimezones();
		initializeSelectedValue();
	});

	// Reload timezones when countryFilter changes
	$effect(() => {
		if (countryFilter !== lastCountryFilter) {
			lastCountryFilter = countryFilter;
			loadTimezones();
			// Re-initialize after loading new timezones
			if (timezones.length > 0) {
				initializeSelectedValue();
			}
		}
	});

	function loadTimezones() {
		const allTimezones = ct.getAllTimezones();
		let filteredTimezones = Object.values(allTimezones);

		// Filter by country if specified
		if (countryFilter) {
			filteredTimezones = filteredTimezones.filter((tz) =>
				tz.countries.includes(countryFilter as any)
			);
		}

		timezones = filteredTimezones
			.filter((tz) => !tz.aliasOf) // Exclude aliases
			.map((tz) => ({
				value: tz.name,
				text: `${tz.name} (${tz.utcOffsetStr})`,
				utcOffsetStr: tz.utcOffsetStr,
				countries: tz.countries
			}))
			.sort((a, b) => {
				// Sort by UTC offset first, then by name
				const offsetA = parseFloat(a.utcOffsetStr.replace(':', '.'));
				const offsetB = parseFloat(b.utcOffsetStr.replace(':', '.'));
				if (offsetA !== offsetB) {
					return offsetA - offsetB;
				}
				return a.value.localeCompare(b.value);
			});
	}

	// Initialize selectedValue (called after timezones load, and when value prop changes)
	function initializeSelectedValue() {
		if (timezones.length === 0 || value === lastProcessedValue) return;

		lastProcessedValue = value;
		selectedValue = value || '';
	}

	// Watch for value prop changes (but only update if different from last processed)
	$effect(() => {
		if (timezones.length > 0) {
			initializeSelectedValue();
		}
	});

	// Only handle user interactions via Svelecte events
	function handleSelect(event: any) {
		const selected = event?.detail;
		if (selected) {
			selectedValue = selected.value;
			const timezoneData = timezones.find((tz) => tz.value === selected.value);
			if (timezoneData && onselect) {
				onselect({
					name: timezoneData.value,
					utcOffsetStr: timezoneData.utcOffsetStr,
					countries: timezoneData.countries
				});
			}
		}
	}

	function handleClear() {
		selectedValue = '';
		if (onselect) {
			onselect({ name: '', utcOffsetStr: '', countries: [] });
		}
	}
</script>

<div class="timezone-selector">
	<Svelecte
		options={timezones}
		bind:value={selectedValue}
		{placeholder}
		{disabled}
		clearable={true}
		searchable={true}
		class="timezone-select"
	/>
</div>

<style>
	.timezone-selector {
		width: 100%;
	}

	:global(.timezone-select) {
		font-size: 0.875rem;
	}

	:global(.timezone-select .sv-control) {
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		min-height: 32px;
		transition: all 0.2s ease;
	}

	:global(.timezone-select .sv-control:focus-within) {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	:global(.timezone-select .sv-item) {
		padding: 6px 8px;
		font-size: 0.875rem;
	}

	:global(.timezone-select .sv-dropdown) {
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		max-height: 200px;
	}
</style>
