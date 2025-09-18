<script lang="ts">
	import { onMount } from 'svelte';
	import Svelecte from 'svelecte';
	import * as ct from 'countries-and-timezones';

	interface Props {
		value?: string; // Can be country ID or country name
		placeholder?: string;
		onselect?: (country: { id: string; name: string; timezones: string[] }) => void;
		disabled?: boolean;
	}

	let {
		value = '',
		placeholder = 'Select country...',
		onselect,
		disabled = false
	}: Props = $props();

	let countries = $state<Array<{ value: string; text: string; timezones: string[] }>>([]);
	let selectedValue = $state('');
	let lastProcessedValue = $state(''); // Track what we last processed to avoid loops

	onMount(() => {
		// Get all countries and format for Svelecte
		const allCountries = ct.getAllCountries();
		countries = Object.values(allCountries)
			.map((country) => ({
				value: country.id,
				text: country.name,
				timezones: country.timezones
			}))
			.sort((a, b) => a.text.localeCompare(b.text));

		// Initialize selectedValue after countries are loaded
		initializeSelectedValue();
	});

	// Helper function to find country ID from name
	function findCountryIdByName(countryName: string): string {
		if (!countryName || countries.length === 0) return '';
		const countryEntry = countries.find((c) => c.text === countryName);
		return countryEntry ? countryEntry.value : '';
	}

	// Initialize selectedValue (called once after countries load, and when value prop changes)
	function initializeSelectedValue() {
		if (countries.length === 0 || value === lastProcessedValue) return;

		lastProcessedValue = value;

		if (value) {
			// Check if value is already a country ID (2-letter code)
			if (value.length === 2 && countries.find((c) => c.value === value)) {
				selectedValue = value;
			} else {
				// Value might be a country name, try to find the ID
				const foundId = findCountryIdByName(value);
				selectedValue = foundId;
			}
		} else {
			selectedValue = '';
		}
	}

	// Watch for value prop changes (but only update if different from last processed)
	$effect(() => {
		if (countries.length > 0) {
			initializeSelectedValue();
		}
	});

	// Only handle user interactions via Svelecte events
	function handleSelect(event: any) {
		const selected = event?.detail;
		if (selected) {
			selectedValue = selected.value;
			const countryData = countries.find((c) => c.value === selected.value);
			if (countryData && onselect) {
				onselect({
					id: countryData.value,
					name: countryData.text,
					timezones: countryData.timezones
				});
			}
		}
	}

	function handleClear() {
		selectedValue = '';
		if (onselect) {
			onselect({ id: '', name: '', timezones: [] });
		}
	}
</script>

<div class="country-selector">
	<Svelecte
		options={countries}
		bind:value={selectedValue}
		{placeholder}
		{disabled}
		clearable={true}
		searchable={true}
		on:change={handleSelect}
		on:clear={handleClear}
		class="country-select"
	/>
</div>

<style>
	.country-selector {
		width: 100%;
	}

	:global(.country-select) {
		font-size: 0.875rem;
	}

	:global(.country-select .sv-control) {
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		background: white;
		min-height: 32px;
		transition: all 0.2s ease;
	}

	:global(.country-select .sv-control:focus-within) {
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px rgba(59, 130, 246, 0.2);
	}

	:global(.country-select .sv-item) {
		padding: 6px 8px;
		font-size: 0.875rem;
	}

	:global(.country-select .sv-dropdown) {
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		max-height: 200px;
	}
</style>
