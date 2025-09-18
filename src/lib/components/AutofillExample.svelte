<script lang="ts">
	import CountrySelector from './CountrySelector.svelte';
	import TimezoneSelector from './TimezoneSelector.svelte';

	// Demo state
	let selectedCountry = $state('');
	let selectedCountryId = $state('');
	let selectedTimezone = $state('');

	// Address fields for display
	let country = $state('');

	// Handle country selection
	function handleCountrySelect(countryData: { id: string; name: string; timezones: string[] }) {
		selectedCountry = countryData.name;
		selectedCountryId = countryData.id;
		country = countryData.name;

		// Auto-set timezone if not already set
		if (countryData.timezones.length > 0 && !selectedTimezone) {
			selectedTimezone = countryData.timezones[0];
		}

		console.log('Selected country:', countryData);
	}

	// Handle timezone selection
	function handleTimezoneSelect(timezoneData: {
		name: string;
		utcOffsetStr: string;
		countries: string[];
	}) {
		selectedTimezone = timezoneData.name;
		console.log('Selected timezone:', timezoneData);
	}
</script>

<div class="autofill-demo space-y-6 p-6">
	<h1 class="text-2xl font-bold text-gray-900">Autofill Components Demo</h1>

	<div class="grid grid-cols-1 gap-6 md:grid-cols-2">
		<!-- Country Selector -->
		<div class="demo-section">
			<h2 class="mb-3 text-lg font-semibold text-gray-700">Country Selector</h2>
			<CountrySelector
				value={selectedCountryId}
				placeholder="Select a country..."
				onselect={handleCountrySelect}
			/>
			{#if selectedCountry}
				<div class="result mt-2 rounded bg-blue-50 p-2 text-sm">
					Selected: <strong>{selectedCountry}</strong> (ID: {selectedCountryId})
				</div>
			{/if}
		</div>

		<!-- Timezone Selector -->
		<div class="demo-section">
			<h2 class="mb-3 text-lg font-semibold text-gray-700">Timezone Selector</h2>
			<TimezoneSelector
				value={selectedTimezone}
				placeholder="Select a timezone..."
				onselect={handleTimezoneSelect}
				countryFilter={selectedCountryId}
			/>
			{#if selectedTimezone}
				<div class="result mt-2 rounded bg-green-50 p-2 text-sm">
					Selected: <strong>{selectedTimezone}</strong>
				</div>
			{/if}
		</div>
	</div>

	<!-- Current Values Display -->
	<div class="values-display rounded-lg bg-gray-50 p-4">
		<h3 class="text-md mb-3 font-semibold text-gray-700">Current Values</h3>
		<div class="grid grid-cols-1 gap-4 text-sm md:grid-cols-2">
			<div>
				<strong>Selected Values:</strong>
				<ul class="mt-1 space-y-1">
					<li>Country: {country || 'Not set'}</li>
					<li>Country ID: {selectedCountryId || 'Not set'}</li>
					<li>Timezone: {selectedTimezone || 'Not set'}</li>
				</ul>
			</div>
		</div>
	</div>

	<!-- Usage Instructions -->
	<div class="instructions rounded-lg bg-blue-50 p-4">
		<h3 class="text-md mb-2 font-semibold text-blue-900">How to Use</h3>
		<ul class="space-y-1 text-sm text-blue-800">
			<li>
				• <strong>Country Selector:</strong> Search and select from all world countries. Automatically
				suggests timezone.
			</li>
			<li>
				• <strong>Timezone Selector:</strong> Browse all timezones, optionally filtered by selected country.
			</li>
			<li>• All components support keyboard navigation and are fully accessible.</li>
		</ul>
	</div>
</div>

<style>
	.demo-section {
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 16px;
		background: white;
	}

	.result {
		border-left: 4px solid currentColor;
	}

	.values-display ul {
		font-family: monospace;
		background: white;
		padding: 8px;
		border-radius: 4px;
		border: 1px solid #d1d5db;
	}
</style>
