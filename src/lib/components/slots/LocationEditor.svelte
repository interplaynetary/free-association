<script lang="ts">
	/**
	 * LocationEditor - Flexible location input
	 * 
	 * Supports three location types:
	 * - Specific address (street address, city, state, postal code, country)
	 * - Coordinates (latitude, longitude)
	 * - Online (link)
	 * - Undefined
	 * 
	 * Can use presets (Home, Work) or custom locations
	 */
	
	interface Props {
		locationType?: string;
		streetAddress?: string;
		city?: string;
		stateProvince?: string;
		postalCode?: string;
		country?: string;
		latitude?: number;
		longitude?: number;
		onlineLink?: string;
		onUpdate: (location: LocationData) => void;
	}
	
	export interface LocationData {
		location_type?: string;
		street_address?: string;
		city?: string;
		state_province?: string;
		postal_code?: string;
		country?: string;
		latitude?: number;
		longitude?: number;
		online_link?: string;
	}
	
	let {
		locationType = 'Undefined',
		streetAddress,
		city,
		stateProvince,
		postalCode,
		country,
		latitude,
		longitude,
		onlineLink,
		onUpdate
	}: Props = $props();
	
	let localLocationType = $state(locationType);
	let localStreetAddress = $state(streetAddress);
	let localCity = $state(city);
	let localStateProvince = $state(stateProvince);
	let localPostalCode = $state(postalCode);
	let localCountry = $state(country);
	let localLatitude = $state(latitude);
	let localLongitude = $state(longitude);
	let localOnlineLink = $state(onlineLink);
	
	// Sync with props
	$effect(() => {
		localLocationType = locationType;
		localStreetAddress = streetAddress;
		localCity = city;
		localStateProvince = stateProvince;
		localPostalCode = postalCode;
		localCountry = country;
		localLatitude = latitude;
		localLongitude = longitude;
		localOnlineLink = onlineLink;
	});
	
	function emitUpdate() {
		onUpdate({
			location_type: localLocationType,
			street_address: localStreetAddress,
			city: localCity,
			state_province: localStateProvince,
			postal_code: localPostalCode,
			country: localCountry,
			latitude: localLatitude,
			longitude: localLongitude,
			online_link: localOnlineLink
		});
	}
	
	function handleLocationTypeChange(e: Event) {
		localLocationType = (e.target as HTMLSelectElement).value;
		emitUpdate();
	}
</script>

<div class="location-editor">
	<h4 class="editor-title">📍 Location</h4>
	
	<div class="form-field">
		<label for="location-type">Type:</label>
		<select
			id="location-type"
			bind:value={localLocationType}
			onchange={handleLocationTypeChange}
			class="location-type-select"
		>
			<option value="Undefined">Not specified</option>
			<option value="Specific">Specific address</option>
			<option value="Coordinates">GPS coordinates</option>
			<option value="Online">Online/Virtual</option>
		</select>
	</div>
	
	{#if localLocationType === 'Specific'}
		<div class="address-fields">
			<div class="form-field">
				<label for="street-address">Street Address:</label>
				<input
					id="street-address"
					type="text"
					bind:value={localStreetAddress}
					onblur={emitUpdate}
					placeholder="123 Main St"
					class="text-input"
				/>
			</div>
			
			<div class="form-row">
				<div class="form-field">
					<label for="city">City:</label>
					<input
						id="city"
						type="text"
						bind:value={localCity}
						onblur={emitUpdate}
						placeholder="City"
						class="text-input"
					/>
				</div>
				
				<div class="form-field">
					<label for="state">State/Province:</label>
					<input
						id="state"
						type="text"
						bind:value={localStateProvince}
						onblur={emitUpdate}
						placeholder="State"
						class="text-input"
					/>
				</div>
			</div>
			
			<div class="form-row">
				<div class="form-field">
					<label for="postal">Postal Code:</label>
					<input
						id="postal"
						type="text"
						bind:value={localPostalCode}
						onblur={emitUpdate}
						placeholder="12345"
						class="text-input"
					/>
				</div>
				
				<div class="form-field">
					<label for="country">Country:</label>
					<input
						id="country"
						type="text"
						bind:value={localCountry}
						onblur={emitUpdate}
						placeholder="Country"
						class="text-input"
					/>
				</div>
			</div>
		</div>
	{:else if localLocationType === 'Coordinates'}
		<div class="coordinates-fields">
			<div class="form-row">
				<div class="form-field">
					<label for="latitude">Latitude:</label>
					<input
						id="latitude"
						type="number"
						step="0.000001"
						min="-90"
						max="90"
						bind:value={localLatitude}
						onblur={emitUpdate}
						placeholder="37.7749"
						class="text-input"
					/>
				</div>
				
				<div class="form-field">
					<label for="longitude">Longitude:</label>
					<input
						id="longitude"
						type="number"
						step="0.000001"
						min="-180"
						max="180"
						bind:value={localLongitude}
						onblur={emitUpdate}
						placeholder="-122.4194"
						class="text-input"
					/>
				</div>
			</div>
		</div>
	{:else if localLocationType === 'Online'}
		<div class="online-fields">
			<div class="form-field">
				<label for="online-link">Meeting Link:</label>
				<input
					id="online-link"
					type="url"
					bind:value={localOnlineLink}
					onblur={emitUpdate}
					placeholder="https://zoom.us/j/..."
					class="text-input"
				/>
			</div>
		</div>
	{/if}
</div>

<style>
	.location-editor {
		padding: 1rem;
		background: #f8fafc;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
	}
	
	.editor-title {
		margin: 0 0 1rem 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #1f2937;
	}
	
	.form-field {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
		margin-bottom: 0.75rem;
	}
	
	.form-field label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #475569;
	}
	
	.form-row {
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 0.75rem;
	}
	
	.location-type-select,
	.text-input {
		padding: 0.5rem 0.75rem;
		border: 1px solid #cbd5e1;
		border-radius: 6px;
		font-size: 0.875rem;
		color: #1f2937;
		background: white;
		transition: all 0.2s ease;
	}
	
	.location-type-select:focus,
	.text-input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}
	
	.address-fields,
	.coordinates-fields,
	.online-fields {
		margin-top: 0.75rem;
	}
</style>

