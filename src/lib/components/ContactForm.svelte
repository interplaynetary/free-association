<script lang="ts">
	import { onMount } from 'svelte';

	// Define the contact interface
	interface ContactData {
		id: string;
		name: string;
		email?: string;
		phone?: string;
		notes?: string;
	}

	// Props for the form
	let {
		contact = {
			id: '',
			name: '',
			email: '',
			phone: '',
			notes: ''
		},
		isEdit = false,
		onSubmit = (contact: ContactData) => {},
		onCancel = () => {}
	} = $props<{
		contact?: ContactData;
		isEdit?: boolean;
		onSubmit: (contact: ContactData) => void;
		onCancel: () => void;
	}>();

	// Form data
	let formData = $state({
		name: contact.name || '',
		email: contact.email || '',
		phone: contact.phone || '',
		notes: contact.notes || ''
	});

	// Form validation
	let errors = $state({
		name: '',
		email: '',
		phone: ''
	});

	// Focus the name field on mount
	let nameInput = $state<HTMLInputElement | null>(null);

	onMount(() => {
		// Focus on the name input when the component is mounted
		if (nameInput) {
			setTimeout(() => nameInput?.focus(), 50);
		}
	});

	// Validate form data
	function validateForm(): boolean {
		// Reset errors
		errors = {
			name: '',
			email: '',
			phone: ''
		};

		let isValid = true;

		// Validate name (required)
		if (!formData.name.trim()) {
			errors.name = 'Name is required';
			isValid = false;
		}

		// Validate email format (if provided)
		if (formData.email && !isValidEmail(formData.email)) {
			errors.email = 'Please enter a valid email address';
			isValid = false;
		}

		// Validate phone format (if provided)
		if (formData.phone && !isValidPhone(formData.phone)) {
			errors.phone = 'Please enter a valid phone number';
			isValid = false;
		}

		return isValid;
	}

	// Email validation helper
	function isValidEmail(email: string): boolean {
		const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
		return emailRegex.test(email);
	}

	// Phone validation helper (simple version)
	function isValidPhone(phone: string): boolean {
		// Allow digits, spaces, parentheses, hyphens, and plus sign
		const phoneRegex = /^[+\d\s()\-]{7,20}$/;
		return phoneRegex.test(phone);
	}

	// Handle form submission
	function handleSubmit() {
		if (!validateForm()) {
			return;
		}

		const updatedContact: ContactData = {
			id: contact.id,
			name: formData.name.trim(),
			email: formData.email.trim() || undefined,
			phone: formData.phone.trim() || undefined,
			notes: formData.notes.trim() || undefined
		};

		onSubmit(updatedContact);
	}
</script>

<div class="contact-form-container">
	<h2>{isEdit ? 'Edit Contact' : 'Add New Contact'}</h2>

	<form on:submit|preventDefault={handleSubmit}>
		<div class="form-field">
			<label for="name">Name<span class="required">*</span></label>
			<input
				type="text"
				id="name"
				bind:this={nameInput}
				bind:value={formData.name}
				class:error={errors.name}
			/>
			{#if errors.name}
				<div class="error-message">{errors.name}</div>
			{/if}
		</div>

		<div class="form-field">
			<label for="email">Email</label>
			<input type="email" id="email" bind:value={formData.email} class:error={errors.email} />
			{#if errors.email}
				<div class="error-message">{errors.email}</div>
			{/if}
		</div>

		<div class="form-field">
			<label for="phone">Phone</label>
			<input type="tel" id="phone" bind:value={formData.phone} class:error={errors.phone} />
			{#if errors.phone}
				<div class="error-message">{errors.phone}</div>
			{/if}
		</div>

		<div class="form-field">
			<label for="notes">Notes</label>
			<textarea id="notes" bind:value={formData.notes} rows="4"></textarea>
		</div>

		<div class="form-actions">
			<button type="button" class="cancel-button" on:click={onCancel}> Cancel </button>
			<button type="submit" class="submit-button">
				{isEdit ? 'Save Changes' : 'Add Contact'}
			</button>
		</div>
	</form>
</div>

<style>
	.contact-form-container {
		background-color: white;
		border-radius: 8px;
		padding: 20px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
		max-width: 500px;
		margin: 0 auto;
	}

	h2 {
		margin-top: 0;
		margin-bottom: 24px;
		font-size: 1.5rem;
		color: #333;
		text-align: center;
	}

	.form-field {
		margin-bottom: 16px;
	}

	label {
		display: block;
		margin-bottom: 6px;
		font-weight: 500;
		font-size: 14px;
		color: #555;
	}

	.required {
		color: #f44336;
		margin-left: 4px;
	}

	input,
	textarea {
		width: 100%;
		padding: 10px 12px;
		font-size: 14px;
		border: 1px solid #ddd;
		border-radius: 4px;
		box-sizing: border-box;
		transition: border-color 0.2s ease;
	}

	input:focus,
	textarea:focus {
		outline: none;
		border-color: #2196f3;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.2);
	}

	input.error,
	textarea.error {
		border-color: #f44336;
	}

	.error-message {
		color: #f44336;
		font-size: 12px;
		margin-top: 4px;
	}

	.form-actions {
		display: flex;
		justify-content: flex-end;
		gap: 12px;
		margin-top: 24px;
	}

	button {
		padding: 10px 16px;
		border-radius: 4px;
		font-size: 14px;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.cancel-button {
		background-color: transparent;
		border: 1px solid #ddd;
		color: #555;
	}

	.cancel-button:hover {
		background-color: #f5f5f5;
	}

	.submit-button {
		background-color: #2196f3;
		border: none;
		color: white;
	}

	.submit-button:hover {
		background-color: #1976d2;
	}
</style>
