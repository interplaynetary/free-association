<script lang="ts">
	import {
		filteredContacts,
		contactSearchQuery,
		createContact,
		updateContact,
		deleteContact,
		isPublicKeyInUse,
		isLoadingContacts
	} from '$lib/state/users.svelte';
	import { getUserAlias } from '$lib/state/users.svelte';
	import { getColorForUserId } from '$lib/utils/colorUtils';
	import type { Contact } from '$lib/schema';
	import { t } from '$lib/translations';

	// Component state
	let showCreateForm = $state(false);
	let showEditForm = $state(false);
	let editingContact: Contact | null = $state(null);
	let searchQuery = $state('');

	// Form state
	let formData = $state({
		name: '',
		public_key: ''
	});

	// Reactive updates
	$effect(() => {
		contactSearchQuery.set(searchQuery);
	});

	// Form functions
	function resetForm() {
		formData = {
			name: '',
			public_key: ''
		};
	}

	function openCreateForm() {
		resetForm();
		showCreateForm = true;
		showEditForm = false;
		editingContact = null;
	}

	function openEditForm(contact: Contact) {
		formData = {
			name: contact.name,
			public_key: contact.public_key || ''
		};
		editingContact = contact;
		showEditForm = true;
		showCreateForm = false;
	}

	function closeForm() {
		showCreateForm = false;
		showEditForm = false;
		editingContact = null;
		resetForm();
	}

	function handleSubmit() {
		if (!formData.name.trim()) {
			alert('Name is required');
			return;
		}

		// Check for duplicate public key
		if (formData.public_key && isPublicKeyInUse(formData.public_key, editingContact?.contact_id)) {
			alert('A contact with this public key already exists');
			return;
		}

		try {
			if (editingContact) {
				// Update existing contact
				updateContact(editingContact.contact_id, {
					name: formData.name,
					public_key: formData.public_key || undefined
				});
			} else {
				// Create new contact
				createContact({
					name: formData.name,
					public_key: formData.public_key || undefined
				});
			}
			closeForm();
		} catch (error) {
			console.error('Error saving contact:', error);
			alert('Error saving contact. Please check the data and try again.');
		}
	}

	function handleDelete(contact: Contact) {
		if (confirm(`Are you sure you want to delete ${contact.name}?`)) {
			deleteContact(contact.contact_id);
		}
	}

	function clearSearch() {
		searchQuery = '';
	}

	// Helper to get display name for a contact
	async function getDisplayName(contact: Contact): Promise<string> {
		if (contact.public_key) {
			const gunName = await getUserAlias(contact.public_key);
			return gunName !== contact.public_key ? `${contact.name} (@${gunName})` : contact.name;
		}
		return contact.name;
	}
</script>

<div class="contacts-container">
	<div class="header">
		<h2>{$t('contacts.title')}</h2>
		<button class="add-button" onclick={openCreateForm}>{$t('contacts.add_contact')}</button>
	</div>

	<div class="search-bar">
		<input
			type="text"
			placeholder={$t('contacts.search_contacts')}
			bind:value={searchQuery}
			class="search-input"
		/>
		{#if searchQuery}
			<button class="clear-button" onclick={clearSearch}>×</button>
		{/if}
	</div>

	{#if $isLoadingContacts}
		<div class="loading">{$t('common.loading')}</div>
	{:else}
		<div class="contacts-list">
			{#each $filteredContacts as contact (contact.contact_id)}
				<div class="contact-item">
					<div class="contact-avatar">
						<div class="avatar" style="background-color: {getColorForUserId(contact.contact_id)}">
							{contact.name.charAt(0).toUpperCase()}
						</div>
					</div>
					<div class="contact-info">
						<div class="contact-name">{contact.name}</div>
						{#if contact.public_key}
							{#await getUserAlias(contact.public_key) then gunName}
								{#if gunName !== contact.public_key}
									<div class="contact-alias">@{gunName}</div>
								{/if}
							{/await}
							<div class="contact-key">{contact.public_key.substring(0, 16)}...</div>
						{/if}
					</div>
					<div class="contact-actions">
						<button class="action-button edit" onclick={() => openEditForm(contact)}>{$t('common.edit')}</button>
						<button class="action-button delete" onclick={() => handleDelete(contact)}
							>{$t('common.delete')}</button
						>
					</div>
				</div>
			{/each}
		</div>

		{#if $filteredContacts.length === 0}
			<div class="empty-state">
				{#if searchQuery}
					<p>{$t('contacts.no_contacts')}</p>
					<button class="secondary-button" onclick={clearSearch}>{$t('common.clear')}</button>
				{:else}
					<p>{$t('contacts.no_contacts_yet')}</p>
					<button class="secondary-button" onclick={openCreateForm}>{$t('contacts.add_first_contact')}</button>
				{/if}
			</div>
		{/if}
	{/if}
</div>

<!-- Form Modal -->
{#if showCreateForm || showEditForm}
	<div class="modal-overlay" onclick={closeForm}>
		<div class="modal-content" onclick={(e) => e.stopPropagation()}>
			<div class="modal-header">
				<h3>{editingContact ? $t('contacts.edit_contact') : $t('contacts.add_contact')}</h3>
				<button class="close-button" onclick={closeForm}>×</button>
			</div>

			<form
				onsubmit={(e) => {
					e.preventDefault();
					handleSubmit();
				}}
			>
				<div class="form-field">
					<label for="name">{$t('contacts.contact_name')}</label>
					<input type="text" id="name" bind:value={formData.name} required />
				</div>

				<div class="form-field">
					<label for="public_key">{$t('contacts.public_key')}</label>
					<input type="text" id="public_key" bind:value={formData.public_key} />
				</div>

				<div class="form-actions">
					<button type="button" class="secondary-button" onclick={closeForm}>{$t('common.cancel')}</button>
					<button type="submit" class="primary-button">
						{editingContact ? $t('common.update') : $t('common.create')}
					</button>
				</div>
			</form>
		</div>
	</div>
{/if}

<style>
	.contacts-container {
		padding: 20px;
		max-width: 800px;
		margin: 0 auto;
		font-family:
			system-ui,
			-apple-system,
			sans-serif;
	}

	.header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 24px;
	}

	.header h2 {
		margin: 0;
		font-size: 24px;
		font-weight: 600;
		color: #1f2937;
	}

	.add-button {
		background: #3b82f6;
		color: white;
		border: none;
		padding: 8px 16px;
		border-radius: 6px;
		font-size: 14px;
		font-weight: 500;
		cursor: pointer;
		transition: background 0.2s;
	}

	.add-button:hover {
		background: #2563eb;
	}

	.search-bar {
		position: relative;
		margin-bottom: 20px;
	}

	.search-input {
		width: 100%;
		padding: 12px 16px;
		border: 1px solid #d1d5db;
		border-radius: 8px;
		font-size: 14px;
		background: white;
		transition: border-color 0.2s;
	}

	.search-input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}

	.clear-button {
		position: absolute;
		right: 12px;
		top: 50%;
		transform: translateY(-50%);
		background: none;
		border: none;
		color: #6b7280;
		cursor: pointer;
		font-size: 18px;
		width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
		border-radius: 4px;
		transition: background 0.2s;
	}

	.clear-button:hover {
		background: #f3f4f6;
	}

	.loading {
		text-align: center;
		padding: 40px;
		color: #6b7280;
		font-size: 14px;
	}

	.contacts-list {
		display: flex;
		flex-direction: column;
		gap: 8px;
	}

	.contact-item {
		display: flex;
		align-items: center;
		padding: 16px;
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		transition: all 0.2s;
		cursor: pointer;
	}

	.contact-item:hover {
		border-color: #d1d5db;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
	}

	.contact-avatar {
		margin-right: 12px;
	}

	.avatar {
		width: 40px;
		height: 40px;
		border-radius: 20px;
		display: flex;
		align-items: center;
		justify-content: center;
		color: white;
		font-weight: 600;
		font-size: 16px;
	}

	.contact-info {
		flex: 1;
		min-width: 0;
	}

	.contact-name {
		font-weight: 500;
		color: #1f2937;
		margin-bottom: 2px;
	}

	.contact-alias {
		font-size: 12px;
		color: #6b7280;
		margin-bottom: 2px;
	}

	.contact-key {
		font-size: 11px;
		color: #9ca3af;
		font-family: 'SF Mono', Monaco, monospace;
		background: #f9fafb;
		padding: 2px 6px;
		border-radius: 4px;
		display: inline-block;
	}

	.contact-actions {
		display: flex;
		gap: 8px;
	}

	.action-button {
		padding: 6px 12px;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		font-size: 12px;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
		background: white;
	}

	.action-button.edit {
		color: #3b82f6;
		border-color: #3b82f6;
	}

	.action-button.edit:hover {
		background: #eff6ff;
	}

	.action-button.delete {
		color: #ef4444;
		border-color: #ef4444;
	}

	.action-button.delete:hover {
		background: #fef2f2;
	}

	.empty-state {
		text-align: center;
		padding: 60px 20px;
		color: #6b7280;
	}

	.empty-state p {
		margin-bottom: 16px;
		font-size: 16px;
	}

	.secondary-button {
		background: white;
		color: #374151;
		border: 1px solid #d1d5db;
		padding: 8px 16px;
		border-radius: 6px;
		font-size: 14px;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
	}

	.secondary-button:hover {
		background: #f9fafb;
		border-color: #9ca3af;
	}

	/* Modal Styles */
	.modal-overlay {
		position: fixed;
		top: 0;
		left: 0;
		width: 100%;
		height: 100%;
		background: rgba(0, 0, 0, 0.5);
		display: flex;
		align-items: center;
		justify-content: center;
		z-index: 1000;
		animation: fadeIn 150ms ease-out;
	}

	@keyframes fadeIn {
		from {
			opacity: 0;
		}
		to {
			opacity: 1;
		}
	}

	.modal-content {
		background: white;
		border-radius: 12px;
		width: 90%;
		max-width: 480px;
		max-height: 90vh;
		overflow-y: auto;
		box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);
		animation: slideIn 150ms ease-out;
	}

	@keyframes slideIn {
		from {
			opacity: 0;
			transform: translateY(-20px) scale(0.95);
		}
		to {
			opacity: 1;
			transform: translateY(0) scale(1);
		}
	}

	.modal-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 24px 24px 0 24px;
		margin-bottom: 24px;
	}

	.modal-header h3 {
		margin: 0;
		font-size: 18px;
		font-weight: 600;
		color: #1f2937;
	}

	.close-button {
		background: none;
		border: none;
		font-size: 24px;
		cursor: pointer;
		color: #9ca3af;
		padding: 4px;
		border-radius: 4px;
		transition: all 0.2s;
	}

	.close-button:hover {
		color: #374151;
		background: #f3f4f6;
	}

	form {
		padding: 0 24px 24px 24px;
	}

	.form-field {
		margin-bottom: 20px;
	}

	.form-field label {
		display: block;
		margin-bottom: 6px;
		font-weight: 500;
		color: #374151;
		font-size: 14px;
	}

	.form-field input {
		width: 100%;
		padding: 12px 16px;
		border: 1px solid #d1d5db;
		border-radius: 8px;
		font-size: 14px;
		transition: border-color 0.2s;
		background: white;
	}

	.form-field input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}

	.form-actions {
		display: flex;
		gap: 12px;
		justify-content: flex-end;
		margin-top: 32px;
	}

	.primary-button {
		background: #3b82f6;
		color: white;
		border: none;
		padding: 10px 20px;
		border-radius: 6px;
		font-size: 14px;
		font-weight: 500;
		cursor: pointer;
		transition: background 0.2s;
	}

	.primary-button:hover {
		background: #2563eb;
	}
</style>
