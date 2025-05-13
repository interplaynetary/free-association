<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { gun, user } from '$lib/gun/gunSetup';
	import { globalState } from '$lib/global.svelte';
	import Contact from '$lib/components/Contact.svelte';
	import ContactForm from '$lib/components/ContactForm.svelte';
	import {
		type Contact as ContactType,
		addContact,
		updateContact,
		deleteContact,
		subscribeToContacts
	} from '$lib/centralized/contacts';

	// Page state
	let contacts = $state<ContactType[]>([]);
	let loading = $state(true);
	let showForm = $state(false);
	let showEditForm = $state(false);
	let currentContact = $state<ContactType | null>(null);
	let searchTerm = $state('');
	let cleanupSubscription: (() => void) | null = null;

	// Filtered contacts based on search term
	function getFilteredContacts(): ContactType[] {
		if (!searchTerm.trim()) return contacts;

		const term = searchTerm.toLowerCase();
		return contacts.filter(
			(contact) =>
				contact.name.toLowerCase().includes(term) ||
				(contact.email && contact.email.toLowerCase().includes(term)) ||
				(contact.phone && contact.phone.toLowerCase().includes(term)) ||
				(contact.notes && contact.notes.toLowerCase().includes(term))
		);
	}

	// Sort contacts by name
	function getSortedContacts(): ContactType[] {
		const filtered = getFilteredContacts();
		return [...filtered].sort((a, b) => a.name.localeCompare(b.name));
	}

	// Subscribe to contacts
	function subscribeToContactUpdates() {
		if (!user.is?.pub) {
			loading = false;
			globalState.showToast('Please log in to view your contacts', 'warning');
			return;
		}

		try {
			// Clean up existing subscription
			if (cleanupSubscription) {
				cleanupSubscription();
				cleanupSubscription = null;
			}

			// Set loading state
			loading = true;

			// Subscribe to contacts updates
			cleanupSubscription = subscribeToContacts((updatedContacts) => {
				contacts = updatedContacts;
				loading = false;
			});
		} catch (error) {
			console.error('Error subscribing to contacts:', error);
			loading = false;
			globalState.showToast('Failed to load contacts', 'error');
		}
	}

	// Handle adding a new contact
	async function handleAddContact(contact: ContactType) {
		try {
			await addContact({
				name: contact.name,
				email: contact.email,
				phone: contact.phone,
				notes: contact.notes
			});

			showForm = false;
			globalState.showToast('Contact added successfully', 'success');
		} catch (error) {
			console.error('Error adding contact:', error);
			globalState.showToast('Failed to add contact', 'error');
		}
	}

	// Handle editing a contact
	async function handleEditContact(contactId: string) {
		const contact = contacts.find((c) => c.id === contactId);
		if (contact) {
			currentContact = contact;
			showEditForm = true;
		}
	}

	// Handle updating a contact
	async function handleUpdateContact(contact: ContactType) {
		try {
			await updateContact(contact);

			showEditForm = false;
			currentContact = null;
			globalState.showToast('Contact updated successfully', 'success');
		} catch (error) {
			console.error('Error updating contact:', error);
			globalState.showToast('Failed to update contact', 'error');
		}
	}

	// Handle deleting a contact
	async function handleDeleteContact(contactId: string) {
		// Confirm deletion
		if (!confirm('Are you sure you want to delete this contact?')) {
			return;
		}

		try {
			await deleteContact(contactId);
			globalState.showToast('Contact deleted successfully', 'success');
		} catch (error) {
			console.error('Error deleting contact:', error);
			globalState.showToast('Failed to delete contact', 'error');
		}
	}

	// Handle form cancellation
	function handleCancel() {
		showForm = false;
		showEditForm = false;
		currentContact = null;
	}

	// Subscribe to contacts when component mounts
	onMount(() => {
		subscribeToContactUpdates();
	});

	// Clean up subscription when component is destroyed
	onDestroy(() => {
		if (cleanupSubscription) {
			cleanupSubscription();
		}
	});

	// Reactively subscribe to contacts when user changes
	$effect(() => {
		if (user.is?.pub) {
			subscribeToContactUpdates();
		} else {
			contacts = [];
			loading = false;
		}
	});
</script>

<div class="contacts-page">
	<header>
		<h1>Contacts</h1>

		<div class="search-container">
			<div class="search-box">
				<svg
					xmlns="http://www.w3.org/2000/svg"
					viewBox="0 0 24 24"
					width="18"
					height="18"
					fill="none"
					stroke="currentColor"
					stroke-width="2"
					stroke-linecap="round"
					stroke-linejoin="round"
				>
					<circle cx="11" cy="11" r="8"></circle>
					<line x1="21" y1="21" x2="16.65" y2="16.65"></line>
				</svg>
				<input type="text" placeholder="Search contacts..." bind:value={searchTerm} />
				{#if searchTerm}
					<button class="clear-search" on:click={() => (searchTerm = '')} aria-label="Clear search">
						✕
					</button>
				{/if}
			</div>

			<button class="add-contact-button" on:click={() => (showForm = true)}>
				<svg
					xmlns="http://www.w3.org/2000/svg"
					viewBox="0 0 24 24"
					width="20"
					height="20"
					fill="none"
					stroke="currentColor"
					stroke-width="2"
					stroke-linecap="round"
					stroke-linejoin="round"
				>
					<line x1="12" y1="5" x2="12" y2="19"></line>
					<line x1="5" y1="12" x2="19" y2="12"></line>
				</svg>
				Add Contact
			</button>
		</div>
	</header>

	{#if !user.is?.pub}
		<div class="auth-message">
			<p>Please log in to manage your contacts</p>
		</div>
	{:else if loading && contacts.length === 0}
		<div class="loading">
			<div class="spinner"></div>
			<p>Loading contacts...</p>
		</div>
	{:else if contacts.length === 0}
		<div class="empty-state">
			<svg
				xmlns="http://www.w3.org/2000/svg"
				viewBox="0 0 24 24"
				width="64"
				height="64"
				fill="none"
				stroke="currentColor"
				stroke-width="1"
				stroke-linecap="round"
				stroke-linejoin="round"
			>
				<path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"></path>
				<circle cx="9" cy="7" r="4"></circle>
				<path d="M23 21v-2a4 4 0 0 0-3-3.87"></path>
				<path d="M16 3.13a4 4 0 0 1 0 7.75"></path>
			</svg>
			<p>No contacts found</p>
			<button class="add-first-contact" on:click={() => (showForm = true)}
				>Add your first contact</button
			>
		</div>
	{:else}
		<!-- Display contact list -->
		<div class="contacts-list">
			{#if getFilteredContacts().length === 0}
				<div class="no-results">
					<p>No contacts match your search</p>
				</div>
			{:else}
				{#each getSortedContacts() as contact (contact.id)}
					<Contact {contact} onEdit={handleEditContact} onDelete={handleDeleteContact} />
				{/each}
			{/if}
		</div>
	{/if}

	<!-- Add contact form modal -->
	{#if showForm}
		<div class="modal-overlay" on:click={handleCancel}>
			<div class="modal-content" on:click|stopPropagation>
				<ContactForm onSubmit={handleAddContact} onCancel={handleCancel} />
			</div>
		</div>
	{/if}

	<!-- Edit contact form modal -->
	{#if showEditForm && currentContact}
		<div class="modal-overlay" on:click={handleCancel}>
			<div class="modal-content" on:click|stopPropagation>
				<ContactForm
					contact={currentContact}
					isEdit={true}
					onSubmit={handleUpdateContact}
					onCancel={handleCancel}
				/>
			</div>
		</div>
	{/if}
</div>

<style>
	.contacts-page {
		max-width: 800px;
		margin: 0 auto;
		padding: 20px;
	}

	header {
		margin-bottom: 24px;
	}

	h1 {
		font-size: 2rem;
		margin-bottom: 16px;
		color: #333;
	}

	.search-container {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 16px;
		margin-bottom: 16px;
	}

	.search-box {
		position: relative;
		flex: 1;
		max-width: 500px;
		display: flex;
		align-items: center;
	}

	.search-box svg {
		position: absolute;
		left: 12px;
		color: #999;
	}

	.search-box input {
		width: 100%;
		padding: 10px 36px 10px 40px;
		font-size: 14px;
		border: 1px solid #ddd;
		border-radius: 8px;
		background-color: #f5f5f5;
		transition: all 0.2s ease;
	}

	.search-box input:focus {
		outline: none;
		border-color: #2196f3;
		background-color: white;
		box-shadow: 0 0 0 2px rgba(33, 150, 243, 0.2);
	}

	.clear-search {
		position: absolute;
		right: 12px;
		background: none;
		border: none;
		cursor: pointer;
		color: #999;
		font-size: 14px;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 20px;
		height: 20px;
		border-radius: 50%;
	}

	.clear-search:hover {
		background-color: #eee;
		color: #666;
	}

	.add-contact-button {
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 10px 16px;
		background-color: #2196f3;
		color: white;
		border: none;
		border-radius: 8px;
		font-weight: 500;
		cursor: pointer;
		transition: background-color 0.2s ease;
	}

	.add-contact-button:hover {
		background-color: #1976d2;
	}

	.contacts-list {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.loading,
	.empty-state,
	.auth-message,
	.no-results {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		padding: 40px 20px;
		text-align: center;
		background-color: white;
		border-radius: 8px;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		margin-top: 20px;
	}

	.loading p,
	.empty-state p,
	.auth-message p,
	.no-results p {
		margin-top: 16px;
		color: #666;
	}

	.spinner {
		width: 40px;
		height: 40px;
		border: 3px solid rgba(33, 150, 243, 0.3);
		border-radius: 50%;
		border-top-color: #2196f3;
		animation: spin 1s linear infinite;
	}

	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}

	.empty-state svg {
		color: #ccc;
		margin-bottom: 16px;
	}

	.add-first-contact {
		margin-top: 16px;
		padding: 10px 16px;
		background-color: #2196f3;
		color: white;
		border: none;
		border-radius: 8px;
		font-weight: 500;
		cursor: pointer;
		transition: background-color 0.2s ease;
	}

	.add-first-contact:hover {
		background-color: #1976d2;
	}

	.modal-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background-color: rgba(0, 0, 0, 0.5);
		display: flex;
		align-items: center;
		justify-content: center;
		z-index: 1000;
		padding: 20px;
	}

	.modal-content {
		width: 100%;
		max-width: 500px;
		animation: fadeIn 0.3s ease;
	}

	@keyframes fadeIn {
		from {
			opacity: 0;
			transform: translateY(20px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}
</style>
