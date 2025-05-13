<script lang="ts">
	import { getColorForUserId } from '$lib/utils/colorUtils';
	import { globalState } from '$lib/global.svelte';

	// Props for the contact component
	let {
		contact = {
			id: '',
			name: '',
			email: '',
			phone: '',
			notes: ''
		},
		onDelete = (id: string) => {},
		onEdit = (id: string) => {}
	} = $props<{
		contact: {
			id: string;
			name: string;
			email?: string;
			phone?: string;
			notes?: string;
		};
		onDelete: (id: string) => void;
		onEdit: (id: string) => void;
	}>();

	// Local state
	let isExpanded = $state(false);

	function toggleExpand() {
		isExpanded = !isExpanded;
	}
</script>

<div class="contact-card" class:expanded={isExpanded}>
	<div class="contact-header" on:click={toggleExpand}>
		<div class="contact-avatar" style="background-color: {getColorForUserId(contact.id)}">
			{contact.name.substring(0, 1).toUpperCase()}
		</div>
		<div class="contact-name">{contact.name}</div>
		<div class="contact-actions">
			<button
				class="action-button edit"
				on:click={(e) => {
					e.stopPropagation();
					onEdit(contact.id);
				}}
				aria-label="Edit contact"
			>
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
					<path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"></path>
					<path d="M18.5 2.5a2.121 2.121 0 0 1 3 3L12 15l-4 1 1-4 9.5-9.5z"></path>
				</svg>
			</button>
			<button
				class="action-button delete"
				on:click={(e) => {
					e.stopPropagation();
					onDelete(contact.id);
				}}
				aria-label="Delete contact"
			>
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
					<path d="M3 6h18"></path>
					<path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"
					></path>
				</svg>
			</button>
		</div>
	</div>

	{#if isExpanded}
		<div class="contact-details">
			{#if contact.email}
				<div class="detail-item">
					<div class="detail-label">Email:</div>
					<div class="detail-value">
						<a href="mailto:{contact.email}">{contact.email}</a>
					</div>
				</div>
			{/if}

			{#if contact.phone}
				<div class="detail-item">
					<div class="detail-label">Phone:</div>
					<div class="detail-value">
						<a href="tel:{contact.phone}">{contact.phone}</a>
					</div>
				</div>
			{/if}

			{#if contact.notes}
				<div class="detail-item">
					<div class="detail-label">Notes:</div>
					<div class="detail-value notes">{contact.notes}</div>
				</div>
			{/if}
		</div>
	{/if}
</div>

<style>
	.contact-card {
		background-color: white;
		border-radius: 8px;
		margin-bottom: 12px;
		overflow: hidden;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		transition: all 0.3s ease;
	}

	.contact-card:hover {
		box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
	}

	.contact-card.expanded {
		box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
	}

	.contact-header {
		display: flex;
		align-items: center;
		padding: 12px 16px;
		cursor: pointer;
	}

	.contact-avatar {
		width: 40px;
		height: 40px;
		border-radius: 50%;
		display: flex;
		align-items: center;
		justify-content: center;
		font-weight: bold;
		color: white;
		margin-right: 12px;
		flex-shrink: 0;
	}

	.contact-name {
		font-size: 16px;
		font-weight: 500;
		flex: 1;
	}

	.contact-actions {
		display: flex;
		gap: 8px;
	}

	.action-button {
		background: transparent;
		border: none;
		width: 32px;
		height: 32px;
		border-radius: 4px;
		display: flex;
		align-items: center;
		justify-content: center;
		cursor: pointer;
		color: #666;
		transition: all 0.2s ease;
	}

	.action-button:hover {
		background-color: #f0f0f0;
	}

	.action-button.edit:hover {
		color: #2196f3;
	}

	.action-button.delete:hover {
		color: #f44336;
	}

	.contact-details {
		padding: 0 16px 16px 68px;
		border-top: 1px solid #f0f0f0;
		animation: fadeIn 0.3s ease;
	}

	@keyframes fadeIn {
		from {
			opacity: 0;
		}
		to {
			opacity: 1;
		}
	}

	.detail-item {
		margin-top: 12px;
	}

	.detail-label {
		font-size: 12px;
		color: #666;
		margin-bottom: 2px;
	}

	.detail-value {
		font-size: 14px;
	}

	.detail-value a {
		color: #2196f3;
		text-decoration: none;
	}

	.detail-value a:hover {
		text-decoration: underline;
	}

	.notes {
		white-space: pre-line;
		line-height: 1.4;
	}
</style>
