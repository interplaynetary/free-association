<script lang="ts">
	/**
	 * @component ConfigProposalForm
	 * Form for creating meta-proposals to change game configuration
	 */
	import type { ProposedConfigChanges, AgendaItem } from '../../decider.svelte';
	
	interface Props {
		agenda: AgendaItem[];
		onSubmit: (description: string, changes: ProposedConfigChanges) => void;
		onCancel: () => void;
	}
	
	let { agenda, onSubmit, onCancel }: Props = $props();
	
	let description = $state('');
	let changeType = $state<'global' | 'agenda'>('global');
	let targetAgendaIndex = $state(0);
	
	// Global time window
	let globalTimeWindow = $state<number | undefined>(undefined);
	let globalTimeEnabled = $state(false);
	
	// Global phase times
	let globalPhaseEnabled = $state(false);
	let globalProposing = $state<number | undefined>(undefined);
	let globalChallenging = $state<number | undefined>(undefined);
	let globalCommenting = $state<number | undefined>(undefined);
	let globalSupporting = $state<number | undefined>(undefined);
	
	// Agenda-specific times
	let agendaTimeWindow = $state<number | undefined>(undefined);
	let agendaTimeEnabled = $state(false);
	
	// Agenda-specific phase times
	let agendaPhaseEnabled = $state(false);
	let agendaProposing = $state<number | undefined>(undefined);
	let agendaChallenging = $state<number | undefined>(undefined);
	let agendaCommenting = $state<number | undefined>(undefined);
	let agendaSupporting = $state<number | undefined>(undefined);
	
	function handleSubmit() {
		if (!description.trim()) return;
		
		const changes: ProposedConfigChanges = {};
		
		if (changeType === 'global') {
			if (globalTimeEnabled && globalTimeWindow) {
				changes.timeWindow = globalTimeWindow * 1000; // Convert to ms
			}
			
			if (globalPhaseEnabled) {
				changes.phaseTimeConfig = {
					proposing: globalProposing ? globalProposing * 1000 : undefined,
					challenging: globalChallenging ? globalChallenging * 1000 : undefined,
					commenting: globalCommenting ? globalCommenting * 1000 : undefined,
					supporting: globalSupporting ? globalSupporting * 1000 : undefined,
				};
			}
		} else {
			changes.targetAgendaIndex = targetAgendaIndex;
			
			if (agendaTimeEnabled && agendaTimeWindow) {
				changes.agendaItemTimeWindow = agendaTimeWindow * 1000;
			}
			
			if (agendaPhaseEnabled) {
				changes.agendaItemPhaseConfig = {
					proposing: agendaProposing ? agendaProposing * 1000 : undefined,
					challenging: agendaChallenging ? agendaChallenging * 1000 : undefined,
					commenting: agendaCommenting ? agendaCommenting * 1000 : undefined,
					supporting: agendaSupporting ? agendaSupporting * 1000 : undefined,
				};
			}
		}
		
		onSubmit(description, changes);
	}
	
	const canSubmit = $derived(description.trim().length > 0 && (
		(changeType === 'global' && (globalTimeEnabled || globalPhaseEnabled)) ||
		(changeType === 'agenda' && (agendaTimeEnabled || agendaPhaseEnabled))
	));
</script>

<div class="config-proposal-form">
	<h3>⚙️ Propose Configuration Change</h3>
	<p class="subtitle">Propose changes to how decisions are made</p>
	
	<div class="form-group">
		<label for="description">Description *</label>
		<textarea
			id="description"
			bind:value={description}
			placeholder="Describe the proposed changes and why they're needed..."
			rows="3"
		></textarea>
	</div>
	
	<div class="form-group">
		<label>Change Scope</label>
		<div class="radio-group">
			<label class="radio-option">
				<input type="radio" bind:group={changeType} value="global" />
				<span>Global Settings</span>
			</label>
			<label class="radio-option">
				<input type="radio" bind:group={changeType} value="agenda" />
				<span>Specific Agenda Item</span>
			</label>
		</div>
	</div>
	
	{#if changeType === 'agenda'}
		<div class="form-group">
			<label for="agenda-select">Target Agenda Item</label>
			<select id="agenda-select" bind:value={targetAgendaIndex}>
				{#each agenda as item, index}
					{@const text = typeof item === 'string' ? item : item.text}
					<option value={index}>{index + 1}. {text}</option>
				{/each}
			</select>
		</div>
	{/if}
	
	<div class="config-section">
		<h4>⏱️ Time Window</h4>
		
		{#if changeType === 'global'}
			<label class="checkbox-option">
				<input type="checkbox" bind:checked={globalTimeEnabled} />
				<span>Change global time window</span>
			</label>
			
			{#if globalTimeEnabled}
				<div class="input-with-unit">
					<input 
						type="number" 
						bind:value={globalTimeWindow}
						placeholder="30"
						min="1"
					/>
					<span class="unit">seconds</span>
				</div>
			{/if}
		{:else}
			<label class="checkbox-option">
				<input type="checkbox" bind:checked={agendaTimeEnabled} />
				<span>Change agenda item time window</span>
			</label>
			
			{#if agendaTimeEnabled}
				<div class="input-with-unit">
					<input 
						type="number" 
						bind:value={agendaTimeWindow}
						placeholder="60"
						min="1"
					/>
					<span class="unit">seconds</span>
				</div>
			{/if}
		{/if}
	</div>
	
	<div class="config-section">
		<h4>⚡ Phase Durations</h4>
		
		{#if changeType === 'global'}
			<label class="checkbox-option">
				<input type="checkbox" bind:checked={globalPhaseEnabled} />
				<span>Change global phase durations</span>
			</label>
			
			{#if globalPhaseEnabled}
				<div class="phase-inputs">
					<div class="phase-input">
						<label>📝 Proposing</label>
						<div class="input-with-unit">
							<input type="number" bind:value={globalProposing} placeholder="10" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
					<div class="phase-input">
						<label>⚠️ Challenging</label>
						<div class="input-with-unit">
							<input type="number" bind:value={globalChallenging} placeholder="5" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
					<div class="phase-input">
						<label>💬 Commenting</label>
						<div class="input-with-unit">
							<input type="number" bind:value={globalCommenting} placeholder="10" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
					<div class="phase-input">
						<label>👍 Supporting</label>
						<div class="input-with-unit">
							<input type="number" bind:value={globalSupporting} placeholder="5" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
				</div>
			{/if}
		{:else}
			<label class="checkbox-option">
				<input type="checkbox" bind:checked={agendaPhaseEnabled} />
				<span>Change agenda item phase durations</span>
			</label>
			
			{#if agendaPhaseEnabled}
				<div class="phase-inputs">
					<div class="phase-input">
						<label>📝 Proposing</label>
						<div class="input-with-unit">
							<input type="number" bind:value={agendaProposing} placeholder="10" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
					<div class="phase-input">
						<label>⚠️ Challenging</label>
						<div class="input-with-unit">
							<input type="number" bind:value={agendaChallenging} placeholder="5" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
					<div class="phase-input">
						<label>💬 Commenting</label>
						<div class="input-with-unit">
							<input type="number" bind:value={agendaCommenting} placeholder="10" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
					<div class="phase-input">
						<label>👍 Supporting</label>
						<div class="input-with-unit">
							<input type="number" bind:value={agendaSupporting} placeholder="5" min="1" />
							<span class="unit">sec</span>
						</div>
					</div>
				</div>
			{/if}
		{/if}
	</div>
	
	<div class="form-actions">
		<button class="btn-secondary" onclick={onCancel}>Cancel</button>
		<button class="btn-primary" onclick={handleSubmit} disabled={!canSubmit}>
			Submit Meta-Proposal
		</button>
	</div>
</div>

<style>
	.config-proposal-form {
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
		padding: 1.5rem;
		background: white;
		border-radius: 1rem;
		max-height: 80vh;
		overflow-y: auto;
	}
	
	h3 {
		margin: 0;
		font-size: 1.25rem;
		color: #1f2937;
	}
	
	.subtitle {
		margin: 0;
		font-size: 0.875rem;
		color: #6b7280;
	}
	
	.form-group {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	label {
		font-size: 0.875rem;
		font-weight: 600;
		color: #374151;
	}
	
	textarea, input[type="number"], select {
		padding: 0.75rem;
		border: 1px solid #d1d5db;
		border-radius: 0.5rem;
		font-family: inherit;
		font-size: 0.875rem;
		transition: border-color 0.2s;
	}
	
	textarea:focus, input:focus, select:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
	}
	
	.radio-group {
		display: flex;
		gap: 1rem;
	}
	
	.radio-option {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.75rem 1rem;
		background: #f9fafb;
		border: 2px solid #e5e7eb;
		border-radius: 0.5rem;
		cursor: pointer;
		transition: all 0.2s;
		flex: 1;
	}
	
	.radio-option:has(input:checked) {
		border-color: #667eea;
		background: rgba(102, 126, 234, 0.05);
	}
	
	.radio-option input[type="radio"] {
		margin: 0;
	}
	
	.config-section {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		padding: 1rem;
		background: #f9fafb;
		border-radius: 0.5rem;
	}
	
	h4 {
		margin: 0;
		font-size: 1rem;
		color: #1f2937;
	}
	
	.checkbox-option {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		cursor: pointer;
		font-weight: 500;
	}
	
	.checkbox-option input[type="checkbox"] {
		margin: 0;
		width: 1.125rem;
		height: 1.125rem;
	}
	
	.input-with-unit {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.input-with-unit input {
		flex: 1;
	}
	
	.unit {
		font-size: 0.875rem;
		color: #6b7280;
		font-weight: 500;
	}
	
	.phase-inputs {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
		gap: 0.75rem;
	}
	
	.phase-input {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.phase-input label {
		font-size: 0.75rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		color: #6b7280;
	}
	
	.form-actions {
		display: flex;
		gap: 0.75rem;
		justify-content: flex-end;
		padding-top: 0.5rem;
		border-top: 1px solid #e5e7eb;
	}
	
	button {
		padding: 0.75rem 1.5rem;
		border: none;
		border-radius: 0.5rem;
		font-weight: 600;
		font-size: 0.875rem;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.btn-primary {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
	}
	
	.btn-primary:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	.btn-primary:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	.btn-secondary {
		background: #f3f4f6;
		color: #374151;
	}
	
	.btn-secondary:hover {
		background: #e5e7eb;
	}
</style>

