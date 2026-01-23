<script lang="ts">
	/**
	 * DivisibilityEditor (Constraints)
	 * 
	 * Allows setting generalized resource constraints:
	 * - min_atomic_size: Granularity (Minimum usable chunk)
	 * - max_participation: Fan-In (Max unique agents)
	 * - max_concurrency: Bandwidth (Max simultaneous agents)
	 */
	
	interface Props {
		minAtomicSize?: number;
		maxParticipation?: number;
		maxConcurrency?: number;
		onUpdate: (minAtomicSize?: number, maxParticipation?: number, maxConcurrency?: number) => void;
	}
	
	let {
		minAtomicSize,
		maxParticipation,
		maxConcurrency,
		onUpdate
	}: Props = $props();
	
	let localMinAtomicSize = $state(minAtomicSize);
	let localMaxParticipation = $state(maxParticipation);
	let localMaxConcurrency = $state(maxConcurrency);
	
	$effect(() => {
		localMinAtomicSize = minAtomicSize;
		localMaxParticipation = maxParticipation;
		localMaxConcurrency = maxConcurrency;
	});
	
	function handleUpdate() {
		onUpdate(localMinAtomicSize, localMaxParticipation, localMaxConcurrency);
	}

    function handleNumericInput(e: Event, setter: (val: number | undefined) => void) {
        const val = (e.target as HTMLInputElement).value;
        const num = val ? parseFloat(val) : undefined;
        setter(num);
        handleUpdate();
    }
</script>

<div class="divisibility-editor">
	<h4 class="editor-title">⚙️ Flow Constraints</h4>
	<p class="editor-description">
		Control the Granularity, Fan-In, and Bandwidth of this resource flow.
	</p>
	
	<div class="constraint-fields">
		<!-- Granularity -->
		<div class="constraint-field">
			<label for="atomic-size">
				Min Atomic Size (Granularity)
				<span class="help-icon" title="The smallest divisible unit. E.g., '1 hour shift' or '1 crate'.">ⓘ</span>
			</label>
			<input
				id="atomic-size"
				type="number"
				min="0"
				step="any"
				value={localMinAtomicSize}
				placeholder="0 (No minimum)"
				oninput={(e) => handleNumericInput(e, v => localMinAtomicSize = v)}
				class="constraint-input"
			/>
            <p class="field-hint">Defines the "Packet Size". Anything smaller is rejected.</p>
		</div>
		
		<!-- Fan-In -->
		<div class="constraint-field">
			<label for="max-participation">
				Max Participation (Fan-In)
				<span class="help-icon" title="Maximum number of unique agents allowed to contribute over the lifecycle.">ⓘ</span>
			</label>
			<input
				id="max-participation"
				type="number"
				min="1"
				step="1"
				value={localMaxParticipation}
				placeholder="Unlimited"
				oninput={(e) => handleNumericInput(e, v => localMaxParticipation = v)}
				class="constraint-input"
			/>
            <p class="field-hint">Limits management overhead (e.g. "I can only manage 5 people").</p>
		</div>

        <!-- Bandwidth -->
		<div class="constraint-field">
			<label for="max-concurrency">
				Max Concurrency (Bandwidth)
				<span class="help-icon" title="Maximum number of simultaneous active agents allowed at any moment.">ⓘ</span>
			</label>
			<input
				id="max-concurrency"
				type="number"
				min="1"
				step="1"
				value={localMaxConcurrency}
				placeholder="Unlimited"
				oninput={(e) => handleNumericInput(e, v => localMaxConcurrency = v)}
				class="constraint-input"
			/>
            <p class="field-hint">Physical limit (e.g. "Only 2 chairs available"). 1 = Sequential.</p>
		</div>
	</div>
	
	<!-- Preview -->
	<div class="constraints-preview">
		<strong>Active Configuration:</strong>
		<ul>
            <li>Packet Size: <strong>{localMinAtomicSize ? `>= ${localMinAtomicSize}` : 'Any'}</strong></li>
			<li>Total Roster: <strong>{localMaxParticipation ? `Max ${localMaxParticipation} people` : 'Unlimited'}</strong></li>
			<li>Throughput: <strong>{localMaxConcurrency ? `Max ${localMaxConcurrency} at once` : 'Unlimited'}</strong></li>
		</ul>
	</div>
</div>

<style>
	.divisibility-editor {
		padding: 1rem;
		background: #f8fafc;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
	}
	
	.editor-title {
		margin: 0 0 0.5rem 0;
		font-size: 0.875rem;
		font-weight: 600;
		color: #1f2937;
	}
	
	.editor-description {
		margin: 0 0 1rem 0;
		font-size: 0.75rem;
		color: #64748b;
	}
	
	.constraint-fields {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.constraint-field {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.constraint-field label {
		font-size: 0.75rem;
		font-weight: 600;
		color: #475569;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.help-icon {
		display: inline-flex;
		align-items: center;
		justify-content: center;
		width: 1rem;
		height: 1rem;
		border-radius: 50%;
		background: #e0e7ff;
		color: #4f46e5;
		font-size: 0.7rem;
		cursor: help;
	}
	
	.constraint-input {
		padding: 0.5rem 0.75rem;
		border: 1px solid #cbd5e1;
		border-radius: 6px;
		font-size: 0.875rem;
		color: #1f2937;
		background: white;
		transition: all 0.2s ease;
	}
	
	.constraint-input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
	}
	
	.field-hint {
		margin: 0;
		font-size: 0.7rem;
		color: #94a3b8;
		font-style: italic;
	}
	
	.constraints-preview {
		margin-top: 1rem;
		padding: 0.75rem;
		background: #eff6ff;
		border: 1px solid #bfdbfe;
		border-radius: 6px;
		font-size: 0.75rem;
	}
	
	.constraints-preview strong {
		color: #1e40af;
	}
	
	.constraints-preview ul {
		margin: 0.5rem 0 0 0;
		padding-left: 1.5rem;
	}
	
	.constraints-preview li {
		color: #1e3a8a;
		margin: 0.25rem 0;
	}
</style>

