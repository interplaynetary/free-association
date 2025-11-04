<script lang="ts">
	/**
	 * DivisibilityEditor - Slot divisibility constraints
	 * 
	 * Allows setting constraints on how much a slot can be divided:
	 * - max_natural_div: Maximum number of natural divisions (e.g., can't split a person)
	 * - max_percentage_div: Minimum percentage of slot that can be allocated
	 * 
	 * @example
	 * <DivisibilityEditor 
	 *   maxNaturalDiv={4}
	 *   maxPercentageDiv={0.25}
	 *   onUpdate={(natural, percentage) => {...}}
	 * />
	 */
	
	interface Props {
		maxNaturalDiv?: number;
		maxPercentageDiv?: number;
		onUpdate: (maxNaturalDiv?: number, maxPercentageDiv?: number) => void;
	}
	
	let {
		maxNaturalDiv,
		maxPercentageDiv,
		onUpdate
	}: Props = $props();
	
	// Default values: 1 natural division, 1% minimum percentage
	let localMaxNaturalDiv = $state(maxNaturalDiv ?? 1);
	let localMaxPercentageDiv = $state(maxPercentageDiv ?? 0.01);
	
	// Sync with props (but keep defaults if undefined)
	$effect(() => {
		localMaxNaturalDiv = maxNaturalDiv ?? 1;
		localMaxPercentageDiv = maxPercentageDiv ?? 0.01;
	});
	
	function handleNaturalDivChange(e: Event) {
		const value = (e.target as HTMLInputElement).value;
		const numValue = value ? parseInt(value) : undefined;
		localMaxNaturalDiv = numValue;
		onUpdate(numValue, localMaxPercentageDiv);
	}
	
	function handlePercentageDivChange(e: Event) {
		const value = (e.target as HTMLInputElement).value;
		const numValue = value ? parseFloat(value) / 100 : undefined;
		localMaxPercentageDiv = numValue;
		onUpdate(localMaxNaturalDiv, numValue);
	}
	
	// Convert to percentage for display (default 1%)
	const displayPercentage = $derived(() => {
		return Math.round((localMaxPercentageDiv ?? 0.01) * 100);
	});
</script>

<div class="divisibility-editor">
	<h4 class="editor-title">📏 Divisibility Constraints</h4>
	<p class="editor-description">
		Control how this resource can be divided among recipients
	</p>
	
	<div class="constraint-fields">
		<!-- Natural Division -->
		<div class="constraint-field">
			<label for="natural-div">
				Maximum Natural Divisions
				<span class="help-icon" title="How many times can this be split? E.g., a person can't be divided.">ⓘ</span>
			</label>
			<input
				id="natural-div"
				type="number"
				min="1"
				step="1"
				value={localMaxNaturalDiv}
				placeholder="1 (default)"
				oninput={handleNaturalDivChange}
				class="constraint-input"
			/>
			<p class="field-hint">
				Default: 1 (indivisible). Increase to allow splitting resources into multiple parts.
			</p>
		</div>
		
		<!-- Percentage Division -->
		<div class="constraint-field">
			<label for="percentage-div">
				Minimum Allocation Percentage
				<span class="help-icon" title="What's the smallest chunk that can be allocated? Prevents tiny fragments.">ⓘ</span>
			</label>
			<div class="percentage-input-group">
				<input
					id="percentage-div"
					type="range"
					min="1"
					max="100"
					step="1"
					value={displayPercentage()}
					oninput={handlePercentageDivChange}
					class="percentage-slider"
				/>
				<span class="percentage-value">
					{displayPercentage()}%
				</span>
			</div>
			<p class="field-hint">
				Default: 1% (minimum allocation). Increase to prevent tiny fragments (e.g., 10% minimum).
			</p>
		</div>
	</div>
	
	<!-- Preview of constraints (always show with defaults) -->
	<div class="constraints-preview">
		<strong>Active Constraints:</strong>
		<ul>
			<li>Can be divided into <strong>at most {localMaxNaturalDiv}</strong> part{localMaxNaturalDiv > 1 ? 's' : ''}</li>
			<li>Each allocation must be <strong>at least {displayPercentage()}%</strong> of total quantity</li>
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
	
	.percentage-input-group {
		display: flex;
		align-items: center;
		gap: 1rem;
	}
	
	.percentage-slider {
		flex: 1;
		height: 0.5rem;
		border-radius: 0.25rem;
		background: linear-gradient(to right, #fee2e2, #dcfce7);
		outline: none;
		appearance: none;
	}
	
	.percentage-slider::-webkit-slider-thumb {
		appearance: none;
		width: 1.25rem;
		height: 1.25rem;
		border-radius: 50%;
		background: #3b82f6;
		cursor: pointer;
		border: 2px solid white;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
	}
	
	.percentage-slider::-moz-range-thumb {
		width: 1.25rem;
		height: 1.25rem;
		border-radius: 50%;
		background: #3b82f6;
		cursor: pointer;
		border: 2px solid white;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
	}
	
	.percentage-value {
		font-size: 0.875rem;
		font-weight: 600;
		color: #1f2937;
		min-width: 3rem;
		text-align: right;
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

