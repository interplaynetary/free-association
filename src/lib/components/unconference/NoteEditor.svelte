<script lang="ts">
	let { 
		onClose = () => {},
		onSave = (content: string, color: string) => {}
	} = $props<{
		onClose: () => void;
		onSave: (content: string, color: string) => void;
	}>();

	let content = $state('');
	let selectedColor = $state('#FFEAA7');
	
	const colors = [
		'#FFEAA7', // Yellow
		'#FFB6C1', // Pink
		'#98D8C8', // Mint
		'#AED6F1', // Blue
		'#D5A6BD', // Purple
		'#F9E79F', // Light Yellow
		'#ABEBC6', // Green
		'#F5B7B1', // Coral
	];

	function handleSave() {
		if (content.trim()) {
			onSave(content.trim(), selectedColor);
			content = '';
		}
	}

	function handleKeyDown(e: KeyboardEvent) {
		if (e.key === 'Enter' && e.ctrlKey) {
			e.preventDefault();
			handleSave();
		} else if (e.key === 'Escape') {
			onClose();
		}
	}
</script>

<div class="modal-overlay" role="button" tabindex="0" onclick={onClose} onkeydown={(e) => { if (e.key === 'Escape' || e.key === 'Enter' || e.key === ' ') { e.preventDefault(); onClose(); } }}>
	<div class="modal-content" role="dialog" aria-modal="true" aria-labelledby="note-editor-title" onpointerdown={(e) => e.stopPropagation()}>
		<div class="modal-header">
			<h3 id="note-editor-title">Create Session Idea</h3>
			<button class="btn-close" onclick={onClose}>✕</button>
		</div>

		<div class="modal-body">
			<label class="input-label">
				Session Description
				<textarea
					bind:value={content}
					onkeydown={handleKeyDown}
					placeholder="Describe your session idea..."
					class="content-input"
					></textarea>
			</label>

			<div class="color-picker">
        <div class="input-label">Note Color</div>
				<div class="color-options">
					{#each colors as color}
						<button
							class="color-option"
							class:selected={selectedColor === color}
							style:background-color={color}
							onclick={() => selectedColor = color}
							aria-label="Select color {color}"></button>
					{/each}
				</div>
			</div>

			<div class="preview-container">
        <div class="input-label">Preview</div>
				<div 
					class="note-preview"
					style:background-color={selectedColor}
				>
					{content || 'Your session idea will appear here...'}
				</div>
			</div>
		</div>

		<div class="modal-footer">
			<button class="btn-cancel" onclick={onClose}>
				Cancel
			</button>
			<button 
				class="btn-save"
				onclick={handleSave}
				disabled={!content.trim()}
			>
				Create Note
			</button>
		</div>
	</div>
</div>

<style>
	.modal-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.5);
		display: flex;
		align-items: center;
		justify-content: center;
		z-index: 1000;
		animation: fadeIn 0.2s ease;
	}

	.modal-content {
		background: white;
		border-radius: 0.5rem;
		width: 90%;
		max-width: 500px;
		max-height: 90vh;
		overflow: auto;
		box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
		animation: slideUp 0.3s ease;
	}

	.modal-header {
		padding: 1rem;
		border-bottom: 1px solid #e0e0e0;
		display: flex;
		justify-content: space-between;
		align-items: center;
	}

	.modal-header h3 {
		margin: 0;
		font-size: 1.25rem;
		color: #333;
	}

	.btn-close {
		background: transparent;
		border: none;
		font-size: 1.5rem;
		color: #999;
		cursor: pointer;
		padding: 0;
		width: 2rem;
		height: 2rem;
		display: flex;
		align-items: center;
		justify-content: center;
		border-radius: 0.25rem;
		transition: all 0.2s;
	}

	.btn-close:hover {
		background: #f0f0f0;
		color: #333;
	}

	.modal-body {
		padding: 1.5rem;
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
	}

	.input-label {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
		font-size: 0.875rem;
		font-weight: 500;
		color: #555;
	}

	.content-input {
		padding: 0.75rem;
		border: 1px solid #ddd;
		border-radius: 0.375rem;
		font-size: 0.95rem;
		resize: vertical;
		min-height: 100px;
		font-family: inherit;
		transition: border-color 0.2s;
	}

	.content-input:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
	}

	.color-picker {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.color-options {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}

	.color-option {
		width: 2.5rem;
		height: 2.5rem;
		border: 2px solid transparent;
		border-radius: 0.375rem;
		cursor: pointer;
		transition: all 0.2s;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
	}

	.color-option:hover {
		transform: scale(1.1);
		box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
	}

	.color-option.selected {
		border-color: #333;
		transform: scale(1.1);
		box-shadow: 0 0 0 3px rgba(0, 0, 0, 0.1);
	}

	.preview-container {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.note-preview {
		padding: 1rem;
		border-radius: 0.375rem;
		min-height: 80px;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		font-size: 0.875rem;
		line-height: 1.4;
		color: #333;
		transform: rotate(-1deg);
		font-family: 'Comic Sans MS', cursive, sans-serif;
	}

	.modal-footer {
		padding: 1rem;
		border-top: 1px solid #e0e0e0;
		display: flex;
		justify-content: flex-end;
		gap: 0.75rem;
	}

	.btn-cancel,
	.btn-save {
		padding: 0.5rem 1.25rem;
		border-radius: 0.375rem;
		font-size: 0.875rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
		border: none;
	}

	.btn-cancel {
		background: white;
		color: #666;
		border: 1px solid #ddd;
	}

	.btn-cancel:hover {
		background: #f8f9fa;
		border-color: #999;
	}

	.btn-save {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		box-shadow: 0 2px 4px rgba(102, 126, 234, 0.2);
	}

	.btn-save:hover:not(:disabled) {
		transform: translateY(-1px);
		box-shadow: 0 4px 8px rgba(102, 126, 234, 0.3);
	}

	.btn-save:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	@keyframes fadeIn {
		from {
			opacity: 0;
		}
		to {
			opacity: 1;
		}
	}

	@keyframes slideUp {
		from {
			transform: translateY(20px);
			opacity: 0;
		}
		to {
			transform: translateY(0);
			opacity: 1;
		}
	}

	@media (max-width: 640px) {
		.modal-content {
			width: 95%;
			max-height: 95vh;
		}

		.color-option {
			width: 2rem;
			height: 2rem;
		}
	}
</style>
