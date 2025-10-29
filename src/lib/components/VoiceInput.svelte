<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { browser } from '$app/environment';

	// Web Speech API type definitions
	interface SpeechRecognition extends EventTarget {
		continuous: boolean;
		interimResults: boolean;
		lang: string;
		maxAlternatives: number;
		start(): void;
		stop(): void;
		abort(): void;
		onerror: ((event: SpeechRecognitionErrorEvent) => void) | null;
		onresult: ((event: SpeechRecognitionEvent) => void) | null;
		onend: (() => void) | null;
	}

	interface SpeechRecognitionEvent extends Event {
		resultIndex: number;
		results: SpeechRecognitionResultList;
	}

	interface SpeechRecognitionResultList {
		length: number;
		item(index: number): SpeechRecognitionResult;
		[index: number]: SpeechRecognitionResult;
	}

	interface SpeechRecognitionResult {
		isFinal: boolean;
		length: number;
		item(index: number): SpeechRecognitionAlternative;
		[index: number]: SpeechRecognitionAlternative;
	}

	interface SpeechRecognitionAlternative {
		transcript: string;
		confidence: number;
	}

	interface SpeechRecognitionErrorEvent extends Event {
		error: string;
		message: string;
	}

	interface VoiceInputProps {
		/** Optional initial text value */
		value?: string;
		/** Placeholder text when no transcription */
		placeholder?: string;
		/** Language code for speech recognition (default: 'en-US') */
		lang?: string;
		/** Enable continuous recognition (default: true) */
		continuous?: boolean;
		/** Show interim results while speaking (default: true) */
		interimResults?: boolean;
		/** Max number of alternative transcriptions (default: 1) */
		maxAlternatives?: number;
		/** Callback when transcript changes */
		onchange?: (transcript: string) => void;
		/** Allow textarea editing (default: true) */
		editable?: boolean;
	}

	let {
		value = $bindable(''),
		placeholder = 'Click the microphone to start recording...',
		lang = 'en-US',
		continuous = true,
		interimResults = true,
		maxAlternatives = 1,
		onchange,
		editable = true
	}: VoiceInputProps = $props();

	// Recognition state
	let recognition: SpeechRecognition | null = null;
	let isRecording = $state(false);
	let isPaused = $state(false);
	let isSupported = $state(false);

	// Transcript state
	let finalTranscript = $state('');
	let interimTranscript = $state('');

	// Combined transcript for display
	let displayTranscript = $derived(
		finalTranscript + (interimTranscript ? `<span class="interim">${interimTranscript}</span>` : '')
	);

	// Error state
	let error = $state<string | null>(null);
	
	// Retry state for network errors
	let retryCount = $state(0);
	let maxRetries = 3;

	onMount(() => {
		if (!browser) return;

		// Check if we're on HTTPS or localhost (required for some browsers)
		const isSecureContext = window.isSecureContext;
		const isLocalhost = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1';
		
		if (!isSecureContext && !isLocalhost) {
			console.warn('Speech Recognition works best on HTTPS. You may experience network errors on HTTP.');
		}

		// Check if Speech Recognition is supported
		const SpeechRecognitionAPI =
			(window as any).SpeechRecognition || (window as any).webkitSpeechRecognition;

		if (!SpeechRecognitionAPI) {
			error = 'Speech Recognition is not supported in this browser.';
			isSupported = false;
			return;
		}

		isSupported = true;

		// Initialize Speech Recognition
		const recognitionInstance = new SpeechRecognitionAPI() as SpeechRecognition;
		recognitionInstance.continuous = continuous;
		recognitionInstance.interimResults = interimResults;
		recognitionInstance.maxAlternatives = maxAlternatives;
		recognitionInstance.lang = lang;

		// Handle results
		recognitionInstance.onresult = (event: SpeechRecognitionEvent) => {
			let interim = '';
			let final = '';

			for (let i = event.resultIndex; i < event.results.length; i++) {
				const transcript = event.results[i][0].transcript;
				if (event.results[i].isFinal) {
					final += transcript + ' ';
				} else {
					interim += transcript;
				}
			}

			if (final) {
				finalTranscript += final;
				value = finalTranscript;
				if (onchange) onchange(finalTranscript);
			}

			interimTranscript = interim;
		};

		// Handle errors
		recognitionInstance.onerror = (event: SpeechRecognitionErrorEvent) => {
			console.error('Speech recognition error:', event.error);
			
			switch (event.error) {
				case 'no-speech':
					error = 'No speech detected. Please try again.';
					// Auto-clear after 5 seconds
					setTimeout(() => {
						error = null;
					}, 5000);
					break;
				case 'audio-capture':
					error = 'No microphone found. Please check your device.';
					isRecording = false;
					isPaused = false;
					break;
				case 'not-allowed':
					error = 'Microphone access denied. Please grant permission in your browser settings.';
					isRecording = false;
					isPaused = false;
					break;
				case 'network':
					// Network errors are common - try to retry
					if (retryCount < maxRetries) {
						retryCount++;
						error = `Connection issue (attempt ${retryCount}/${maxRetries}). Retrying...`;
						
						// Retry after a short delay
						setTimeout(() => {
							if (isRecording && recognition) {
								console.log(`Retrying speech recognition (attempt ${retryCount})`);
								try {
									recognition.start();
									error = null;
								} catch (e) {
									console.error('Retry failed:', e);
								}
							}
						}, 1000);
					} else {
						error = 'Unable to connect to speech service. This may be due to: (1) No internet connection, (2) Not using HTTPS, or (3) Browser firewall/settings. Try using Chrome on HTTPS.';
						isRecording = false;
						isPaused = false;
						retryCount = 0;
					}
					break;
				case 'service-not-allowed':
					error = 'Speech recognition service blocked. Please check browser settings or use HTTPS.';
					isRecording = false;
					isPaused = false;
					break;
				default:
					error = `Error: ${event.error}. Try refreshing the page or using a different browser.`;
					isRecording = false;
					isPaused = false;
			}
		};

		// Handle end of recognition
		recognitionInstance.onend = () => {
			if (isRecording && !isPaused) {
				// Restart if we're still meant to be recording
				recognition?.start();
			} else {
				isRecording = false;
			}
		};

		// Assign to component state
		recognition = recognitionInstance;

		// Initialize with provided value
		if (value) {
			finalTranscript = value;
		}
	});

	onDestroy(() => {
		if (recognition && isRecording) {
			recognition.stop();
		}
	});

	function startRecording() {
		if (!recognition || !isSupported) return;
		
		error = null;
		isPaused = false;
		isRecording = true;
		retryCount = 0; // Reset retry count on new recording
		
		try {
			recognition.start();
		} catch (err) {
			// Already started - this is ok
			if (err instanceof DOMException && err.name === 'InvalidStateError') {
				console.log('Recognition already started');
			} else {
				console.error('Failed to start recognition:', err);
				error = 'Failed to start recording. Please try again.';
				isRecording = false;
			}
		}
	}

	function pauseRecording() {
		if (!recognition) return;
		
		isPaused = true;
		isRecording = false;
		recognition.stop();
	}

	function resumeRecording() {
		if (!recognition) return;
		
		isPaused = false;
		startRecording();
	}

	function stopRecording() {
		if (!recognition) return;
		
		isRecording = false;
		isPaused = false;
		recognition.stop();
		
		// Clear interim results
		interimTranscript = '';
	}

	function clearTranscript() {
		finalTranscript = '';
		interimTranscript = '';
		value = '';
		error = null;
		
		if (onchange) onchange('');
	}

	function handleTextInput(event: Event) {
		if (!editable) return;
		
		const target = event.target as HTMLTextAreaElement;
		finalTranscript = target.value;
		value = target.value;
		
		if (onchange) onchange(target.value);
	}
</script>

<div class="voice-input-container">
	<!-- Controls -->
	<div class="controls-row">
		{#if !isRecording && !isPaused}
			<button
				class="btn btn-primary"
				onclick={startRecording}
				disabled={!isSupported}
				title="Start recording"
			>
				<svg class="icon" fill="currentColor" viewBox="0 0 20 20">
					<path fill-rule="evenodd" d="M7 4a3 3 0 016 0v4a3 3 0 11-6 0V4zm4 10.93A7.001 7.001 0 0017 8a1 1 0 10-2 0A5 5 0 015 8a1 1 0 00-2 0 7.001 7.001 0 006 6.93V17H6a1 1 0 100 2h8a1 1 0 100-2h-3v-2.07z" clip-rule="evenodd" />
				</svg>
				<span>Start</span>
			</button>
		{/if}

		{#if isRecording}
			<button
				class="btn btn-warning"
				onclick={pauseRecording}
				title="Pause recording"
			>
				<svg class="icon" fill="currentColor" viewBox="0 0 20 20">
					<path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zM7 8a1 1 0 012 0v4a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v4a1 1 0 102 0V8a1 1 0 00-1-1z" clip-rule="evenodd" />
				</svg>
				<span>Pause</span>
			</button>
		{/if}

		{#if isPaused}
			<button
				class="btn btn-success"
				onclick={resumeRecording}
				title="Resume recording"
			>
				<svg class="icon" fill="currentColor" viewBox="0 0 20 20">
					<path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM9.555 7.168A1 1 0 008 8v4a1 1 0 001.555.832l3-2a1 1 0 000-1.664l-3-2z" clip-rule="evenodd" />
				</svg>
				<span>Resume</span>
			</button>
		{/if}

		{#if isRecording || isPaused}
			<button
				class="btn btn-secondary"
				onclick={stopRecording}
				title="Stop recording"
			>
				<svg class="icon" fill="currentColor" viewBox="0 0 20 20">
					<path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8 7a1 1 0 00-1 1v4a1 1 0 001 1h4a1 1 0 001-1V8a1 1 0 00-1-1H8z" clip-rule="evenodd" />
				</svg>
				<span>Stop</span>
			</button>
		{/if}

		{#if finalTranscript}
			<button
				class="btn btn-danger"
				onclick={clearTranscript}
				title="Clear transcript"
			>
				<svg class="icon" fill="currentColor" viewBox="0 0 20 20">
					<path fill-rule="evenodd" d="M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z" clip-rule="evenodd" />
				</svg>
				<span>Clear</span>
			</button>
		{/if}
	</div>

	<!-- Status indicator -->
	{#if isRecording}
		<div class="status-indicator recording">
			<span class="pulse"></span>
			<span class="text">Recording...</span>
		</div>
	{:else if isPaused}
		<div class="status-indicator paused">
			<span class="pause-icon">⏸️</span>
			<span class="text">Paused</span>
		</div>
	{/if}

	<!-- Error message -->
	{#if error}
		<div class="error-message">
			<svg class="icon-error" fill="currentColor" viewBox="0 0 20 20">
				<path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd" />
			</svg>
			<span>{error}</span>
		</div>
	{/if}

	<!-- Transcript display -->
	<div class="transcript-container">
		{#if editable}
			<textarea
				class="transcript-textarea"
				value={finalTranscript}
				oninput={handleTextInput}
				placeholder={placeholder}
				rows="10"
			></textarea>
			{#if interimTranscript}
				<div class="interim-overlay">
					{interimTranscript}
				</div>
			{/if}
		{:else}
			<div class="transcript-display">
				{#if finalTranscript || interimTranscript}
					{@html displayTranscript}
				{:else}
					<span class="placeholder">{placeholder}</span>
				{/if}
			</div>
		{/if}
	</div>

	<!-- Browser support warning -->
	{#if !isSupported}
		<div class="warning-message">
			<svg class="icon-warning" fill="currentColor" viewBox="0 0 20 20">
				<path fill-rule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clip-rule="evenodd" />
			</svg>
			<span>Speech Recognition is not supported in your browser. Please try Chrome, Edge, or Safari.</span>
		</div>
	{/if}
</div>

<style>
	.voice-input-container {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		width: 100%;
		max-width: 800px;
		margin: 0 auto;
		padding: 1rem;
	}

	.controls-row {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
		align-items: center;
	}

	.btn {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.625rem 1rem;
		border: none;
		border-radius: 0.5rem;
		font-size: 0.875rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s ease;
		-webkit-tap-highlight-color: transparent;
		touch-action: manipulation;
	}

	.btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.btn:not(:disabled):hover {
		transform: translateY(-1px);
		box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
	}

	.btn:not(:disabled):active {
		transform: translateY(0);
	}

	.btn-primary {
		background-color: #3b82f6;
		color: white;
	}

	.btn-primary:not(:disabled):hover {
		background-color: #2563eb;
	}

	.btn-warning {
		background-color: #f59e0b;
		color: white;
	}

	.btn-warning:not(:disabled):hover {
		background-color: #d97706;
	}

	.btn-success {
		background-color: #10b981;
		color: white;
	}

	.btn-success:not(:disabled):hover {
		background-color: #059669;
	}

	.btn-secondary {
		background-color: #6b7280;
		color: white;
	}

	.btn-secondary:not(:disabled):hover {
		background-color: #4b5563;
	}

	.btn-danger {
		background-color: #ef4444;
		color: white;
	}

	.btn-danger:not(:disabled):hover {
		background-color: #dc2626;
	}

	.icon {
		width: 1.25rem;
		height: 1.25rem;
	}

	.status-indicator {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.5rem 1rem;
		border-radius: 0.5rem;
		font-size: 0.875rem;
		font-weight: 500;
	}

	.status-indicator.recording {
		background-color: #fee2e2;
		color: #991b1b;
	}

	.status-indicator.paused {
		background-color: #fef3c7;
		color: #92400e;
	}

	.pulse {
		width: 0.75rem;
		height: 0.75rem;
		background-color: #ef4444;
		border-radius: 50%;
		animation: pulse 1.5s ease-in-out infinite;
	}

	@keyframes pulse {
		0%, 100% {
			opacity: 1;
		}
		50% {
			opacity: 0.5;
		}
	}

	.pause-icon {
		font-size: 1rem;
	}

	.error-message,
	.warning-message {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.75rem 1rem;
		border-radius: 0.5rem;
		font-size: 0.875rem;
	}

	.error-message {
		background-color: #fee2e2;
		color: #991b1b;
		border: 1px solid #fecaca;
	}

	.warning-message {
		background-color: #fef3c7;
		color: #92400e;
		border: 1px solid #fde68a;
	}

	.icon-error,
	.icon-warning {
		width: 1.25rem;
		height: 1.25rem;
		flex-shrink: 0;
	}

	.transcript-container {
		position: relative;
		width: 100%;
	}

	.transcript-textarea {
		width: 100%;
		padding: 1rem;
		border: 2px solid #e5e7eb;
		border-radius: 0.5rem;
		font-size: 1rem;
		line-height: 1.5;
		resize: vertical;
		transition: border-color 0.2s ease;
		font-family: inherit;
	}

	.transcript-textarea:focus {
		outline: none;
		border-color: #3b82f6;
	}

	.interim-overlay {
		position: absolute;
		bottom: 1rem;
		left: 1rem;
		right: 1rem;
		padding: 0.5rem;
		background-color: rgba(59, 130, 246, 0.1);
		border: 1px dashed #3b82f6;
		border-radius: 0.375rem;
		color: #1e40af;
		font-size: 0.875rem;
		font-style: italic;
		pointer-events: none;
	}

	.transcript-display {
		width: 100%;
		min-height: 200px;
		padding: 1rem;
		border: 2px solid #e5e7eb;
		border-radius: 0.5rem;
		font-size: 1rem;
		line-height: 1.5;
		white-space: pre-wrap;
		word-wrap: break-word;
	}

	.transcript-display :global(.interim) {
		color: #6b7280;
		font-style: italic;
	}

	.placeholder {
		color: #9ca3af;
		font-style: italic;
	}

	/* Mobile responsive adjustments */
	@media (max-width: 640px) {
		.voice-input-container {
			padding: 0.75rem;
		}

		.controls-row {
			gap: 0.375rem;
		}

		.btn {
			padding: 0.5rem 0.75rem;
			font-size: 0.8125rem;
		}

		.btn span {
			display: none;
		}

		.icon {
			width: 1.125rem;
			height: 1.125rem;
		}
	}
</style>

