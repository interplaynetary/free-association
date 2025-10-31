<script lang="ts">
	import VoiceInput from '$lib/components/VoiceInput.svelte';

	let transcript = $state('');
	let logs = $state<string[]>([]);

	function handleTranscriptChange(newTranscript: string) {
		const timestamp = new Date().toLocaleTimeString();
		logs = [...logs, `[${timestamp}] Transcript updated: ${newTranscript.slice(-50)}...`];
	}
</script>

<div class="demo-container">
	<h1>Voice Input Demo</h1>
	<p class="description">
		This component uses the Web Speech API to convert speech to text. Click "Start" to begin recording.
	</p>

	<div class="demo-section">
		<h2>Editable Voice Input</h2>
		<VoiceInput
			bind:value={transcript}
			placeholder="Start speaking to see your words appear here..."
			lang="en-US"
			onchange={handleTranscriptChange}
			editable={true}
		/>
	</div>

	<div class="demo-section">
		<h2>Read-Only Voice Input</h2>
		<VoiceInput
			placeholder="This version is read-only - you can't edit the text directly"
			lang="en-US"
			editable={false}
		/>
	</div>

	{#if transcript}
		<div class="output-section">
			<h2>Current Transcript</h2>
			<div class="output-box">
				{transcript}
			</div>
			<p class="char-count">
				Character count: {transcript.length}
			</p>
		</div>
	{/if}

	{#if logs.length > 0}
		<div class="logs-section">
			<h2>Event Log</h2>
			<div class="logs-box">
				{#each logs as log}
					<div class="log-entry">{log}</div>
				{/each}
			</div>
		</div>
	{/if}

	<div class="info-section">
		<h2>Browser Compatibility</h2>
		<p>The Web Speech API is supported in:</p>
		<ul>
			<li>Chrome (version 25+) - ⭐ Best support</li>
			<li>Edge (version 79+) - ⭐ Best support</li>
			<li>Safari (version 14.1+) - Good support</li>
			<li>Opera (version 27+) - Good support</li>
		</ul>
		<p class="note">
			Note: Firefox has limited support. For best results, use Chrome or Edge.
		</p>

		<h2>⚠️ Troubleshooting "Network Error"</h2>
		<p>If you see a "network" error, try these solutions:</p>
		<ul>
			<li><strong>Use HTTPS:</strong> Many browsers require HTTPS for speech recognition. If you're on HTTP, the service may fail.</li>
			<li><strong>Check Internet:</strong> The Web Speech API uses cloud services that require an internet connection.</li>
			<li><strong>Allow Microphone:</strong> Make sure you've granted microphone permission in your browser.</li>
			<li><strong>Use Chrome/Edge:</strong> These browsers have the most reliable implementation.</li>
			<li><strong>Check Firewall:</strong> Corporate firewalls may block the speech service.</li>
			<li><strong>Try Incognito:</strong> Extensions may interfere with the API.</li>
		</ul>
		<p class="note">
			💡 The component automatically retries up to 3 times on network errors.
		</p>

		<h2>Features</h2>
		<ul>
			<li>🎤 <strong>Start/Stop Recording</strong> - Control when to capture speech</li>
			<li>⏸️ <strong>Pause/Resume</strong> - Pause recording and resume later</li>
			<li>🗑️ <strong>Clear</strong> - Clear the transcript</li>
			<li>📝 <strong>Live Transcription</strong> - See interim results as you speak</li>
			<li>✏️ <strong>Editable</strong> - Manually edit the transcribed text</li>
			<li>🌍 <strong>Multi-language</strong> - Support for different languages</li>
		</ul>

		<h2>Usage Example</h2>
		<pre class="code-block"><code>{`<script>
  import VoiceInput from '$lib/components/VoiceInput.svelte';
  
  let transcript = $state('');
  
  function handleChange(newTranscript) {
    console.log('New transcript:', newTranscript);
  }
</script>

<VoiceInput
  bind:value={transcript}
  lang="en-US"
  onchange={handleChange}
  editable={true}
/>`}</code></pre>
	</div>
</div>

<style>
	.demo-container {
		max-width: 1200px;
		margin: 0 auto;
		padding: 2rem;
	}

	h1 {
		font-size: 2rem;
		font-weight: bold;
		margin-bottom: 1rem;
		color: #1f2937;
	}

	h2 {
		font-size: 1.5rem;
		font-weight: 600;
		margin-bottom: 1rem;
		color: #374151;
	}

	.description {
		font-size: 1.125rem;
		color: #6b7280;
		margin-bottom: 2rem;
		line-height: 1.75;
	}

	.demo-section {
		margin-bottom: 3rem;
		padding: 1.5rem;
		background-color: #f9fafb;
		border-radius: 0.75rem;
		border: 1px solid #e5e7eb;
	}

	.output-section,
	.logs-section,
	.info-section {
		margin-bottom: 2rem;
	}

	.output-box {
		padding: 1rem;
		background-color: white;
		border: 2px solid #e5e7eb;
		border-radius: 0.5rem;
		min-height: 100px;
		white-space: pre-wrap;
		word-wrap: break-word;
		font-family: 'Fira Mono', monospace;
		font-size: 0.875rem;
		line-height: 1.5;
	}

	.char-count {
		margin-top: 0.5rem;
		font-size: 0.875rem;
		color: #6b7280;
	}

	.logs-box {
		padding: 1rem;
		background-color: #1f2937;
		border-radius: 0.5rem;
		max-height: 300px;
		overflow-y: auto;
		font-family: 'Fira Mono', monospace;
		font-size: 0.75rem;
	}

	.log-entry {
		color: #10b981;
		padding: 0.25rem 0;
		border-bottom: 1px solid #374151;
	}

	.log-entry:last-child {
		border-bottom: none;
	}

	.info-section ul {
		list-style-type: disc;
		margin-left: 1.5rem;
		margin-bottom: 1rem;
		color: #4b5563;
	}

	.info-section li {
		margin-bottom: 0.5rem;
		line-height: 1.5;
	}

	.note {
		font-size: 0.875rem;
		color: #6b7280;
		font-style: italic;
		margin-top: 0.5rem;
	}

	.code-block {
		background-color: #1f2937;
		border-radius: 0.5rem;
		padding: 1rem;
		overflow-x: auto;
		margin-top: 1rem;
	}

	.code-block code {
		color: #10b981;
		font-family: 'Fira Mono', monospace;
		font-size: 0.875rem;
		line-height: 1.5;
	}

	/* Mobile responsive */
	@media (max-width: 640px) {
		.demo-container {
			padding: 1rem;
		}

		h1 {
			font-size: 1.5rem;
		}

		h2 {
			font-size: 1.25rem;
		}

		.description {
			font-size: 1rem;
		}

		.demo-section {
			padding: 1rem;
		}

		.code-block {
			font-size: 0.75rem;
		}
	}
</style>

