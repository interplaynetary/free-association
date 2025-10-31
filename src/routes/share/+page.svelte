<script lang="ts">
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import { page } from '$app/stores';
	import toast from 'svelte-french-toast';
	import {
		processSharedData,
		initializeDefaultHandlers,
		type ShareData
	} from '$lib/utils/share-handler';

	let sharedTitle = $state('');
	let sharedText = $state('');
	let sharedUrl = $state('');
	let processing = $state(true);
	let handlerName = $state('');
	let handlerFound = $state(false);

	onMount(async () => {
		// Initialize default handlers
		initializeDefaultHandlers();

		// Get shared data from URL params (POST data gets converted to GET params)
		const params = $page.url.searchParams;
		sharedTitle = params.get('title') || '';
		sharedText = params.get('text') || '';
		sharedUrl = params.get('url') || '';

		console.log('[Share Target] Received:', { sharedTitle, sharedText, sharedUrl });

		const shareData: ShareData = {
			title: sharedTitle,
			text: sharedText,
			url: sharedUrl,
			timestamp: Date.now()
		};

		// Process through the generic handler system
		const result = await processSharedData(shareData, {
			navigate: goto,
			toast: (msg: string) => toast.success(msg, { duration: 2000 })
		});

		handlerFound = result.handled;
		handlerName = result.handlerName || 'none';
		processing = false;

		// Redirect handled by the handler itself
		if (!result.handled) {
			// Fallback: redirect to home after delay
			setTimeout(() => {
				goto('/');
			}, 2000);
		}
	});
</script>

<div class="share-handler">
	<div class="content">
		{#if processing}
			<div class="spinner"></div>
			<h1>Processing shared content...</h1>
		{:else}
			<div class="success">✓</div>
			<h1>Content received!</h1>
			
			{#if handlerFound}
				<p class="handler-info">📦 Handled by: <strong>{handlerName}</strong></p>
			{/if}
			
			{#if sharedTitle}
				<p><strong>Title:</strong> {sharedTitle}</p>
			{/if}
			
			{#if sharedText && sharedText.length < 200}
				<p><strong>Text:</strong> {sharedText}</p>
			{:else if sharedText}
				<p><strong>Text:</strong> {sharedText.slice(0, 200)}...</p>
			{/if}
			
			{#if sharedUrl}
				<p><strong>URL:</strong> <a href={sharedUrl} target="_blank" rel="noopener noreferrer">{sharedUrl}</a></p>
			{/if}
			
			<p class="redirect-notice">Redirecting...</p>
		{/if}
	</div>
</div>

<style>
	.share-handler {
		display: flex;
		align-items: center;
		justify-content: center;
		min-height: 100vh;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		padding: 20px;
	}

	.content {
		background: white;
		border-radius: 16px;
		padding: 40px;
		max-width: 500px;
		width: 100%;
		box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
		text-align: center;
	}

	h1 {
		font-size: 24px;
		margin: 20px 0;
		color: #333;
	}

	p {
		margin: 12px 0;
		color: #666;
		word-break: break-word;
	}

	p strong {
		color: #333;
	}

	a {
		color: #667eea;
		text-decoration: none;
	}

	a:hover {
		text-decoration: underline;
	}

	.spinner {
		width: 50px;
		height: 50px;
		border: 4px solid #f3f3f3;
		border-top: 4px solid #667eea;
		border-radius: 50%;
		animation: spin 1s linear infinite;
		margin: 0 auto;
	}

	.success {
		width: 60px;
		height: 60px;
		border-radius: 50%;
		background: #10b981;
		color: white;
		font-size: 36px;
		line-height: 60px;
		margin: 0 auto;
	}

	.redirect-notice {
		margin-top: 30px;
		font-size: 14px;
		color: #999;
		font-style: italic;
	}

	.handler-info {
		background: #f0f9ff;
		border-left: 4px solid #0284c7;
		padding: 12px;
		margin: 20px 0;
		border-radius: 4px;
		font-size: 14px;
		color: #0c4a6e;
	}

	.handler-info strong {
		color: #0369a1;
		font-family: monospace;
	}

	@keyframes spin {
		0% { transform: rotate(0deg); }
		100% { transform: rotate(360deg); }
	}
</style>

