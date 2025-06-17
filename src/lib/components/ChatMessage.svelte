<script lang="ts">
	import { gunAvatar } from 'gun-avatar';
	import { getColorForUserId, getContrastTextColor } from '../utils/colorUtils';

	interface Message {
		who: string;
		what: string;
		when: number;
		whopub?: string; // Public key for gun-avatar
	}

	interface ChatMessageProps {
		message: Message;
		sender: string;
	}

	let { message, sender }: ChatMessageProps = $props();

	const messageClass = message.whopub === sender ? 'sent' : 'received';
	const ts = new Date(message.when);

	// Debug logging for whopub
	console.log('ChatMessage - whopub:', message.whopub, 'length:', message.whopub?.length);
	console.log('ChatMessage - is own message:', message.whopub === sender);

	// Generate avatar using gun-avatar if we have a public key, otherwise fallback
	function generateAvatar(whopub?: string, username?: string | any): string {
		if (whopub && whopub.length >= 87) {
			console.log('Using gun-avatar for:', username, 'with pub:', whopub.substring(0, 20) + '...');
			// Use gun-avatar for GUN SEA public keys
			try {
				// @ts-ignore
				return gunAvatar({
					pub: whopub, 					// @ts-ignore

					size: 32,
					round: true,
					draw: 'circles'
				});
			} catch (error) {
				console.warn('Failed to generate gun-avatar:', error);
			}
		} else {
			console.log('Fallback avatar for:', username, 'whopub length:', whopub?.length);
		}

		// Fallback to simple color-based avatar if no valid public key
		// Ensure username is a string
		const name = (typeof username === 'string' ? username : 'Anonymous') || 'Anonymous';
		const hue = Math.abs(name.split('').reduce((a, b) => a + b.charCodeAt(0), 0) % 360);
		const firstChar = name.charAt(0).toUpperCase();

		return `data:image/svg+xml,${encodeURIComponent(`
			<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" viewBox="0 0 32 32">
				<rect width="100%" height="100%" fill="hsl(${hue}, 70%, 60%)" rx="16" ry="16" />
				<text x="50%" y="50%" font-family="Arial" font-size="16px" fill="white" 
					text-anchor="middle" dominant-baseline="middle">
					${firstChar}
				</text>
			</svg>
		`)}`;
	}

	const avatar = generateAvatar(message.whopub, message.who);

	const colorId =
		typeof message.whopub === 'string'
			? message.whopub
			: typeof message.who === 'string'
				? message.who
				: 'Anonymous';
	const userColor = getColorForUserId(colorId);
	const textColor = getContrastTextColor(userColor);

	// Debug logging for color
	console.log('ChatMessage - message.whopub:', message.whopub, 'userColor:', userColor);
</script>

<div class={`message ${messageClass}`}>
	<img src={avatar} alt="avatar" />
	<div class="message-text" style="background-color: {userColor}; color: {textColor};">
		<p>{message.what}</p>
		<time style="color: {textColor}; opacity: 0.8;">{ts.toLocaleTimeString()}</time>
	</div>
</div>

<style>
	.message {
		display: flex;
		align-items: flex-start;
		margin-bottom: 1rem;
		gap: 0.5rem;
		position: relative;
		width: 100%;
		max-width: 100%; /* Ensure message doesn't exceed container */
		box-sizing: border-box;
	}

	.message.sent {
		flex-direction: row-reverse;
	}

	.message img {
		width: 32px;
		height: 32px;
		border-radius: 50%;
		flex-shrink: 0;
		object-fit: cover;
	}

	.message-text {
		max-width: calc(100% - 42px); /* Account for avatar + gap */
		min-width: 0; /* Allow shrinking */
		padding: 0.5rem;
		border-radius: 8px;
		word-wrap: break-word;
		overflow-wrap: break-word;
		hyphens: auto; /* Allow hyphenation for long words */
		position: relative;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
		border: 1px solid rgba(255, 255, 255, 0.2);
		box-sizing: border-box;
	}

	.message-text p {
		margin: 0 0 0.25rem 0;
		word-wrap: break-word;
		overflow-wrap: break-word;
		hyphens: auto;
		white-space: pre-wrap; /* Preserve line breaks but wrap text */
	}

	.message-text time {
		font-size: 0.75rem;
		display: block;
		word-wrap: break-word;
	}
</style>
