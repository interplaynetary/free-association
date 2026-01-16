<script lang="ts">
	import { getAllEmojisByCategory, searchEmoji } from 'random-emoji-picker';

	interface Props {
		onSelect: (emoji: string) => void;
	}

	let { onSelect }: Props = $props();

	let selectedCategory = $state(0);
	let searchQuery = $state('');
	let emojiGridRef = $state<HTMLDivElement>();

	// Categories organized for needs/capacities - comprehensive mapping of ALL 51 package categories
	const needsCategories = [
        {
			name: '🛠️ Tools & Equipment',
			packageCategories: ['tech', 'science', 'engineering']
		},
        {
			name: '🎉 Events & Entertainment',
			packageCategories: ['entertainment', 'holidays', 'casino', 'circus']
		},
        {
			name: '🏠 Housing & Workspace',
			packageCategories: ['office'] // workspace, offices, shelters
		},
        {
			name: '🍽️ Food & Nutrition',
			packageCategories: ['food', 'cooking', 'baking']
		},
        {
			name: '🚗 Transportation & Travel',
			packageCategories: ['travel', 'car', 'bike']
		},
		{
			name: '🎨 Creative & Arts',
			packageCategories: ['art', 'music', 'photography', 'hobbies']
		},
        {
			name: '⚽ Sports & Recreation',
			packageCategories: ['sports', 'crime'] // crime might represent competitive/strategy games
		},
		{
			name: '🏥 Health & Wellbeing',
			packageCategories: ['health', 'medical', 'fitness', 'yoga', 'meditation', 'relaxation', 
			                    'selfCare', 'selfLove', 'selfImprovement', 'selfDiscovery', 
			                    'selfExpression', 'selfAwareness', 'selfConfidence']
		},
        {
			name: '🌱 Nature & Environment',
			packageCategories: ['nature', 'animals', 'weather', 'space', 'gardening']
		},
		{
			name: '💼 Services & Skills',
			packageCategories: ['office', 'communication', 'warrior'] // warrior = security/protection services
		},
		{
			name: '📚 Education & Learning',
			packageCategories: ['education', 'student']
		},
		{
			name: '👕 Clothing & Fashion',
			packageCategories: ['fashion']
		},
		{
			name: '💻 Technology & Gaming',
			packageCategories: ['tech', 'gaming']
		},

		{
			name: '💰 Money & Resources',
			packageCategories: ['money']
		},
		{
			name: '😊 Social & Emotional',
			packageCategories: ['happy', 'sad', 'angry', 'love']
		},
		{
			name: '🔮 Cultural & Spiritual',
			packageCategories: ['religion', 'fantasy']
		},
	];

	// Get emojis for our custom categories
	const categoryEmojis = needsCategories.map(cat => {
		const emojis = new Set<string>();
		cat.packageCategories.forEach(pkgCat => {
			const catEmojis = getAllEmojisByCategory(pkgCat);
			catEmojis.forEach(emoji => emojis.add(emoji));
		});
		return {
			name: cat.name,
			emojis: Array.from(emojis)
		};
	});

	// Search results when user is searching
	const searchResults = $derived.by(() => {
		if (!searchQuery.trim()) {
			return null;
		}
		return searchEmoji(searchQuery.trim());
	});

	// Use search results if searching, otherwise use category emojis
	const displayEmojis = $derived.by(() => {
		if (searchResults) {
			return searchResults;
		}
		return categoryEmojis[selectedCategory]?.emojis || [];
	});

	function handleEmojiClick(emoji: string) {
		onSelect(emoji);
	}

	function selectCategory(index: number) {
		selectedCategory = index;
		// Scroll to top of emoji grid when switching categories
		if (emojiGridRef) {
			emojiGridRef.scrollTop = 0;
		}
	}
</script>

<div class="emoji-picker">
	<!-- Search bar -->
	<div class="search-container">
		<input 
			type="text" 
			bind:value={searchQuery}
			placeholder="Search emojis..."
			class="search-input"
		/>
		<span class="search-icon">🔍</span>
	</div>

	<!-- Category tabs -->
	<div class="category-tabs">
		{#each categoryEmojis as category, index}
			<button
				class="category-tab"
				class:active={selectedCategory === index}
				onclick={() => selectCategory(index)}
				title={category.name}
			>
				{category.name.split(' ')[0]}
			</button>
		{/each}
	</div>

	<!-- Emoji grid -->
	<div class="emoji-grid" bind:this={emojiGridRef}>
		{#each displayEmojis as emoji}
			<button
				class="emoji-button"
				onclick={() => handleEmojiClick(emoji)}
				title={emoji}
			>
				{emoji}
			</button>
		{/each}
	</div>
</div>

<style>
	.emoji-picker {
		display: flex;
		flex-direction: column;
		width: 100%;
		height: 100%;
		background: white;
		color: #333;
	}

	.search-container {
		position: relative;
		padding: 12px;
		border-bottom: 1px solid #e5e7eb;
	}

	.search-input {
		width: 100%;
		padding: 8px 40px 8px 12px;
		background: #f9fafb;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		color: #333;
		font-size: 14px;
		outline: none;
	}

	.search-input:focus {
		border-color: #3b82f6;
	}

	.search-input::placeholder {
		color: #9ca3af;
	}

	.search-icon {
		position: absolute;
		right: 24px;
		top: 50%;
		transform: translateY(-50%);
		font-size: 18px;
		pointer-events: none;
	}

	.category-tabs {
		display: flex;
		gap: 2px;
		padding: 8px 12px;
		background: #f9fafb;
		border-bottom: 1px solid #e5e7eb;
		overflow-x: auto;
		scrollbar-width: thin;
		scrollbar-color: #d1d5db transparent;
	}

	.category-tabs::-webkit-scrollbar {
		height: 6px;
	}

	.category-tabs::-webkit-scrollbar-track {
		background: transparent;
	}

	.category-tabs::-webkit-scrollbar-thumb {
		background: #d1d5db;
		border-radius: 3px;
	}

	.category-tab {
		flex-shrink: 0;
		width: 36px;
		height: 36px;
		display: flex;
		align-items: center;
		justify-content: center;
		background: transparent;
		border: none;
		border-radius: 6px;
		font-size: 20px;
		cursor: pointer;
		transition: background 0.15s ease;
		opacity: 0.5;
	}

	.category-tab:hover {
		background: #e5e7eb;
		opacity: 0.8;
	}

	.category-tab.active {
		background: #dbeafe;
		opacity: 1;
	}

	.emoji-grid {
		flex: 1;
		padding: 12px;
		overflow-y: auto;
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(40px, 1fr));
		gap: 4px;
		align-content: start;
		scrollbar-width: thin;
		scrollbar-color: #d1d5db transparent;
	}

	.emoji-grid::-webkit-scrollbar {
		width: 8px;
	}

	.emoji-grid::-webkit-scrollbar-track {
		background: transparent;
	}

	.emoji-grid::-webkit-scrollbar-thumb {
		background: #d1d5db;
		border-radius: 4px;
	}

	.emoji-button {
		width: 100%;
		aspect-ratio: 1;
		display: flex;
		align-items: center;
		justify-content: center;
		background: transparent;
		border: none;
		border-radius: 6px;
		font-size: 28px;
		cursor: pointer;
		transition: all 0.15s ease;
		padding: 0;
	}

	.emoji-button:hover {
		background: #f3f4f6;
		transform: scale(1.1);
	}

	.emoji-button:active {
		transform: scale(0.95);
	}

	/* Mobile responsive */
	@media (max-width: 480px) {
		.emoji-grid {
			grid-template-columns: repeat(auto-fill, minmax(36px, 1fr));
		}

		.emoji-button {
			font-size: 24px;
		}

		.category-tab {
			width: 32px;
			height: 32px;
			font-size: 18px;
		}
	}
</style>
