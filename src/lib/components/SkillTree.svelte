<script lang="ts">
	import { SKILL_TREE, type SkillNode } from '$lib/protocol/skills';
	import { fade, scale } from 'svelte/transition';
    import { globalState } from '$lib/global.svelte';

	interface Props {
		selectedSkills: Set<string>;
		onSelectionChange: (skills: Set<string>) => void;
		onContinue: () => void;
	}

	let { selectedSkills, onSelectionChange, onContinue }: Props = $props();

    // Derived search query from global state
    const filterQuery = $derived(globalState.skillsSearchQuery.toLowerCase().trim());

    // Recursive search matcher
    // Returns true if:
    // 1. The node itself matches the query
    // 2. OR any of its children match the query
    // 3. OR the query is empty
    function matchesFilter(node: SkillNode): boolean {
        if (!filterQuery) return true;
        
        // Check self
        if (node.name.toLowerCase().includes(filterQuery)) return true;
        if (node.description?.toLowerCase().includes(filterQuery)) return true;
        
        // Check children recursively
        if (node.children) {
            return node.children.some(child => matchesFilter(child));
        }
        
        return false;
    }

	function toggleSkill(skillId: string) {
		const newSelection = new Set(selectedSkills);
		if (newSelection.has(skillId)) {
			newSelection.delete(skillId);
		} else {
			newSelection.add(skillId);
		}
		onSelectionChange(newSelection);
	}

	function isSelected(id: string) {
		return selectedSkills.has(id);
	}
</script>

<div class="skill-tree-container" in:fade>
	<header class="tree-header">
		<h2>Identify Your Skills</h2>
		<p>Select the skills you can offer to the network. This helps us match you with relevant needs.</p>
	</header>

	<div class="skills-grid">
		{#each SKILL_TREE as rootNode}
            {#if matchesFilter(rootNode)}
			<div class="skill-branch">
				<div 
					class="skill-node root-node" 
					class:selected={isSelected(rootNode.id)}
					onclick={() => toggleSkill(rootNode.id)}
                    role="button"
                    tabindex="0"
                    onkeypress={(e) => e.key === 'Enter' && toggleSkill(rootNode.id)}
				>
					<span class="icon">{rootNode.icon}</span>
					<span class="name">{rootNode.name}</span>
				</div>
				
				{#if rootNode.children}
					<div class="children-container">
						{#each rootNode.children as child}
                            {#if matchesFilter(child)}
							<div class="child-branch">
								<div 
									class="skill-node child-node" 
									class:selected={isSelected(child.id)}
									class:locked={!isSelected(rootNode.id) && !isSelected(child.id)}
									onclick={() => isSelected(rootNode.id) && toggleSkill(child.id)}
                                    role="button"
                                    tabindex="0"
                                    onkeypress={(e) => e.key === 'Enter' && isSelected(rootNode.id) && toggleSkill(child.id)}
								>
									<span class="icon">{child.icon}</span>
									<span class="name">{child.name}</span>
								</div>

								{#if child.children}
									<div class="grandchildren-container">
										{#each child.children as grandchild}
                                            {#if matchesFilter(grandchild)}
											<div 
												class="skill-node grandchild-node" 
												class:selected={isSelected(grandchild.id)}
												class:locked={!isSelected(child.id) && !isSelected(grandchild.id)}
												onclick={() => isSelected(child.id) && toggleSkill(grandchild.id)}
                                                role="button"
                                                tabindex="0"
                                                onkeypress={(e) => e.key === 'Enter' && isSelected(child.id) && toggleSkill(grandchild.id)}
											>
												<span class="icon">{grandchild.icon}</span>
												<span class="name">{grandchild.name}</span>
											</div>
                                            {/if}
										{/each}
									</div>
								{/if}
							</div>
                            {/if}
						{/each}
					</div>
				{/if}
			</div>
            {/if}
		{/each}
	</div>
</div>

<style>
	.skill-tree-container {
		display: flex;
		flex-direction: column;
		background: transparent;
		gap: 0.5rem;
		padding: 0.25rem;
	}

	.tree-header {
		text-align: center;
		margin-bottom: 0.5rem;
	}

	.tree-header h2 {
		font-size: 1.1rem;
		font-weight: 700;
		color: #1f2937;
		margin-bottom: 0.1rem;
	}

	.tree-header p {
		font-size: 0.8rem;
		color: #6b7280;
	}

	.skills-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
		gap: 0.5rem;
	}

	.skill-branch {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
		padding: 0.5rem;
		background: rgba(255, 255, 255, 0.8);
		border-radius: 8px;
		box-shadow: 0 2px 4px rgba(0,0,0,0.05);
		border: 1px solid rgba(255,255,255,0.5);
        backdrop-filter: blur(10px);
	}

	.skill-node {
		display: flex;
		align-items: center;
		gap: 0.4rem;
		padding: 0.35rem 0.5rem;
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 6px;
		cursor: pointer;
		transition: all 0.1s ease;
		user-select: none;
        box-shadow: 0 1px 1px rgba(0,0,0,0.05);
        font-size: 0.9rem;
	}

	.skill-node:hover:not(.locked):not(.selected) {
        background-color: #f3f4f6;
        border-color: #d1d5db;
	}

	.skill-node.selected {
        background-color: #e0f2fe; /* Sky 100 */
        border-color: #0ea5e9; /* Sky 500 */
        color: #0c4a6e; /* Sky 900 */
        box-shadow: 0 1px 2px rgba(14, 165, 233, 0.2);
	}
    
    .skill-node.selected:hover {
        background-color: #dbeafe; /* Sky 200 */
    }

	.skill-node.locked {
		opacity: 0.5;
		cursor: default;
		background: #f9fafb;
        color: #9ca3af;
        border-color: #e5e7eb;
        box-shadow: none;
	}
    
    /* Root node styling */
	.root-node {
		font-weight: 700;
		font-size: 0.95rem;
	}

	.children-container {
		margin-left: 0.5rem;
		padding-left: 0.5rem;
		border-left: 1px solid #e5e7eb;
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
        transition: border-color 0.3s ease;
	}

	.grandchildren-container {
		margin-left: 0.75rem;
		padding-left: 0.5rem;
		border-left: 1px solid #e5e7eb;
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
        transition: border-color 0.3s ease;
	}
    
    .child-node.selected + .grandchildren-container,
    .root-node.selected + .children-container {
        border-color: #7dd3fc; /* Sky 300 */
    }
</style>
