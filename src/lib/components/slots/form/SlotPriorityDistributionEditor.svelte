<script lang="ts">
import { userPubKeys, getUserName } from '$lib/network/users.svelte';
import { slide } from 'svelte/transition';

interface Props {
    priorityDistribution?: Record<string, number>;
    onUpdate: (distribution: Record<string, number>) => void;
    readonly?: boolean;
}

let { priorityDistribution = {}, onUpdate, readonly = false }: Props = $props();

let addMode = $state(false);
let selectedUser = $state('');
let newPriorityPercent = $state(100);

// Derived list of current entries
const entries = $derived(Object.entries(priorityDistribution).map(([pubkey, priority]) => ({
    pubkey,
    priority,
    percent: Math.round(priority * 100)
})));

// Derived list of available users (excluding those already having a priority set)
// Resolving names helps the dropdown
let availableUsers = $derived($userPubKeys.filter(pk => !priorityDistribution[pk]));

function handleUpdate(pubkey: string, newPercent: number) {
    const newDist = { ...priorityDistribution };
    newDist[pubkey] = Math.max(0, Math.min(100, newPercent)) / 100;
    onUpdate(newDist);
}

function handleRemove(pubkey: string) {
    const newDist = { ...priorityDistribution };
    delete newDist[pubkey];
    onUpdate(newDist);
}

function handleAdd() {
    if (!selectedUser) return;
    
    // Default to strict priority (1.0) unless specified
    const priority = Math.max(0, Math.min(100, newPriorityPercent)) / 100;
    
    const newDist = { ...priorityDistribution };
    newDist[selectedUser] = priority;
    
    onUpdate(newDist);
    
    // Reset form
    addMode = false;
    selectedUser = '';
    newPriorityPercent = 100;
}

</script>

<div class="priority-editor">
    <div class="entries">
        {#if entries.length === 0}
            <div class="empty-state">
                No specific priorities set. System defaults to your global recognition weights.
            </div>
        {:else}
            {#each entries as entry (entry.pubkey)}
                <div class="entry" transition:slide|local>
                    <div class="user-info">
                        <span class="name">
                            {#await getUserName(entry.pubkey)}
                                {entry.pubkey.slice(0, 8)}...
                            {:then name} 
                                {name}
                            {:catch}
                                {entry.pubkey.slice(0, 8)}...
                            {/await}
                        </span>
                        <span class="pubkey-hint" title={entry.pubkey}>{entry.pubkey.slice(0, 6)}</span>
                    </div>
                    
                    <div class="controls">
                        {#if readonly}
                            <span class="readonly-value">{entry.percent}%</span>
                        {:else}
                            <input 
                                type="range" 
                                min="0" 
                                max="100" 
                                value={entry.percent} 
                                oninput={(e) => handleUpdate(entry.pubkey, parseInt(e.currentTarget.value))}
                                class="slider"
                            />
                            <span class="value-display">{entry.percent}%</span>
                            
                            <button 
                                type="button" 
                                class="btn-remove"
                                onclick={() => handleRemove(entry.pubkey)}
                                title="Remove / Use Default"
                            >
                                ✕
                            </button>
                        {/if}
                    </div>
                </div>
            {/each}
        {/if}
    </div>

    {#if !readonly}
        {#if addMode}
            <div class="add-form" transition:slide|local>
                <h4>Add Priority Override</h4>
                
                <div class="form-row">
                    <label>
                        User:
                        <select bind:value={selectedUser} class="user-select">
                            <option value="" disabled>Select a user...</option>
                            {#each availableUsers as pubkey}
                                <option value={pubkey}>
                                    {#await getUserName(pubkey) then name}{name} ({pubkey.slice(0,4)}){/await}
                                </option>
                            {/each}
                        </select>
                    </label>
                </div>
                
                <div class="form-row">
                    <label>
                        Priority: 
                        <input type="number" min="0" max="100" bind:value={newPriorityPercent} class="percent-input" /> %
                    </label>
                </div>

                <div class="actions">
                    <button class="btn-cancel" onclick={() => addMode = false}>Cancel</button>
                    <button class="btn-confirm" disabled={!selectedUser} onclick={handleAdd}>Add</button>
                </div>
            </div>
        {:else}
            <button class="btn-add-mode" onclick={() => addMode = true}>
                + Add Priority Override
            </button>
        {/if}
    {/if}
</div>

<style>
    .priority-editor {
        background: #fdfdfd;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 1rem;
        display: flex;
        flex-direction: column;
        gap: 0.5rem;
    }

    .empty-state {
        color: #777;
        font-style: italic;
        font-size: 0.9rem;
        padding: 0.5rem;
        text-align: center;
        background: #fafafa;
        border-radius: 4px;
    }

    .entry {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0.5rem;
        background: white;
        border: 1px solid #eee;
        border-radius: 6px;
        gap: 1rem;
    }

    .user-info {
        display: flex;
        flex-direction: column;
        flex: 1;
        min-width: 0;
    }

    .name {
        font-weight: 500;
        font-size: 0.95rem;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
    }

    .pubkey-hint {
        font-size: 0.75rem;
        color: #999;
        font-family: monospace;
    }

    .controls {
        display: flex;
        align-items: center;
        gap: 0.75rem;
    }

    .slider {
        width: 100px;
    }

    .value-display {
        width: 3ch;
        text-align: right;
        font-variant-numeric: tabular-nums;
        font-weight: 600;
        color: #2563eb;
    }

    .btn-remove {
        background: none;
        border: none;
        color: #999;
        cursor: pointer;
        padding: 0.25rem;
        font-size: 1rem;
        border-radius: 4px;
    }

    .btn-remove:hover {
        background: #fee2e2;
        color: #ef4444;
    }

    .btn-add-mode {
        background: white;
        border: 1px dashed #bbb;
        color: #555;
        padding: 0.5rem;
        border-radius: 6px;
        cursor: pointer;
        font-size: 0.9rem;
        transition: all 0.2s;
        margin-top: 0.5rem;
    }

    .btn-add-mode:hover {
        border-color: #2563eb;
        color: #2563eb;
        background: #f0f7ff;
    }

    .add-form {
        background: #f8fafc;
        padding: 1rem;
        border-radius: 8px;
        border: 1px solid #cbd5e1;
        margin-top: 0.5rem;
    }

    .add-form h4 {
        margin: 0 0 1rem 0;
        font-size: 1rem;
        color: #334155;
    }

    .form-row {
        margin-bottom: 0.75rem;
    }

    .form-row label {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        font-size: 0.9rem;
        font-weight: 500;
        color: #475569;
    }

    .user-select {
        flex: 1;
        padding: 0.4rem;
        border: 1px solid #cbd5e1;
        border-radius: 4px;
    }

    .percent-input {
        width: 60px;
        padding: 0.4rem;
        border: 1px solid #cbd5e1;
        border-radius: 4px;
    }

    .actions {
        display: flex;
        justify-content: flex-end;
        gap: 0.5rem;
        margin-top: 1rem;
    }

    .btn-cancel {
        background: white;
        border: 1px solid #cbd5e1;
        padding: 0.4rem 0.8rem;
        border-radius: 4px;
        cursor: pointer;
    }

    .btn-confirm {
        background: #2563eb;
        color: white;
        border: none;
        padding: 0.4rem 0.8rem;
        border-radius: 4px;
        cursor: pointer;
        font-weight: 500;
    }

    .btn-confirm:disabled {
        background: #94a3b8;
        cursor: not-allowed;
    }
</style>
