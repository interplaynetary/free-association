<script lang="ts">
    import { onMount, createEventDispatcher, afterUpdate } from 'svelte';
    import { getColorForUserId } from '../utils/colorUtils';
    import { loadUsers, updateUserProfile, usersMap, onUserMapChange } from '../utils/userUtils';
    import { derived, writable, readable, get } from 'svelte/store';
    
    // Props
    export let title: string = "Select User";
    export let searchPlaceholder: string = "Search users...";
    export let position: { x: number, y: number } = { x: 0, y: 0 };
    export let width: number = 280;
    export let maxHeight: number = 320;
    export let excludeIds: string[] = [];
    export let filterText: string = "";
    export let rootId: string | undefined = undefined;
    export let show: boolean = false;
    
    // Create event dispatcher
    const dispatch = createEventDispatcher<{
        select: { id: string, name: string };
        close: void;
    }>();
    
    // State
    let dropdownContainer: HTMLDivElement;
    let searchInput: HTMLInputElement;
    let resultsContainer: HTMLDivElement;
    let clearSubscriptions: () => void = () => {};
    let initialized: boolean = false;
    
    // Create reactive stores
    const currentFilterTextStore = writable<string>(filterText);
    const usersStore = writable<Array<{id: string, name: string}>>([]);
    const loadingStore = writable<boolean>(true);
    
    // Subscribe to filter text changes
    $: {
        currentFilterTextStore.set(filterText);
    }
    
    // Setup reactive user loading system
    function setupUserLoading() {
        // Ensure we clean up previous subscriptions
        clearSubscriptions();
        
        // Update loading state
        loadingStore.set(true);
        
        // Create options for loadUsers
        const options = {
            filterText: get(currentFilterTextStore),
            excludeIds,
            rootId
        };
        
        // Set up user loading with reactivity
        const updateUsersCallback = (users: Array<{id: string, name: string}>) => {
            usersStore.set(users);
            loadingStore.set(false);
        };
        
        // Start the loading process
        const cleanup = loadUsers(updateUsersCallback, options);
        
        // Subscribe to filter text changes to update results
        const filterSub = currentFilterTextStore.subscribe(newFilter => {
            // Update loading state
            loadingStore.set(true);
            
            // Clean up previous subscription
            cleanup();
            
            // Create new options with updated filter
            const newOptions = {
                filterText: newFilter,
                excludeIds,
                rootId
            };
            
            // Start new loading process
            clearSubscriptions = loadUsers(updateUsersCallback, newOptions);
        });
        
        // Return a cleanup function that handles all subscriptions
        clearSubscriptions = () => {
            cleanup();
            filterSub();
        };
        
        // Mark as initialized
        initialized = true;
    }
    
    // Event handlers
    function handleClose() {
        // Cleanup subscriptions
        clearSubscriptions();
        
        dispatch('close');
        show = false;
    }
    
    function handleSelect(id: string, name: string) {
        dispatch('select', { id, name });
        handleClose();
    }
    
    function updateSearchFilter(text: string) {
        currentFilterTextStore.set(text);
    }
    
    // Initialize the component when shown
    function initialize() {
        if (!initialized && show) {
            setupUserLoading();
            
            // Focus search input
            if (searchInput) {
                setTimeout(() => searchInput.focus(), 50);
            }
            
            adjustPosition();
        }
    }
    
    // Adjust position to stay in viewport
    function adjustPosition() {
        if (!dropdownContainer) return;
        
        const rect = dropdownContainer.getBoundingClientRect();
        const viewportWidth = window.innerWidth;
        const viewportHeight = window.innerHeight;
        
        // Check right edge
        if (position.x + rect.width > viewportWidth - 10) {
            position.x = Math.max(10, viewportWidth - rect.width - 10);
        }
        
        // Check left edge
        if (position.x < 10) {
            position.x = 10;
        }
        
        // Check bottom edge
        if (position.y + rect.height > viewportHeight - 10) {
            // If dropdown would go above the top, position it at the top with padding
            if (position.y - rect.height < 10) {
                position.y = 10;
            } else {
                // Otherwise, position above the click
                position.y = position.y - rect.height - 10;
            }
        }
    }
    
    // Setup click outside listener
    function setupClickOutside() {
        const handleClickOutside = (event: MouseEvent) => {
            if (dropdownContainer && !dropdownContainer.contains(event.target as Node)) {
                handleClose();
            }
        };
        
        document.addEventListener('click', handleClickOutside);
        return () => {
            document.removeEventListener('click', handleClickOutside);
        };
    }
    
    // Lifecycle
    onMount(() => {
        initialize();
        
        // Setup click outside handler
        if (show) {
            return setupClickOutside();
        }
    });
    
    afterUpdate(() => {
        if (show && !initialized) {
            initialize();
        }
    });
    
    // Watch for changes in show prop
    $: if (show && !initialized) {
        initialize();
    }
    
    // Subscribe to changes in position or show props
    $: if (show && position && dropdownContainer) {
        setTimeout(adjustPosition, 0);
    }
    
    // Clean up when component is destroyed
    onMount(() => {
        return () => {
            clearSubscriptions();
        };
    });
</script>

{#if show}
    <div 
        class="dropdown-container" 
        bind:this={dropdownContainer}
        style="
            top: {position.y}px; 
            left: {position.x}px; 
            width: {width}px; 
            max-height: {maxHeight}px;
        "
    >
        <div class="header">
            <input 
                type="text" 
                placeholder={searchPlaceholder}
                bind:this={searchInput}
                bind:value={$currentFilterTextStore}
                on:input={(e) => updateSearchFilter(e.currentTarget.value)}
            />
            <button class="close-button" on:click={handleClose}>×</button>
        </div>
        
        <div class="results" bind:this={resultsContainer}>
            {#if $loadingStore && $usersStore.length === 0}
                <div class="message">Loading users...</div>
            {:else if $usersStore.length === 0}
                <div class="message">
                    {$currentFilterTextStore ? "No matching users found" : "No users available"}
                </div>
            {:else}
                {#each $usersStore as item (item.id)}
                    <div 
                        class="item" 
                        data-id={item.id}
                        on:click={() => handleSelect(item.id, item.name)}
                    >
                        <div 
                            class="color-dot" 
                            style="background-color: {getColorForUserId(item.id)}"
                        ></div>
                        <div class="item-name">{item.name || item.id}</div>
                    </div>
                {/each}
            {/if}
        </div>
    </div>
{/if}

<style>
    .dropdown-container {
        position: fixed;
        background: white;
        border-radius: 8px;
        overflow: hidden;
        display: flex;
        flex-direction: column;
        z-index: 99999;
        box-shadow: 0 6px 16px rgba(0, 0, 0, 0.12), 0 3px 6px rgba(0, 0, 0, 0.08);
        opacity: 0;
        animation: fadeIn 150ms forwards;
    }
    
    @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
    }
    
    .header {
        display: flex;
        align-items: center;
        border-bottom: 1px solid #eee;
        padding: 8px;
        background-color: #f9f9f9;
    }
    
    input {
        width: 100%;
        padding: 8px 12px;
        border: none;
        border-radius: 4px;
        background: #ffffff;
        box-shadow: inset 0 0 0 1px #e0e0e0;
        flex: 1;
        outline: none;
        font-size: 14px;
        color: #333;
    }
    
    .close-button {
        padding: 6px 8px;
        margin-left: 8px;
        cursor: pointer;
        color: #666;
        font-weight: bold;
        font-size: 16px;
        line-height: 1;
        border-radius: 4px;
        background: transparent;
        border: none;
        transition: background-color 0.2s;
    }
    
    .close-button:hover {
        background-color: #f0f0f0;
    }
    
    .results {
        overflow-y: auto;
        overflow-x: hidden;
        flex: 1;
        max-height: calc(var(--max-height, 320px) - 56px);
        scrollbar-width: thin;
        scrollbar-color: #d0d0d0 #f5f5f5;
        -webkit-overflow-scrolling: touch;
    }
    
    .results::-webkit-scrollbar {
        width: 6px;
        height: 6px;
    }
    
    .results::-webkit-scrollbar-track {
        background: #f5f5f5;
    }
    
    .results::-webkit-scrollbar-thumb {
        background-color: #d0d0d0;
        border-radius: 3px;
    }
    
    .message {
        padding: 16px;
        text-align: center;
        color: #888;
        font-size: 13px;
    }
    
    .item {
        padding: 10px 12px;
        cursor: pointer;
        font-size: 14px;
        border-bottom: 1px solid #f0f0f0;
        display: flex;
        align-items: center;
        transition: background 0.2s;
    }
    
    .item:hover {
        background-color: #f5f7fa;
    }
    
    .color-dot {
        width: 10px;
        height: 10px;
        border-radius: 50%;
        margin-right: 8px;
    }
    
    .item-name {
        flex: 1;
        color: #333;
    }
</style>
