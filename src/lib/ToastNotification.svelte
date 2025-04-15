<script lang="ts">
    import { onMount, createEventDispatcher } from 'svelte';
    
    // Props
    export let message: string = "";
    export let type: "success" | "warning" = "success";
    export let duration: number = 3000;
    export let show: boolean = false;
    
    // Create event dispatcher
    const dispatch = createEventDispatcher<{
        close: void;
    }>();
    
    // Element reference
    let toastElement: HTMLDivElement;
    let mounted = false;
    
    // Show and hide logic
    onMount(() => {
        mounted = true;
        if (show) {
            showToast();
        }
    });
    
    function showToast() {
        if (!toastElement || !mounted) return;
        
        // Small delay to let the browser render the element first
        setTimeout(() => {
            if (toastElement) {
                toastElement.style.opacity = "1";
                
                // Auto-hide after duration
                setTimeout(() => {
                    hideToast();
                }, duration);
            }
        }, 10);
    }
    
    function hideToast() {
        if (!toastElement || !mounted) return;
        
        toastElement.style.opacity = "0";
        toastElement.addEventListener('transitionend', () => {
            dispatch('close');
        }, { once: true });
    }
    
    // Watch for changes in the show prop
    $: if (show && toastElement && mounted) {
        showToast();
    } else if (!show && toastElement && mounted) {
        hideToast();
    }
</script>

<div 
    bind:this={toastElement}
    class="toast-message {type === 'warning' ? 'toast-warning' : 'toast-success'}"
    role="alert"
    aria-live="assertive"
>
    {message}
</div>

<style>
    .toast-message {
        position: fixed;
        bottom: 20px;
        left: 50%;
        transform: translateX(-50%);
        color: white;
        padding: 10px 20px;
        border-radius: 8px;
        font-weight: 500;
        z-index: 10000;
        opacity: 0;
        transition: opacity 0.3s;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        pointer-events: none;
    }
    
    .toast-success {
        background-color: #2ecc40;
    }
    
    .toast-warning {
        background-color: #ff4136;
    }
</style> 