<script lang="ts">
    import * as h3 from 'h3-js';
    import HexNode from './HexNode.svelte';

    interface Props {
        cell: string;
        allSelected: Set<string>; // Full set context
        onHover?: (cell: string) => void;
        onClick?: (cell: string) => void;
    }

    let { cell, allSelected, onHover, onClick }: Props = $props();

    function handleMouseEnter() {
        if(onHover) onHover(cell);
    }
    
    function handleClick(e: MouseEvent) {
        e.stopPropagation(); 
        if(onClick) onClick(cell);
    }
    
    // Optimized Hierarchy Logic:
    // A cell C is a direct child of THIS cell if:
    // 1. C is a descendant of THIS (verified by h3.cellToParent)
    // 2. There is NO intermediate cell I in `allSelected` such that C < I < THIS
    
    let myDirectChildren = $derived(
        Array.from(allSelected).filter(candidate => {
            if (candidate === cell) return false; // Self is not child
            try {
                const myRes = h3.getResolution(cell);
                const candRes = h3.getResolution(candidate);
                
                // Candidate must be higher resolution (smaller cell)
                if (candRes <= myRes) return false;
                
                // Candidate must be a descendant of THIS cell
                if (h3.cellToParent(candidate, myRes) !== cell) return false;
                
                // CRITICAL: Check for intermediate ancestors in the selection set
                // Scan resolutions between myRes and candRes
                for (let r = candRes - 1; r > myRes; r--) {
                    const intermediate = h3.cellToParent(candidate, r);
                    if (allSelected.has(intermediate)) {
                        return false; // Candidate is a child of the intermediate, not ME direct
                    }
                }
                
                return true;
            } catch(e) { return false; }
        }).sort((a, b) => {
            // Sort by resolution then index for stability
            const resA = h3.getResolution(a);
            const resB = h3.getResolution(b);
            return resA - resB || a.localeCompare(b);
        })
    );

</script>

<div class="hex-node-container flex flex-col items-center relative transition-all duration-300 ease-in-out">
    <!-- Parent/Self Node -->
    <div class="relative z-10 flex flex-col items-center">
        <button 
            class="hexagon-btn relative group transition-transform hover:scale-110 active:scale-95 m-1"
            onclick={handleClick}
            onmouseenter={handleMouseEnter}
            title="{cell} (Res {h3.getResolution(cell)})"
        >
            <svg viewBox="0 0 100 115" class="w-14 h-14 drop-shadow-md overflow-visible">
                <path 
                    d="M50 0 L100 28.8 L100 86.6 L50 115.4 L0 86.6 L0 28.8 Z" 
                    class="fill-blue-500 hover:fill-blue-400 stroke-blue-700 hover:stroke-blue-500 stroke-2 transition-colors"
                />
                <!-- Central Text -->
                <text x="50" y="62" text-anchor="middle" class="fill-white font-bold text-[18px] font-mono pointer-events-none select-none drop-shadow-sm">
                    {cell.slice(-4)}
                </text>
                <!-- Resolution Label -->
                <text x="50" y="82" text-anchor="middle" class="fill-blue-100 text-[10px] font-mono pointer-events-none select-none tracking-tighter opacity-80">
                    R{h3.getResolution(cell)}
                </text>
            </svg>
            
            <!-- Connection Dot (if has children) -->
            {#if myDirectChildren.length > 0}
                <div class="absolute -bottom-2 left-1/2 -translate-x-1/2 w-2 h-2 bg-blue-300 rounded-full z-0 ring-2 ring-surface-800/20"></div>
            {/if}
        </button>
    </div>

    <!-- Children Cluster -->
    {#if myDirectChildren.length > 0}
        <div class="relative mt-2 pt-4 px-2 bg-surface-500/5 rounded-3xl border border-surface-500/10">
             <!-- Connector Line -->
            <div class="absolute top-0 left-1/2 -translate-x-1/2 w-px h-4 bg-surface-400"></div>
            
            <div class="flex flex-wrap justify-center gap-2 max-w-[280px]">
                {#each myDirectChildren as child}
                    <div class="relative">
                        <!-- Branch to child? Too messy for grid. Just cluster them. -->
                        <HexNode cell={child} {allSelected} {onHover} {onClick} />
                    </div>
                {/each}
            </div>
        </div>
    {/if}
</div>
