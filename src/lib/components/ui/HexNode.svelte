<script lang="ts">
    import * as h3 from 'h3-js';
    import HexNode from './HexNode.svelte';

    interface Props {
        cell: string;
        allSelected: Set<string>; // Full set context
        parentRes?: number; // Resolution of the parent node (for calculating depth gap)
        onHover?: (cell: string) => void;
        onClick?: (cell: string) => void;
    }

    let { cell, allSelected, parentRes, onHover, onClick }: Props = $props();

    function handleMouseEnter() {
        if(onHover) onHover(cell);
    }
    
    function handleClick(e: MouseEvent) {
        e.stopPropagation(); 
        if(onClick) onClick(cell);
    }
    
    // Calculate visualization metrics
    const myRes = h3.getResolution(cell);
    // If no parentRes (root), gap is 0. Otherwise difference - 1 (direct child = 0 gap)
    // Actually, visual tree: Y position = Resolution * scale.
    // So if I am Res 7 and Parent is Res 5. 
    // I should be physically lower than a Res 6 sibling.
    // Standard visual step per resolution level
    const LEVEL_HEIGHT_PX = 40; 
    const levelDiff = parentRes !== undefined ? (myRes - parentRes) : 0;
    // Extra top margin to push me down to my "global level"
    // We already have some standardized padding in the cluster.
    // Let's add extra margin only if we skipped levels.
    // Or just enforce global depth?
    // Relative to parent: I am `levelDiff` UI-steps below parent.
    // Direct child (diff=1) is standard.
    // Sparse child (diff=2) is +LEVEL_HEIGHT_PX.
    const extraTopMargin = Math.max(0, levelDiff - 1) * LEVEL_HEIGHT_PX;
    
    // Generate ticks for intermediate levels
    const skippedLevels = Math.max(0, levelDiff - 1);
    const ticks = Array.from({length: skippedLevels}, (_, i) => i);

    // Optimized Hierarchy Logic:
    let myDirectChildren = $derived(
        Array.from(allSelected).filter(candidate => {
            if (candidate === cell) return false;
            try {
                const candRes = h3.getResolution(candidate);
                if (candRes <= myRes) return false;
                if (h3.cellToParent(candidate, myRes) !== cell) return false;
                
                for (let r = candRes - 1; r > myRes; r--) {
                    const intermediate = h3.cellToParent(candidate, r);
                    if (allSelected.has(intermediate)) {
                        return false; 
                    }
                }
                return true;
            } catch(e) { return false; }
        }).sort((a, b) => {
            const resA = h3.getResolution(a);
            const resB = h3.getResolution(b);
            return resA - resB || a.localeCompare(b);
        })
    );

</script>

<div class="hex-node-container flex flex-col items-center relative transition-all duration-300 ease-in-out" style="margin-top: {extraTopMargin}px">
    <!-- Extension Line for Sparse Hierarchies -->
    {#if skippedLevels > 0}
        <div class="absolute -top-[{extraTopMargin + 10}px] left-1/2 -translate-x-[0.5px] w-px bg-surface-300 dark:bg-surface-600" style="height: {extraTopMargin}px; top: -{extraTopMargin}px;">
             <!-- Ticks for missing levels -->
             {#each ticks as tick}
                <div class="absolute w-2 h-px bg-surface-300 dark:bg-surface-600 -left-1" style="top: {(tick + 1) * LEVEL_HEIGHT_PX}px"></div>
                 <div class="absolute -right-4 text-[8px] text-surface-400 font-mono" style="top: {(tick + 1) * LEVEL_HEIGHT_PX - 5}px">R{parentRes + tick + 1}</div>
             {/each}
        </div>
    {/if}

    <!-- Parent/Self Node -->
    <div class="relative z-10 flex flex-col items-center">
        <button 
            class="hexagon-btn relative group transition-transform hover:scale-110 active:scale-95 m-1"
            onclick={handleClick}
            onmouseenter={handleMouseEnter}
            title="{cell} (Res {myRes})"
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
                    R{myRes}
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
                        <!-- Pass myRes as parentRes to child -->
                        <HexNode cell={child} {allSelected} parentRes={myRes} {onHover} {onClick} />
                    </div>
                {/each}
            </div>
        </div>
    {/if}
</div>
