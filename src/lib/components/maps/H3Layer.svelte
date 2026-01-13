<script lang="ts">
	import * as h3 from 'h3-js';
	import type { Map as MapLibreMap } from 'maplibre-gl';
    import { onMount } from 'svelte';

	interface Props {
		map?: MapLibreMap;
        visible?: boolean;
        onselect?: (cells: string[]) => void;
	}

	let { map, visible = true, onselect }: Props = $props();

    let canvas: HTMLCanvasElement;
    let ctx: CanvasRenderingContext2D | null = null;
    let width = $state(0);
    let height = $state(0);
    // Use a reactive Set approach (or just re-assign array/set for Svelte reactivity)
    // Svelte 5 Set reactivity is fine if we use $state(new Set()) and mutate correctly or reassign?
    // Let's use a simple array or Set wrapped in state.
    let selectedCells = $state(new Set<string>());
    
    // Zoom -> Res mapping
    // Tuned to avoid "too small" hexes
    function getResolution(zoom: number): number {
		if (zoom < 2) return 0;
		if (zoom < 3) return 1;
		if (zoom < 4.5) return 2;
		if (zoom < 6) return 3;
        if (zoom < 7.5) return 4;
        if (zoom < 9) return 5;
        if (zoom < 10.5) return 6;
        if (zoom < 12) return 7;
        if (zoom < 13.5) return 8;
        if (zoom < 15) return 9;
        if (zoom < 16.5) return 10;
        if (zoom < 18) return 11;
        if (zoom < 20) return 12;
		return 13;
    }

    function resize() {
        if (!map || !canvas) return;
        const container = map.getContainer();
        width = container.clientWidth;
        height = container.clientHeight;
        canvas.width = width * window.devicePixelRatio;
        canvas.height = height * window.devicePixelRatio;
        ctx = canvas.getContext('2d');
        if (ctx) ctx.scale(window.devicePixelRatio, window.devicePixelRatio);
        draw();
    }

    // Helper: Check if a point is roughly facing the camera (Globe mode)
    // MapLibre doesn't easily expose "is this point on the back of the globe" via public API for arbitrary points?
    // We can use a spherical distance check from the map center.
    // Visible hemisphere is 90 degrees from center, but perspective camera sees less (~120-150 degree cone depending on zoom?)
    // Actually, on a globe, anything > 90 deg from the sub-camera point is obscured by the curve (ignoring altitude).
    // With altitude, it's slightly less than 90.
    // Let's implement a heuristic: if distance(center, point) > 85 degrees, cull it.
    function isVisibleOnGlobe(lat: number, lng: number, centerLat: number, centerLng: number): boolean {
        // Haversine-ish or just internal angle
        const rad = Math.PI / 180;
        const phi1 = lat * rad;
        const phi2 = centerLat * rad;
        const dPhi = (centerLat - lat) * rad;
        const dLambda = (centerLng - lng) * rad;

        const a = Math.sin(dPhi / 2) * Math.sin(dPhi / 2) +
                  Math.cos(phi1) * Math.cos(phi2) *
                  Math.sin(dLambda / 2) * Math.sin(dLambda / 2);
        const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        const distanceRad = c;
        
        // 1.5 rad is ~85 degrees. 
        // 90 degrees = pi/2 = 1.57.
        // Culling slightly before the edge fades it out nicely.
        return distanceRad < 1.45;
    }

    function handleClick(e: maplibregl.MapMouseEvent) {
        if (!map || !visible) return;
        const zoom = map.getZoom();
        const res = getResolution(zoom);
        const { lat, lng } = e.lngLat;
        
        // Find cell at clicked location
        const cell = h3.latLngToCell(lat, lng, res);
        
        // Toggle selection
        const newSet = new Set(selectedCells);
        if (newSet.has(cell)) {
            newSet.delete(cell);
        } else {
            newSet.add(cell);
        }
        selectedCells = newSet;
        
        if (onselect) onselect(Array.from(selectedCells));
        draw();
    }

    function draw() {
        if (!map || !visible || !ctx) return;
        
        ctx.clearRect(0, 0, width, height);
        
        const zoom = map.getZoom();
        const res = getResolution(zoom);
        const center = map.getCenter();
        
        // Style settings
        ctx.lineJoin = 'round';
        ctx.lineCap = 'round';
        ctx.lineWidth = 1;

        let cells: string[] = [];
        
        // Global logic for very low zoom
        if (res <= 2) {
             if (res === 0) cells = h3.getRes0Cells();
             else {
                 const base = h3.getRes0Cells();
                 cells = base.flatMap(b => h3.cellToChildren(b, res));
             }
        } else {
             // High zoom logic (clamped)
             const bounds = map.getBounds();
             let south = bounds.getSouth();
             let north = bounds.getNorth();
             let west = bounds.getWest();
             let east = bounds.getEast();
             
             const latSpan = north - south;
             const lngSpan = east - west; 
             
             const maxSpan = (180 / Math.pow(2, zoom)) * 15; 
             
             if (latSpan > maxSpan) {
                 north = Math.min(north, center.lat + maxSpan/2);
                 south = Math.max(south, center.lat - maxSpan/2);
             }
             if (lngSpan > maxSpan) {
                 east = Math.min(east, center.lng + maxSpan/2);
                 west = Math.max(west, center.lng - maxSpan/2);
             }
             
             if (south < -85) south = -85;
             if (north > 85) north = 85;

             const polygon = [
                [west, south],
                [west, north],
                [east, north],
                [east, south],
                [west, south]
             ];

             try {
                cells = h3.polygonToCells(polygon, res, true);
             } catch(e) {
                 try {
                     cells = h3.gridDisk(h3.latLngToCell(center.lat, center.lng, res), 50);
                 } catch(e2) {}
             }
        }
        
        if (cells.length > 5000) cells = cells.slice(0, 5000);

        // Draw regular cells
        ctx.strokeStyle = 'rgba(79, 70, 229, 0.4)';
        ctx.fillStyle = 'rgba(79, 70, 229, 0.15)'; 
    
        ctx.beginPath();
        for (const cell of cells) {
            // Skip selected cells in this pass
            if (selectedCells.has(cell)) continue;

            const [lat, lng] = h3.cellToLatLng(cell);
            if (!isVisibleOnGlobe(lat, lng, center.lat, center.lng)) continue;

            const boundary = h3.cellToBoundary(cell, true); 
            let first = true;
            for (const [lng, lat] of boundary) {
                const p = map.project([lng, lat]);
                if (first) {
                    ctx.moveTo(p.x, p.y);
                    first = false;
                } else {
                    ctx.lineTo(p.x, p.y);
                }
            }
        }
        ctx.stroke();
        ctx.fill();

        // Draw Selected Cells (On Top)
        if (selectedCells.size > 0) {
             ctx.strokeStyle = '#ffffff'; // White stroke
             ctx.lineWidth = 3;
             ctx.fillStyle = 'rgba(79, 70, 229, 0.5)'; // Brighter fill
             
             ctx.beginPath();
             for (const cell of selectedCells) {
                const [lat, lng] = h3.cellToLatLng(cell);
                
                // For selected cells, we might want to draw them even if they are technically 'Back facing' if they are close to edge?
                // But generally sticking to visibility rule is safer for globe.
                if (isVisibleOnGlobe(lat, lng, center.lat, center.lng)) {
                    const boundary = h3.cellToBoundary(cell, true);
                    let first = true;
                    for (const [lng, lat] of boundary) {
                        const p = map.project([lng, lat]);
                        if (first) {
                            ctx.moveTo(p.x, p.y);
                            first = false;
                        } else {
                            ctx.lineTo(p.x, p.y);
                        }
                    }
                    ctx.closePath();
                }
             }
             ctx.stroke();
             ctx.fill();
        }
    }

    $effect(() => {
        if (map) {
            resize();
            map.on('move', draw); 
            map.on('resize', resize);
            map.on('zoom', draw); 
            map.on('click', handleClick);
            
            return () => {
                map.off('move', draw);
                map.off('resize', resize);
                map.off('zoom', draw);
                map.off('click', handleClick);
            };
        }
    });
    
    $effect(() => {
        if (visible) draw();
        else if (ctx) ctx.clearRect(0, 0, width, height); 
    });
</script>

<canvas 
    bind:this={canvas} 
    class="pointer-events-none absolute top-0 left-0 z-10 block"
    style="width: {width}px; height: {height}px;"
></canvas>
