<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import * as d3 from 'd3';
	import { derived } from 'svelte/store';
	
	// Import existing stores and types
	import {
		myCommitmentStore,
		myAllocationStateStore,
		myRecognitionWeightsStore,
		networkCommitments,
		networkAllocationStates,
		getNetworkCommitmentsRecord
	} from './stores.svelte';
	
	import {
		myMutualRecognition,
		myMutualBeneficiaries,
		myNonMutualBeneficiaries,
		mutualProvidersForMe,
		computeAndPublishAllocations,
		publishMyCommitment,
		myPubKey
	} from './algorithm.svelte';
	
	import type { Commitment, TwoTierAllocationState } from './schemas';
	
	// Props (Svelte 5 runes mode)
	let { 
		width = 900, 
		height = 700, 
		resourceType 
	}: { 
		width?: number; 
		height?: number; 
		resourceType?: string;
	} = $props();
	
	// Local state
	let svgRef: SVGSVGElement;
	let selectedEntity: string | null = null;
	let round = $state(0);
	
	// Filter slots by resource type
	function filterSlotsByType<T extends { resource_type?: string }>(slots: T[] | undefined): T[] {
		if (!slots) return [];
		if (!resourceType) return slots;
		return slots.filter(slot => slot.resource_type === resourceType);
	}
	
	// Calculate total capacity from slots
	function getTotalCapacity(commitment: Commitment | undefined): number {
		if (!commitment?.capacity_slots) return 0;
		const filteredSlots = filterSlotsByType(commitment.capacity_slots);
		return filteredSlots.reduce((sum, slot) => sum + slot.quantity, 0);
	}
	
	// Calculate total need from slots
	function getTotalNeed(commitment: Commitment | undefined): number {
		if (!commitment?.need_slots) return 0;
		const filteredSlots = filterSlotsByType(commitment.need_slots);
		return filteredSlots.reduce((sum, slot) => sum + slot.quantity, 0);
	}
	
	// Get mutual recognition value
	function getMR(entityA: string, entityB: string): number {
		const mrValues = $myMutualRecognition;
		if (entityA === $myPubKey) {
			return mrValues[entityB] || 0;
		}
		// For other entities, we'd need their MR values from network
		const entityACommitment = networkCommitments.get(entityA);
		const entityBCommitment = networkCommitments.get(entityB);
		
		if (!entityACommitment?.mr_values || !entityBCommitment?.mr_values) return 0;
		
		const aRecognizesB = entityACommitment.mr_values[entityB] || 0;
		const bRecognizesA = entityBCommitment.mr_values[entityA] || 0;
		return Math.min(aRecognizesB, bRecognizesA);
	}
	
	// Get all entities to visualize
	const entities = derived(
		[myCommitmentStore, myMutualRecognition],
		([$myCommitment, $myMR]) => {
			const result = new Map<string, {
				pubKey: string;
				name: string;
				commitment: Commitment;
				capacity: number;
				need: number;
				x: number;
				y: number;
			}>();
			
			// Add me
			if ($myPubKey && $myCommitment) {
				result.set($myPubKey, {
					pubKey: $myPubKey,
					name: 'Me',
					commitment: $myCommitment,
					capacity: getTotalCapacity($myCommitment),
					need: getTotalNeed($myCommitment),
					x: width / 2,
					y: height / 2
				});
			}
			
			// Add network participants
			const commitments = getNetworkCommitmentsRecord();
			const pubKeys = Object.keys(commitments);
			const angleStep = (2 * Math.PI) / Math.max(pubKeys.length, 1);
			const radius = Math.min(width, height) * 0.35;
			
			pubKeys.forEach((pubKey, index) => {
				if (pubKey === $myPubKey) return;
				
				const commitment = commitments[pubKey];
				const angle = index * angleStep;
				
				result.set(pubKey, {
					pubKey,
					name: pubKey.slice(0, 8),
					commitment,
					capacity: getTotalCapacity(commitment),
					need: getTotalNeed(commitment),
					x: width / 2 + radius * Math.cos(angle),
					y: height / 2 + radius * Math.sin(angle)
				});
			});
			
			return result;
		}
	);
	
	// Get allocations for a provider
	function getProviderAllocations(providerPubKey: string): Map<string, { amount: number; tier: 'mutual' | 'non-mutual' }> {
		const allocations = new Map();
		
		const allocationState = providerPubKey === $myPubKey 
			? $myAllocationStateStore 
			: networkAllocationStates.get(providerPubKey);
			
		if (!allocationState?.slot_allocations) return allocations;
		
		// Get provider's commitment to check slot types
		const providerCommitment = providerPubKey === $myPubKey
			? $myCommitmentStore
			: networkCommitments.get(providerPubKey);
		
		// If filtering by resource type, only include allocations from matching slots
		const validSlotIds = new Set<string>();
		if (resourceType && providerCommitment?.capacity_slots) {
			const filteredSlots = filterSlotsByType(providerCommitment.capacity_slots);
			filteredSlots.forEach(slot => validSlotIds.add(slot.id));
		}
		
		// Aggregate slot allocations by recipient
		for (const slotAlloc of allocationState.slot_allocations) {
			// Skip if filtering and slot doesn't match resource type
			if (resourceType && validSlotIds.size > 0 && !validSlotIds.has(slotAlloc.availability_slot_id)) {
				continue;
			}
			
			const existing = allocations.get(slotAlloc.recipient_pubkey) || { amount: 0, tier: slotAlloc.tier };
			existing.amount += slotAlloc.quantity;
			allocations.set(slotAlloc.recipient_pubkey, existing);
		}
		
		return allocations;
	}
	
	// Get who is providing to a receiver
	function getReceiverSources(receiverPubKey: string): Map<string, { amount: number; tier: 'mutual' | 'non-mutual' }> {
		const sources = new Map();
		
		$entities.forEach((entity, providerPubKey) => {
			if (providerPubKey === receiverPubKey) return;
			if (entity.capacity <= 0) return;
			
			const allocations = getProviderAllocations(providerPubKey);
			const allocation = allocations.get(receiverPubKey);
			
			if (allocation) {
				sources.set(providerPubKey, allocation);
			}
		});
		
		return sources;
	}
	
	// Render visualization
	function renderVisualization() {
		if (!svgRef) return;
		
		const svg = d3.select(svgRef);
		svg.selectAll('*').remove();
		
		svg.attr('width', width).attr('height', height);
		
		// Draw connection lines (mutual recognition)
		const connections: Array<{
			source: string;
			target: string;
			mr: number;
			sourceEntity: typeof $entities extends Map<string, infer T> ? T : never;
			targetEntity: typeof $entities extends Map<string, infer T> ? T : never;
		}> = [];
		
		$entities.forEach((entity1, id1) => {
			$entities.forEach((entity2, id2) => {
				if (id1 >= id2) return;
				
				const mr = getMR(id1, id2);
				if (mr > 0) {
					connections.push({
						source: id1,
						target: id2,
						mr,
						sourceEntity: entity1,
						targetEntity: entity2
					});
				}
			});
		});
		
		svg.insert('g', ':first-child')
			.selectAll('.connection')
			.data(connections)
			.enter()
			.append('line')
			.attr('class', 'connection')
			.attr('x1', d => d.sourceEntity.x)
			.attr('y1', d => d.sourceEntity.y)
			.attr('x2', d => d.targetEntity.x)
			.attr('y2', d => d.targetEntity.y)
			.attr('stroke', '#0EA5E9')
			.attr('stroke-width', d => d.mr * 6)
			.attr('opacity', 0.3)
			.attr('stroke-dasharray', '5,5');
		
		// Draw entities
		$entities.forEach((entity, id) => {
			const g = svg.append('g')
				.attr('class', `entity-${id}`)
				.attr('transform', `translate(${entity.x}, ${entity.y})`)
				.style('cursor', 'pointer')
				.on('click', () => selectedEntity = id);
			
			const hasCapacity = entity.capacity > 0;
			const hasNeed = entity.need > 0;
			const baseRadius = hasCapacity ? 40 + (entity.capacity / 20) : 30 + (entity.need / 20);
			
			// CAPACITY CIRCLE (Provider)
			if (hasCapacity) {
				const allocations = getProviderAllocations(id);
				
				if (allocations.size > 0) {
					const pie = d3.pie<[string, { amount: number; tier: 'mutual' | 'non-mutual' }]>()
						.value(d => d[1].amount)
						.sort(null);
					
					const arc = d3.arc()
						.innerRadius(0)
						.outerRadius(baseRadius);
					
					const pieData = pie(Array.from(allocations.entries()));
					
					g.selectAll('.capacity-slice')
						.data(pieData)
						.enter()
						.append('path')
						.attr('class', 'capacity-slice')
						.attr('d', arc as any)
						.attr('fill', d => d.data[1].tier === 'mutual' ? '#0EA5E9' : '#F59E0B')
						.attr('stroke', '#1e293b')
						.attr('stroke-width', 2)
						.attr('opacity', 0.85)
						.on('mouseover', function(event, d) {
							d3.select(this).attr('opacity', 1);
							const recipientName = $entities.get(d.data[0])?.name || d.data[0].slice(0, 8);
							g.append('text')
								.attr('class', 'tooltip')
								.attr('x', 0)
								.attr('y', -baseRadius - 20)
								.attr('text-anchor', 'middle')
								.attr('fill', '#fff')
								.attr('font-size', '12px')
								.text(`→ ${recipientName}: ${Math.round(d.data[1].amount)}`);
						})
						.on('mouseout', function() {
							d3.select(this).attr('opacity', 0.85);
							g.selectAll('.tooltip').remove();
						});
				} else {
					g.append('circle')
						.attr('r', baseRadius)
						.attr('fill', '#334155')
						.attr('stroke', '#64748b')
						.attr('stroke-width', 2)
						.attr('opacity', 0.6);
				}
				
				g.append('text')
					.attr('y', baseRadius + 20)
					.attr('text-anchor', 'middle')
					.attr('fill', '#0EA5E9')
					.attr('font-size', '11px')
					.attr('font-weight', 'bold')
					.text(`💰 ${entity.capacity.toFixed(0)}`);
			}
			
			// NEED CIRCLE (Receiver)
			if (hasNeed) {
				const sources = getReceiverSources(id);
				const needRadius = baseRadius * 0.6;
				
				if (sources.size > 0) {
					const pie = d3.pie<[string, { amount: number; tier: 'mutual' | 'non-mutual' }]>()
						.value(d => d[1].amount)
						.sort(null);
					
					const arc = d3.arc()
						.innerRadius(hasCapacity ? baseRadius * 0.3 : 0)
						.outerRadius(hasCapacity ? baseRadius * 0.6 : needRadius);
					
					const totalReceived = Array.from(sources.values()).reduce((sum, s) => sum + s.amount, 0);
					const pieData = pie(Array.from(sources.entries()));
					
					g.selectAll('.need-slice')
						.data(pieData)
						.enter()
						.append('path')
						.attr('class', 'need-slice')
						.attr('d', arc as any)
						.attr('fill', d => d.data[1].tier === 'mutual' ? '#10B981' : '#84CC16')
						.attr('stroke', '#1e293b')
						.attr('stroke-width', 1.5)
						.attr('opacity', 0.8)
						.on('mouseover', function(event, d) {
							d3.select(this).attr('opacity', 1);
							const providerName = $entities.get(d.data[0])?.name || d.data[0].slice(0, 8);
							g.append('text')
								.attr('class', 'tooltip')
								.attr('x', 0)
								.attr('y', needRadius + 15)
								.attr('text-anchor', 'middle')
								.attr('fill', '#fff')
								.attr('font-size', '11px')
								.text(`← ${providerName}: ${Math.round(d.data[1].amount)}`);
						})
						.on('mouseout', function() {
							d3.select(this).attr('opacity', 0.8);
							g.selectAll('.tooltip').remove();
						});
					
					const fulfillment = (totalReceived / entity.need * 100).toFixed(0);
					g.append('text')
						.attr('y', hasCapacity ? 0 : 5)
						.attr('text-anchor', 'middle')
						.attr('fill', +fulfillment >= 80 ? '#10B981' : '#F59E0B')
						.attr('font-size', '10px')
						.attr('font-weight', 'bold')
						.text(`${fulfillment}%`);
				} else {
					if (!hasCapacity) {
						g.append('circle')
							.attr('r', needRadius)
							.attr('fill', 'none')
							.attr('stroke', '#EF4444')
							.attr('stroke-width', 2)
							.attr('stroke-dasharray', '5,5')
							.attr('opacity', 0.6);
					}
				}
				
				g.append('text')
					.attr('y', baseRadius + (hasCapacity ? 35 : 20))
					.attr('text-anchor', 'middle')
					.attr('fill', '#EF4444')
					.attr('font-size', '11px')
					.text(`📊 ${entity.need.toFixed(0)}`);
			}
			
			// Entity name
			g.append('text')
				.attr('y', -baseRadius - 10)
				.attr('text-anchor', 'middle')
				.attr('fill', '#f1f5f9')
				.attr('font-size', '14px')
				.attr('font-weight', 'bold')
				.text(entity.name);
			
			// Breathing animation
			g.transition()
				.duration(2000)
				.ease(d3.easeSinInOut)
				.attr('transform', `translate(${entity.x}, ${entity.y}) scale(1.02)`)
				.transition()
				.duration(2000)
				.ease(d3.easeSinInOut)
				.attr('transform', `translate(${entity.x}, ${entity.y}) scale(1)`)
				.on('end', function repeat() {
					d3.select(this)
						.transition()
						.duration(2000)
						.ease(d3.easeSinInOut)
						.attr('transform', `translate(${entity.x}, ${entity.y}) scale(1.02)`)
						.transition()
						.duration(2000)
						.ease(d3.easeSinInOut)
						.attr('transform', `translate(${entity.x}, ${entity.y}) scale(1)`)
						.on('end', repeat);
				});
		});
	}
	
	// Run allocation round
	async function runAllocation() {
		round++;
		
		// Recompute and publish my allocation
		await computeAndPublishAllocations();
		
		console.log('[CIRCLE-VIZ] Round', round, 'complete');
	}
	
	// React to store changes
	$effect(() => {
		// Trigger re-render when entities change
		$entities;
		$myAllocationStateStore;
		renderVisualization();
	});
	
	onMount(() => {
		renderVisualization();
	});
</script>

<div class="circle-visualization">
	<!-- Header -->
	<div class="header">
		<h1>Free Association: Living Denominators</h1>
		<p>Capacity circles distribute outward • Need circles receive inward</p>
		{#if resourceType}
			<p class="resource-filter">Filtered by resource type: <strong>{resourceType}</strong></p>
		{/if}
	</div>
	
	<!-- Legend -->
	<div class="legend">
		<div class="legend-item">
			<div class="color-box" style="background-color: #0EA5E9;"></div>
			<div>
				<div class="legend-title">Tier 1 Capacity</div>
				<div class="legend-desc">Mutual recognition (priority)</div>
			</div>
		</div>
		
		<div class="legend-item">
			<div class="color-box" style="background-color: #F59E0B;"></div>
			<div>
				<div class="legend-title">Tier 2 Capacity</div>
				<div class="legend-desc">Generous giving (leftover)</div>
			</div>
		</div>
		
		<div class="legend-item">
			<div class="color-box" style="background-color: #10B981;"></div>
			<div>
				<div class="legend-title">Receiving (Mutual)</div>
				<div class="legend-desc">From mutual partners</div>
			</div>
		</div>
		
		<div class="legend-item">
			<div class="color-box" style="background-color: #84CC16;"></div>
			<div>
				<div class="legend-title">Receiving (Generous)</div>
				<div class="legend-desc">From non-mutual donors</div>
			</div>
		</div>
	</div>
	
	<!-- Controls -->
	<div class="controls">
		<button class="btn-primary" onclick={runAllocation}>
			Run Round {round + 1}
		</button>
	</div>
	
	<!-- Visualization -->
	<div class="viz-container">
		<svg bind:this={svgRef} style="min-height: {height}px;"></svg>
	</div>
	
	<!-- Instructions -->
	<div class="instructions">
		<h3>How to Read:</h3>
		<ul>
			<li><strong>Outer pie slices</strong> = Capacity circle (who they're giving to)</li>
			<li><strong>Inner pie slices</strong> = Need circle (who they're receiving from)</li>
			<li><strong>Blue/Amber</strong> = Provider's allocation by tier (mutual vs generous)</li>
			<li><strong>Green/Lime</strong> = Receiver's sources by tier</li>
			<li><strong>Circle size</strong> = Denominator magnitude (total system weight)</li>
			<li><strong>Breathing animation</strong> = Living system responding to changes</li>
			<li><strong>Dashed lines</strong> = Mutual recognition connections</li>
		</ul>
	</div>
</div>

<style>
	.circle-visualization {
		width: 100%;
		min-height: 100vh;
		background: linear-gradient(135deg, #0f172a 0%, #1e293b 50%, #0f172a 100%);
		padding: 2rem;
		overflow: auto;
	}
	
	.header {
		text-align: center;
		margin-bottom: 2rem;
	}
	
	.header h1 {
		font-size: 2.5rem;
		font-weight: bold;
		color: white;
		margin-bottom: 0.5rem;
	}
	
	.header p {
		font-size: 1.125rem;
		color: #cbd5e1;
	}
	
	.header .resource-filter {
		font-size: 1rem;
		color: #60a5fa;
		margin-top: 0.5rem;
	}
	
	.header .resource-filter strong {
		color: #93c5fd;
		font-weight: 700;
	}
	
	.legend {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1rem;
		margin-bottom: 1.5rem;
	}
	
	.legend-item {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		background-color: rgba(30, 41, 59, 0.5);
		border: 1px solid #475569;
		border-radius: 0.5rem;
		padding: 0.75rem;
	}
	
	.color-box {
		width: 1rem;
		height: 1rem;
		border-radius: 9999px;
		flex-shrink: 0;
	}
	
	.legend-title {
		color: white;
		font-weight: 600;
		font-size: 0.875rem;
	}
	
	.legend-desc {
		color: #94a3b8;
		font-size: 0.75rem;
	}
	
	.controls {
		display: flex;
		gap: 1rem;
		justify-content: center;
		margin-bottom: 1.5rem;
	}
	
	.btn-primary {
		padding: 0.75rem 1.5rem;
		background-color: #2563eb;
		color: white;
		border: none;
		border-radius: 0.5rem;
		font-weight: 600;
		cursor: pointer;
		transition: background-color 0.2s;
	}
	
	.btn-primary:hover {
		background-color: #1d4ed8;
	}
	
	.viz-container {
		background-color: rgba(30, 41, 59, 0.3);
		border: 1px solid #475569;
		border-radius: 0.75rem;
		padding: 1.5rem;
		backdrop-filter: blur(4px);
		margin-bottom: 1.5rem;
	}
	
	.viz-container svg {
		width: 100%;
		display: block;
	}
	
	.instructions {
		background-color: rgba(30, 41, 59, 0.5);
		border: 1px solid #475569;
		border-radius: 0.5rem;
		padding: 1rem;
	}
	
	.instructions h3 {
		color: white;
		font-weight: 600;
		margin-bottom: 0.5rem;
	}
	
	.instructions ul {
		color: #cbd5e1;
		font-size: 0.875rem;
		list-style-position: inside;
	}
	
	.instructions li {
		margin-bottom: 0.25rem;
	}
	
	.instructions strong {
		color: white;
	}
</style>

