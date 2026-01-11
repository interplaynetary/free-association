<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { page } from '$app/state';
	import Parent from '$lib/components/Parent.svelte';
	import Bar from '$lib/components/Bar.svelte';
	import Map from '$lib/components/Map.svelte';
	import Type from '$lib/components/Type.svelte';
	import {
		myRecognitionTreeStore,
		myRecognitionWeights,
		myMutualRecognition,
		myNeedSlotsStore,
		myCapacitySlotsStore,
		myNeedTypesStore,
		myCapacityTypesStore,
		myCommitmentStore,
		// enableAutoCommitmentComposition, setMyNeedSlots etc removed
		setMyNeedSlots,
		setMyCapacitySlots,
		subscribeToRecognitionTree,
		subscribeToCommitment,
		networkRecognitionTrees,
		networkCommitments
	} from '$lib/protocol/stores/stores.svelte';

	import { globalState } from '$lib/global.svelte';
	import { demoTreeStore } from '$lib/protocol/stores/demoTree.svelte';
	import { currentPath } from '$lib/global.svelte';
	import { derived, get } from 'svelte/store';
	import { t, loading } from '$lib/translations';
	import { holsterUserPub } from '$lib/network/holster.svelte';
	import type {
		NeedSlot,
		AvailabilitySlot,
		NonRootNode,
		RootNode
	} from '$lib/protocol/schemas';
	import { types, formatNeedType } from '$lib/protocol/needTypes-local';
	import type { PageData } from './+page';
	import { globalOrganizations } from '$lib/network/organizations.svelte';
	import { DEMO_ORGANIZATIONS } from '$lib/config/org-trees';
	import {
		sharesOfGeneralFulfillmentMap,
		getAllContributorsFromTree
	} from '@playnet/free-association/tree';
	// Local implementation of mutual recognition (since it was removed from allocation module)
	function computeMutualRecognition(
		myRecognition: Record<string, number>,
		othersRecognition: Record<string, Record<string, number>>,
		myId: string
	): Record<string, number> {
		console.log('[TRACE] [ENTER] src/routes/org/[slug]/+page.svelte: computeMutualRecognition');
		const mutual: Record<string, number> = {};
		
		// For each person/org I recognize
		for (const [theirId, myWeight] of Object.entries(myRecognition)) {
			if (myWeight <= 0) continue;
			
			// Check how much they recognize me
			// othersRecognition is a map of theirId -> their weights
			const theirWeights = othersRecognition[theirId] || {};
			const theirWeight = theirWeights[myId] || 0;
			
			// Mutual is the minimum of the two
			const mutualWeight = Math.min(myWeight, theirWeight);
			if (mutualWeight > 0) {
				mutual[theirId] = mutualWeight;
			}
		}
		
		return mutual;
		console.log('[TRACE] [EXIT] src/routes/org/[slug]/+page.svelte: computeMutualRecognition'); // Unreachable, need to fix manual return placement if I could, but wait, function returns on line 73.
	}
	import { 
		setViewContext, 
		resetViewContext, 
		currentUserTree, 
		currentUserCapacityTypes,
		currentUserCapacitySlots,
		currentUserMutualRecognition
	} from '$lib/protocol/stores/context.svelte';

	// Get page data (tree configuration)
	const { data }: { data: PageData } = $props();

	// Reactive view state
	const currentView = $derived(globalState.currentView);

	// Reactive state for inventory view (Svelte 5 runes)
	let needSlots = $state<NeedSlot[]>([]);
	let capacitySlots = $state<AvailabilitySlot[]>([]);

	// Form state for adding new slots
	let newNeedName = $state('');
	let newNeedType = $state('food');
	let newNeedQuantity = $state(10);

	let newCapacityName = $state('');
	let newCapacityType = $state('food');
	let newCapacityQuantity = $state(5);

	let showRawData = $state(false);

	// Cleanup functions
	let cleanupUserTreeSubscription: (() => void) | null = null;
	
	// Loading state for user trees
	let userTreeLoading = $state(false);
	let userTreeLoaded = $state(false);

	// ✅ CRITICAL: Initialize org tree IMMEDIATELY (before component renders)
// This prevents the SDG tree from being loaded from localStorage first
console.log('[TRACE] [STEP] src/routes/org/[slug]/+page.svelte: initialization logic');
console.log('[ORG-PAGE] Initializing org page for:', data.orgName);

// Handle user trees differently from org trees
if (data.isUserTree) {
	const userPubkey = data.userPubkey!;
	const isOwnTree = get(holsterUserPub) === userPubkey;
	
	if (isOwnTree) {
		// 🎯 Viewing own tree - instant load from myRecognitionTreeStore
		console.log('[ORG-PAGE] 👤 Viewing own tree');
		const myTree = get(myRecognitionTreeStore);
		
		if (myTree) {
			const treeWithCorrectId = { ...myTree, id: userPubkey, manual_fulfillment: myTree.manual_fulfillment ?? undefined };
			demoTreeStore.initializeWithCustomTree(treeWithCorrectId, true);
			userTreeLoading = false;
			userTreeLoaded = true;
			console.log('[ORG-PAGE] ✅ Own tree loaded');
		} else {
			console.warn('[ORG-PAGE] ⚠️ Own tree not found');
		}
	} else {
		// 🌐 Viewing another user's tree - load from network
		console.log('[ORG-PAGE] 🌐 Loading from network:', userPubkey);
		userTreeLoading = true;
		userTreeLoaded = false;
		
		// Placeholder while loading
		const placeholderTree: RootNode = {
			id: userPubkey,
			name: `${data.orgName}`,
			type: 'RootNode',
			children: [],
			manual_fulfillment: 0,
			created_at: new Date().toISOString(),
			updated_at: new Date().toISOString()
		};
		demoTreeStore.initializeWithCustomTree(placeholderTree as any, true);
		
		// Subscribe to network data
		subscribeToRecognitionTree(userPubkey);
		subscribeToCommitment(userPubkey);
		
		// Check if data already exists
		const existingTree = get(networkRecognitionTrees).get(userPubkey);
		
		if (existingTree?.data) {
			// Fast path: data already cached
			console.log('[ORG-PAGE] 💡 Found cached data');
			const treeWithCorrectId = { ...existingTree.data, id: userPubkey, manual_fulfillment: existingTree.data.manual_fulfillment ?? undefined };
			demoTreeStore.initializeWithCustomTree(treeWithCorrectId, true);
			userTreeLoading = false;
			userTreeLoaded = true;
		} else {
			// Wait for network
			const loadTimeout = setTimeout(() => {
				if (userTreeLoading) {
					console.warn('[ORG-PAGE] ⚠️ Timeout');
				}
			}, 10000);
			
			const unsubTree = networkRecognitionTrees.subscribe(($trees) => {
				if (userTreeLoaded) return;
				const tree = $trees.get(userPubkey);
				if (tree?.data) {
					clearTimeout(loadTimeout);
					const treeWithCorrectId = { ...tree.data, id: userPubkey, manual_fulfillment: tree.data.manual_fulfillment ?? undefined };
					demoTreeStore.initializeWithCustomTree(treeWithCorrectId, true);
					userTreeLoading = false;
					userTreeLoaded = true;
					console.log('[ORG-PAGE] ✅ Loaded from network');
				}
			});
			
			const unsubCommit = networkCommitments.subscribe(($commits) => {
				const commit = $commits.get(userPubkey);
				if (commit?.data) {
					const weights = Object.keys(commit.data.global_recognition_weights || {}).length;
					console.log(`[ORG-PAGE] Commitment: ${weights} weights`);
				}
			});
			
			cleanupUserTreeSubscription = () => {
				clearTimeout(loadTimeout);
				unsubTree();
				unsubCommit();
			};
		}
	}
} else {
	// Original org tree logic
	console.log('[ORG-PAGE] Preparing custom tree:', data.tree!.name);
	
	// Bootstrap: Register demo orgs in globalOrganizations IMMEDIATELY
	// (In production, orgs would come from Holster network)
	globalOrganizations.update((orgs) => ({ ...orgs, ...DEMO_ORGANIZATIONS }));
	console.log('[ORG-PAGE] Registered demo organizations');
	
	// Use the tree as-is - it already has node-specific contributors from the JSON config
	const orgTreeWithContributors = structuredClone(data.tree!);
	
	// Initialize demo tree with organization-specific tree IMMEDIATELY
	// Force initialization to ensure we load the org tree even if another tree exists
	// persist=false means this tree won't be saved to localStorage
	console.log(
		'[ORG-PAGE] Initializing demo tree store with org tree NOW (before component renders)'
	);
	demoTreeStore.initializeWithCustomTree(orgTreeWithContributors, true);
}


	onMount(() => {
	console.log('[TRACE] [ENTER] src/routes/org/[slug]/+page.svelte: onMount');
	console.log('[ORG-PAGE] Component mounted - tree already initialized');
		
		// Ensure we're in tree view
		globalState.currentView = 'tree';
		console.log('[ORG-PAGE] Set currentView to tree');

		// Initialize path with the tree root
	if (data.isUserTree && data.userPubkey) {
		// Use the actual pubkey as the root ID (the tree loaded from network will have this as its root)
		currentPath.set([data.userPubkey]);
		console.log('[ORG-PAGE] Set path to user tree root (pubkey):', data.userPubkey);
		// 🎯 ELEGANT: Set the view context so all context-aware stores work!
		setViewContext(data.userPubkey);
		console.log('[ORG-PAGE] Set view context to:', data.userPubkey);
	} else if (data.tree?.id) {
		currentPath.set([data.tree.id]);
		console.log('[ORG-PAGE] Set path to org tree root:', data.tree.id);
	}

		// Subscribe to stores (reactive)
		const unsubNeeds = myNeedSlotsStore.subscribe((slots) => {
			needSlots = slots || [];
		});

		const unsubCapacity = myCapacitySlotsStore.subscribe((slots) => {
			capacitySlots = slots || [];
		});

		return () => {
			unsubNeeds();
			unsubCapacity();
		};
		console.log('[TRACE] [EXIT] src/routes/org/[slug]/+page.svelte: onMount');
	});

	// Clean up org tree when leaving this route
	onDestroy(() => {
		console.log('[ORG-PAGE] Destroying - clearing org tree and reinitializing with SDG');
		
		// Reset view context back to authenticated user
		resetViewContext();
		
		// Clear the org tree from memory and reinitialize with default SDG tree
		demoTreeStore.clear();
		demoTreeStore.initializeWithSDG();
	});

	// CRUD Operations - Needs
	function addNeedSlot() {
		console.log('[TRACE] [ENTER] src/routes/org/[slug]/+page.svelte: addNeedSlot');
		if (!newNeedName.trim()) return;

		const newSlot: NeedSlot = {
			id: `need_${Date.now()}_${Math.random()}`,
			name: newNeedName,
			type_id: newNeedType,
			quantity: newNeedQuantity,
			unit: 'units',
			max_natural_div: 1,
			min_allocation_percentage: 0.01,
			recurrence: undefined,
			availability_window: undefined
		};

		setMyNeedSlots([...needSlots, newSlot]);

		// Reset form
		newNeedName = '';
		newNeedQuantity = 10;
		console.log('[TRACE] [EXIT] src/routes/org/[slug]/+page.svelte: addNeedSlot');
	}

	function removeNeedSlot(id: string) {
		console.log('[TRACE] [ENTER] src/routes/org/[slug]/+page.svelte: removeNeedSlot', { id });
		setMyNeedSlots(needSlots.filter((s) => s.id !== id));
		console.log('[TRACE] [EXIT] src/routes/org/[slug]/+page.svelte: removeNeedSlot');
	}

	function updateNeedQuantity(id: string, quantity: number) {
		const updated = needSlots.map((s) => (s.id === id ? { ...s, quantity } : s));
		setMyNeedSlots(updated);
	}

	// CRUD Operations - Capacity
	function addCapacitySlot() {
		if (!newCapacityName.trim()) return;

		const newSlot: AvailabilitySlot = {
			id: `capacity_${Date.now()}_${Math.random()}`,
			name: newCapacityName,
			type_id: newCapacityType,
			quantity: newCapacityQuantity,
			unit: 'units',
			max_natural_div: 1,
			min_allocation_percentage: 0.01,
			recurrence: undefined,
			availability_window: undefined,
			priority_distribution: undefined
		};

		setMyCapacitySlots([...capacitySlots, newSlot]);

		// Reset form
		newCapacityName = '';
		newCapacityQuantity = 5;
	}

	function removeCapacitySlot(id: string) {
		setMyCapacitySlots(capacitySlots.filter((s) => s.id !== id));
	}

	function updateCapacityQuantity(id: string, quantity: number) {
		const updated = capacitySlots.map((s) => (s.id === id ? { ...s, quantity } : s));
		setMyCapacitySlots(updated);
	}

	// Batch update handlers for Type component
	function handleNeedTypeBatchUpdate(typeId: string, updates: Partial<NeedSlot>) {
		const updated = needSlots.map((slot) =>
			slot.type_id === typeId ? { ...slot, ...updates } : slot
		);
		setMyNeedSlots(updated);
	}

	function handleCapacityTypeBatchUpdate(typeId: string, updates: Partial<AvailabilitySlot>) {
		const updated = capacitySlots.map((slot) =>
			slot.type_id === typeId ? { ...slot, ...updates } : slot
		);
		setMyCapacitySlots(updated);
	}

	// Individual slot update handlers (for full slot editing with new editors)
	function handleNeedSlotUpdate(updatedSlot: NeedSlot) {
		const updated = needSlots.map((s) => (s.id === updatedSlot.id ? updatedSlot : s));
		setMyNeedSlots(updated);
	}

	function handleCapacitySlotUpdate(updatedSlot: AvailabilitySlot) {
		const updated = capacitySlots.map((s) => (s.id === updatedSlot.id ? updatedSlot : s));
		setMyCapacitySlots(updated);
	}

	// ═══════════════════════════════════════════════════════════════════
	// DEMO RECOGNITION STORES (Reuse Protocol Algorithms, Separate Data)
	// ═══════════════════════════════════════════════════════════════════

	// V5: Compute recognition from demo tree (since stores require authentication)
	// ✅ REUSES PROTOCOL: Same sharesOfGeneralFulfillmentMap algorithm
	// ✅ SEPARATE DATA: Uses demoTreeStore instead of myRecognitionTreeStore
	const demoRecognitionWeights = derived([demoTreeStore.toStore()], ([$tree]) => {
		console.log('[TRACE] [STEP] src/routes/org/[slug]/+page.svelte: demoRecognitionWeights (recalculation)');
		console.log('[📊 DEMO-REC] Tree changed - recomputing recognition weights');

		if (!$tree) {
			console.log('[📊 DEMO-REC] ❌ No demo tree available');
			return {};
		}

		// DEBUG: Inspect tree structure
		console.log('[📊 DEMO-REC]   Root ID:', $tree.id);
		console.log('[📊 DEMO-REC]   Root name:', $tree.name);
		console.log('[📊 DEMO-REC]   Children count:', $tree.children?.length || 0);

		// Find and log all contribution nodes (nodes with contributors)
		const allNodes = [$tree];
		const queue = [...($tree.children || [])];
		while (queue.length > 0) {
			const node = queue.shift()!;
			allNodes.push(node);
			queue.push(...(node.children || []));
		}

		console.log('[📊 DEMO-REC]   Total nodes in tree:', allNodes.length);

		const contributionNodes = allNodes.filter((node) => {
			if (node.type === 'RootNode') return false;
			const nonRoot = node as any;
			return nonRoot.contributors && nonRoot.contributors.length > 0;
		});

		console.log(
			'[📊 DEMO-REC]   Contribution nodes (with contributors):',
			contributionNodes.length
		);

		contributionNodes.forEach((node: any) => {
			console.log(
				`[📊 DEMO-REC]     • Node "${node.name}" (${node.id}): ${node.contributors.length} contributors`
			);
			node.contributors.forEach((c: any) => {
				console.log(`[📊 DEMO-REC]       - ${c.id} (${c.points} points)`);
			});
		});

		// Try to get contributors from tree using protocol function
		console.log('[📊 DEMO-REC] Getting all contributors from tree via protocol...');
		const allContributors = getAllContributorsFromTree($tree);
		console.log('[📊 DEMO-REC]   Protocol found', allContributors.length, 'unique contributor IDs');
		allContributors.forEach((id) => {
			console.log(`[📊 DEMO-REC]     • ${id}`);
		});

		try {
			console.log('[📊 DEMO-REC] Computing recognition weights...');
			// ✅ REUSE PROTOCOL ALGORITHM
			const weights = sharesOfGeneralFulfillmentMap($tree, {});
			const nonZero = Object.values(weights).filter((w) => w > 0).length;
			console.log(
				`[📊 DEMO-REC] ✅ Computed ${Object.keys(weights).length} total, ${nonZero} non-zero recognition weights`
			);

			Object.entries(weights).forEach(([id, weight]) => {
				if (weight > 0) {
					console.log(`[📊 DEMO-REC]   • ${id}: ${(weight * 100).toFixed(2)}%`);
				}
			});

			return weights;
		} catch (error) {
			console.error('[📊 DEMO-REC] ❌ Error computing recognition:', error);
			return {};
		}
	});

	// Load org recognition data from config (who recognizes whom in the demo ecosystem)
	// This is the "others_recognition_of_me" equivalent for demo mode
	import { readable } from 'svelte/store';
	import type { GlobalRecognitionWeights } from '@playnet/free-association/schemas';
	import { getOrgTreesMap } from '$lib/config/org-trees';

	const demoOrgRecognitionMap = readable<Record<string, GlobalRecognitionWeights>>({}, (set) => {
		// Load all org configs and compute their recognition weights
		const orgTreesMap = getOrgTreesMap();
		const recognitionMap: Record<string, GlobalRecognitionWeights> = {};

		console.log('[📊 DEMO-ORG-REC] Loading org recognition data from config...');

		for (const [slug, config] of Object.entries(orgTreesMap)) {
			if (!config.recognizes || config.recognizes.length === 0) continue;

			// Compute recognition weights for this org
			// Convert contributors array to weights (same as protocol does)
			const totalPoints = config.recognizes.reduce((sum, c) => sum + (c.points || 0), 0);
			const weights: GlobalRecognitionWeights = {};

			config.recognizes.forEach((contributor) => {
				if (contributor.points && contributor.id) {
					weights[contributor.id] = contributor.points / totalPoints;
				}
			});

			// Map by org_id (not slug)
			const orgId = `org_demo_${slug.replace(/-/g, '')}`;
			recognitionMap[orgId] = weights;

			console.log(`[📊 DEMO-ORG-REC]   ${config.name} (${orgId}):`, weights);
		}

		console.log(
			`[📊 DEMO-ORG-REC] ✅ Loaded ${Object.keys(recognitionMap).length} org recognition trees`
		);
		set(recognitionMap);

		return () => {}; // No cleanup needed
	});

	// V5: Compute mutual recognition for demo mode
	// ✅ REUSES PROTOCOL: Same computeMutualRecognition algorithm from allocation.ts
	// ✅ SEPARATE DATA: Uses demo recognition weights + org recognition data
	const demoMutualRecognition = derived(
		[demoRecognitionWeights, demoOrgRecognitionMap],
		([$myRecognition, $orgRecognition]) => {
			console.log('[TRACE] [STEP] src/routes/org/[slug]/+page.svelte: demoMutualRecognition (recalculation)');
			console.log('[🤝 DEMO-MR] Computing mutual recognition for demo mode...');

			if (!$myRecognition || Object.keys($myRecognition).length === 0) {
				console.log('[🤝 DEMO-MR] ❌ No recognition weights available');
				return {};
			}

			const myRecCount = Object.keys($myRecognition).length;
			const orgRecCount = Object.keys($orgRecognition).length;
			console.log(`[🤝 DEMO-MR] My recognition: ${myRecCount} entries`);
			console.log(`[🤝 DEMO-MR] Org recognition: ${orgRecCount} orgs`);

			// ✅ FIX: Use the org_demo_* format to match recognition data keys
			// Convert slug → org_id (e.g., "unicef" → "org_demo_unicef")
			const myOrgId = `org_demo_${data.slug.replace(/-/g, '')}`;
			console.log(`[🤝 DEMO-MR] My org ID: ${myOrgId}`);

			// DEBUG: Log the inputs before calling the algorithm
			console.log('[🤝 DEMO-MR] Inputs to computeMutualRecognition:');
			console.log('[🤝 DEMO-MR]   myRecognition:', $myRecognition);
			console.log('[🤝 DEMO-MR]   orgRecognition keys:', Object.keys($orgRecognition));
			Object.entries($orgRecognition).forEach(([orgId, theirWeights]) => {
				console.log(`[🤝 DEMO-MR]     ${orgId} recognizes:`, theirWeights);
				if (theirWeights[myOrgId]) {
					console.log(
						`[🤝 DEMO-MR]       → Recognizes ${myOrgId}: ${(theirWeights[myOrgId] * 100).toFixed(2)}%`
					);
				}
			});

			// ✅ REUSE PROTOCOL ALGORITHM - Same function as authenticated mode!
			const mutualRec = computeMutualRecognition(
				$myRecognition, // Who I recognize
				$orgRecognition, // Who recognizes me (from config)
				myOrgId // My org ID (matches config format!)
			);

			const mutualCount = Object.values(mutualRec).filter((mr) => mr > 0).length;
			console.log(`[🤝 DEMO-MR] ✅ Computed ${mutualCount} mutual recognition relationships`);

			// Detailed comparison logging
			Object.entries(mutualRec).forEach(([id, mr]) => {
				const myRec = $myRecognition[id] || 0;
				const theirRec = $orgRecognition[id]?.[myOrgId] || 0;
				const minValue = Math.min(myRec, theirRec);
				const capped = myRec < theirRec ? 'MY_REC' : theirRec < myRec ? 'THEIR_REC' : 'EQUAL';

				console.log(`[🤝 DEMO-MR]   ${id}:`);
				console.log(`[🤝 DEMO-MR]     I→them: ${(myRec * 100).toFixed(2)}%`);
				console.log(`[🤝 DEMO-MR]     them→me: ${(theirRec * 100).toFixed(2)}%`);
				console.log(`[🤝 DEMO-MR]     MR: ${(mr * 100).toFixed(2)}% (capped by ${capped})`);

				if (mr > 0 && mr !== minValue) {
					console.error(
						`[🤝 DEMO-MR]     ❌ ERROR: MR (${mr}) != min(${myRec}, ${theirRec}) = ${minValue}`
					);
				}
			});

			return mutualRec;
		}
	);

	// V5: Create reactive derived store from demoRecognitionWeights (org page)
	// Recognition weights are computed from the org tree!
	// ✅ ORG PAGE: Always use demo recognition (computed from org tree), never auth weights
	const barSegments = derived(demoRecognitionWeights, ($weights) => {
		console.log('[📊 ORG-YR] Recognition weights changed - generating segments for bar...');

		if (!$weights || Object.keys($weights).length === 0) {
			console.log('[📊 ORG-YR] ❌ No recognition weights available');
			return [];
		}

		const totalEntries = Object.keys($weights).length;
		const nonZeroEntries = Object.values($weights as Record<string, number>).filter(
			(v) => v > 0
		).length;
		console.log(
			`[📊 ORG-YR] Recognition weights has ${totalEntries} entries (${nonZeroEntries} non-zero)`
		);

		// Transform recognition weights into segments for Bar
		const segments = Object.entries($weights as Record<string, number>)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending

		console.log(`[📊 ORG-YR] ✅ Generated ${segments.length} segments for recognition bar:`);
		segments.forEach((seg) => {
			console.log(`  • ${seg.id.slice(0, 20)}... → ${seg.value.toFixed(2)}%`);
		});
		return segments;
	});

	// V5: Create reactive derived store from mutual recognition
	// 🎯 ELEGANT: Now uses
	// Choose appropriate mutual recognition store based on page type
	// User trees: Use context-aware store (loads from network)
	// Demo orgs: Use demo store (computes from demo data)
	const mutualRecStore = data.isUserTree ? currentUserMutualRecognition : demoMutualRecognition;
	
	// V5: Create reactive derived store for mutual recognition bar
	const providerSegments = derived(mutualRecStore, ($mutualRec) => {
		console.log('[📊 ORG-MR] Mutual recognition changed - generating segments for bar...');

		if (!$mutualRec || Object.keys($mutualRec).length === 0) {
			console.log('[📊 ORG-MR] ❌ No mutual recognition data available');
			return [];
		}

		const totalEntries = Object.keys($mutualRec).length;
		const nonZeroEntries = Object.values($mutualRec as Record<string, number>).filter(
			(v) => v > 0
		).length;
		console.log(
			`[📊 ORG-MR] Mutual recognition has ${totalEntries} entries (${nonZeroEntries} non-zero)`
		);

		// Transform mutual recognition data into segments for Bar
		const segments = Object.entries($mutualRec as Record<string, number>)
			.filter(([_, value]) => value > 0) // Only include non-zero values
			.map(([id, value]) => ({
				id,
				value: value * 100 // Convert from decimal to percentage
			}))
			.sort((a, b) => b.value - a.value); // Sort by value descending

		console.log(`[📊 ORG-MR] ✅ Generated ${segments.length} segments for mutual recognition bar:`);
		segments.forEach((seg) => {
			console.log(`  • ${seg.id.slice(0, 20)}... → ${seg.value.toFixed(2)}%`);
		});

		return segments;
	});

	// V5: No manual recalculation needed! Everything is reactive 🎉
	// Recognition weights auto-update when tree changes
	// Mutual recognition auto-updates when recognition weights or network data changes

	import { formatBudget } from '$lib/config/org-trees';

	// Convert initial budget to display format
	function convertToDisplay(fullAmount: number): { value: number; unit: 'K' | 'M' | 'B' } {
		if (fullAmount >= 1_000_000_000) {
			return { value: parseFloat((fullAmount / 1_000_000_000).toFixed(1)), unit: 'B' };
		} else if (fullAmount >= 1_000_000) {
			return { value: parseFloat((fullAmount / 1_000_000).toFixed(1)), unit: 'M' };
		} else if (fullAmount >= 1_000) {
			return { value: parseFloat((fullAmount / 1_000).toFixed(1)), unit: 'K' };
		}
		return { value: fullAmount, unit: 'K' };
	}

	const initial = convertToDisplay(data.monthlyBudget || 0);
	let budgetValue = $state(initial.value);
	let budgetUnit = $state<'K' | 'M' | 'B'>(initial.unit);

	// Calculate full amount from display values
	function getFullAmount(): number {
		const multipliers = { K: 1_000, M: 1_000_000, B: 1_000_000_000 };
		return budgetValue * multipliers[budgetUnit];
	}
</script>

<svelte:head>
	<title>{data.orgName} - Free Association</title>
	<meta name="description" content={data.orgDescription} />
</svelte:head>

<div class="org-page-container">
	<div class="layout org-page" class:full-width={currentView !== 'tree'}>
		<div class="view-content">
			{#if currentView === 'tree'}
				{#if data.isUserTree && userTreeLoading}
					<!-- Loading state for user trees -->
					<div class="loading-container">
						<div class="loading-spinner"></div>
						<p>Loading {data.orgName}'s recognition tree from network...</p>
					</div>
				{:else}
					<Parent />
				{/if}
			{:else if currentView === 'map'}
				<Map fullHeight={true} />
			{:else if currentView === 'inventory'}
				<div class="inventory-view">
					<!-- Need Slots Section -->
					<section class="slots-section needs">
						<h2>🎯 My Need Slots ({needSlots.length})</h2>

						<div class="add-form">
							<input
								type="text"
								bind:value={newNeedName}
								placeholder="Need name..."
								onkeydown={(e) => e.key === 'Enter' && addNeedSlot()}
							/>
							<select bind:value={newNeedType}>
								{#each types as type}
									<option value={type.id}>{formatNeedType(type.id)}</option>
								{/each}
							</select>
							<input type="number" bind:value={newNeedQuantity} min="0" step="0.1" />
							<button onclick={addNeedSlot} class="btn-primary"> ➕ Add Need </button>
						</div>

						<div class="slots-list">
							{#if needSlots.length === 0}
								<div class="empty-state">No need slots yet. Add one above!</div>
							{:else}
								<!-- Organize need slots by type -->
								{#each $myNeedTypesStore as typeId (typeId)}
									<Type
										{typeId}
										typeName={formatNeedType(typeId)}
										slots={$myNeedSlotsStore}
										kind="need"
										capacityId="need-{typeId}"
										onBatchUpdate={handleNeedTypeBatchUpdate}
										onSlotUpdate={handleNeedSlotUpdate}
										onSlotDelete={removeNeedSlot}
									>
										{#snippet children({ slot }: { slot: NeedSlot })}
											<div class="slot-actions-row">
												<div class="quantity-control">
													<label for="need-qty-{slot.id}">Quantity:</label>
													<input
														id="need-qty-{slot.id}"
														type="number"
														value={slot.quantity}
														min="0"
														step="0.1"
														onchange={(e) =>
															slot.id && updateNeedQuantity(slot.id, parseFloat(e.currentTarget.value))}
													/>
													<span>{slot.unit || 'units'}</span>
												</div>
												<button onclick={() => slot.id && removeNeedSlot(slot.id)} class="btn-danger-small">
													🗑️ Delete
												</button>
											</div>
											{#if showRawData}
												<details class="raw-data">
													<summary>Raw data</summary>
													<pre>{JSON.stringify(slot, null, 2)}</pre>
												</details>
											{/if}
										{/snippet}
									</Type>
								{/each}
							{/if}
						</div>
					</section>

					<!-- Capacity Slots Section -->
					<section class="slots-section capacity">
						<h2>🎁 My Capacity Slots ({capacitySlots.length})</h2>

						<div class="add-form">
							<input
								type="text"
								bind:value={newCapacityName}
								placeholder="Capacity name..."
								onkeydown={(e) => e.key === 'Enter' && addCapacitySlot()}
							/>
							<select bind:value={newCapacityType}>
								{#each types as type}
									<option value={type.id}>{formatNeedType(type.id)}</option>
								{/each}
							</select>
							<input type="number" bind:value={newCapacityQuantity} min="0" step="0.1" />
							<button onclick={addCapacitySlot} class="btn-primary"> ➕ Add Capacity </button>
						</div>

						<div class="slots-list">
							{#if capacitySlots.length === 0}
								<div class="empty-state">No capacity slots yet. Add one above!</div>
							{:else}
								<!-- Organize capacity slots by type -->
								{#each $myCapacityTypesStore as typeId (typeId)}
									<Type
										{typeId}
										typeName={formatNeedType(typeId)}
										slots={$myCapacitySlotsStore}
										kind="capacity"
										capacityId="capacity-{typeId}"
										onBatchUpdate={handleCapacityTypeBatchUpdate}
										onSlotUpdate={handleCapacitySlotUpdate}
										onSlotDelete={removeCapacitySlot}
									>
										{#snippet children({ slot }: { slot: AvailabilitySlot })}
											<div class="slot-actions-row">
												<div class="quantity-control">
													<label for="capacity-qty-{slot.id}">Quantity:</label>
													<input
														id="capacity-qty-{slot.id}"
														type="number"
														value={slot.quantity}
														min="0"
														step="0.1"
														onchange={(e) =>
															slot.id && updateCapacityQuantity(slot.id, parseFloat(e.currentTarget.value))}
													/>
													<span>{slot.unit || 'units'}</span>
												</div>
												<button
													onclick={() => slot.id && removeCapacitySlot(slot.id)}
													class="btn-danger-small"
												>
													🗑️ Delete
												</button>
											</div>
											{#if showRawData}
												<details class="raw-data">
													<summary>Raw data</summary>
													<pre>{JSON.stringify(slot, null, 2)}</pre>
												</details>
											{/if}
										{/snippet}
									</Type>
								{/each}
							{/if}
						</div>
					</section>
				</div>
			{/if}
		</div>
		{#if currentView === 'tree'}
			{#key $loading}
				<div class="bars">
					<div class="bar-group">
						<div class="bar-label bar-label-yr" title={$t('home.your_recognition_description')}>
							<span class="label-mobile"
								>{@html $t('home.your_recognition').toLowerCase().replace(' ', '<br />')}</span
							>
							<span class="label-desktop">{$t('home.your_recognition_abbr')}</span>
						</div>
						<div class="bar-area">
							{#if $barSegments.length > 0}
								<Bar
									segments={$barSegments}
									width="100%"
									height="100%"
									showLabels={true}
									showLabelsAboveOnSelect={true}
									showValues={false}
									rounded={false}
								/>
							{:else}
								<div class="placeholder">
									<p>
										{$t('home.no_contributors')}
									</p>
								</div>
							{/if}
						</div>
					</div>
				<!-- Mutual recognition now works for ALL users via context-aware stores! -->
				<div class="bar-group">
						<div class="bar-label bar-label-mr" title={$t('home.mutual_recognition_description')}>
							<span class="label-mobile"
								>{@html $t('home.mutual_recognition').toLowerCase().replace(' ', '<br />')}</span
							>
							<span class="label-desktop">{$t('home.mutual_recognition_abbr')}</span>
						</div>
						<div class="bar-area">
							{#if $providerSegments.length > 0}
								<Bar
									segments={$providerSegments}
									width="100%"
									height="100%"
									showLabels={true}
									showLabelsAboveOnSelect={true}
									showValues={false}
									rounded={false}
								/>
							{:else}
								<div class="placeholder">
									<p>{$t('home.no_mutual_contributors')}</p>
								</div>
							{/if}
						</div>
					</div>
				</div>
			{/key}
		{/if}
	</div>

	{#if data.monthlyBudget}
		<div class="org-capacity-footer">
			<div class="capacity-info">
				<span class="budget-label">Monthly Distribution:</span>
				<div class="budget-input-wrapper">
					<span class="currency">$</span>
					<input type="number" bind:value={budgetValue} min="0" step="0.1" class="budget-input" />
					<select bind:value={budgetUnit} class="unit-select">
						<option value="K">K</option>
						<option value="M">M</option>
						<option value="B">B</option>
					</select>
					<span class="formatted-display">= ${formatBudget(getFullAmount())}</span>
				</div>
				<span class="separator">•</span>
				<span class="explainer">Allocated via mutual recognition</span>
			</div>
		</div>
	{/if}
</div>

<style>
	/* Removed conflicting :global(body) styles - handled by layout */

	/* Org page container - no scroll needed */
	.org-page-container {
		display: flex;
		flex-direction: column;
		height: 100%;
		width: 100%;
		overflow: hidden;
	}

	/* Org capacity footer - prominent and editable at bottom */
	.org-capacity-footer {
		border-top: 1px solid #e0e0e0;
		background: #fafafa;
		padding: 1rem 1.5rem;
		flex-shrink: 0;
	}

	.capacity-info {
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 1rem;
		font-size: 1.1rem;
		color: #666;
		flex-wrap: wrap;
	}

	.budget-label {
		font-weight: 600;
		font-size: 1.2rem;
	}

	.budget-input-wrapper {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		background: white;
		padding: 0.5rem 1rem;
		border-radius: 6px;
		border: 2px solid #e0e0e0;
		transition: border-color 0.2s;
	}

	.budget-input-wrapper:focus-within {
		border-color: #2196f3;
	}

	.currency {
		font-weight: 600;
		color: #2196f3;
		font-size: 1.3rem;
	}

	.budget-input {
		border: none;
		outline: none;
		font-size: 1.3rem;
		font-weight: 600;
		color: #333;
		width: 80px;
		padding: 0;
		background: transparent;
	}

	.budget-input::-webkit-inner-spin-button,
	.budget-input::-webkit-outer-spin-button {
		opacity: 1;
	}

	.unit-select {
		border: none;
		outline: none;
		font-size: 1.2rem;
		font-weight: 600;
		color: #2196f3;
		background: transparent;
		cursor: pointer;
		padding: 0;
		margin-left: 0.25rem;
	}

	.unit-select:focus {
		outline: none;
	}

	.formatted-display {
		font-size: 1rem;
		color: #888;
		font-weight: 500;
		padding-left: 0.5rem;
		margin-left: 0.5rem;
		border-left: 1px solid #e0e0e0;
	}

	.separator {
		color: #ccc;
		font-weight: bold;
		font-size: 1.2rem;
	}

	.explainer {
		font-style: italic;
		color: #888;
		font-size: 1rem;
	}

	@media (max-width: 768px) {
		.org-capacity-footer {
			padding: 0.75rem 1rem;
		}

		.capacity-info {
			font-size: 1rem;
			gap: 0.75rem;
		}

		.budget-label {
			font-size: 1rem;
		}

		.budget-input {
			width: 70px;
			font-size: 1.1rem;
		}

		.unit-select {
			font-size: 1rem;
		}

		.formatted-display {
			font-size: 0.9rem;
		}

		.explainer {
			font-size: 0.9rem;
		}
	}

	.layout {
		flex: 1;
		min-height: 0;
		display: grid;
		grid-template-columns: 9fr 1fr;
		width: 100%;
		overflow: hidden;
		user-select: none;
	}

	/* Full-width layout when bars are hidden */
	.layout.full-width {
		grid-template-columns: 1fr;
	}

	/* Org page specific: ensure it doesn't scroll */
	.layout.org-page {
		overflow: hidden;
		height: 100%;
		max-height: 100%;
		position: relative;
	}

	.view-content {
		width: 100%;
		height: 100%;
		overflow: auto;
	}

	.bars {
		width: 100%;
		height: 100%;
		overflow: hidden;
		display: flex;
		gap: 0.5rem;
		padding: 0.5rem;
		box-sizing: border-box;
		min-width: 0;
	}

	/* Mobile: Horizontal bars stacked vertically */
	@media (max-width: 768px) {
		.bars {
			flex-direction: column;
			height: auto;
		}

		.bar-group {
			display: grid;
			grid-template-columns: auto 1fr;
			gap: 0.75rem;
			align-items: center;
			height: 2rem;
			width: 100%;
			min-width: 0;
			overflow: hidden;
		}

		.bar-area {
			height: 100%;
			width: 100%;
			min-width: 0;
			max-width: 100%;
			overflow: hidden;
		}

		.bar-label {
			white-space: nowrap;
			overflow: hidden;
			text-overflow: ellipsis;
			flex-shrink: 0;
		}

		.label-mobile {
			display: inline;
		}

		.label-desktop {
			display: none;
		}
	}

	/* Desktop: Vertical bars side by side */
	@media (min-width: 769px) {
		.bars {
			flex-direction: row;
			height: 100%;
		}

		.bar-group {
			display: flex;
			flex-direction: column;
			width: 2rem;
			min-width: 2rem;
			max-width: 2rem;
			height: 100%;
			min-height: 0;
			max-height: 100%;
			gap: 0.25rem;
			overflow: hidden;
			flex-shrink: 0;
		}

		.bar-area {
			flex: 1;
			order: 1;
			display: flex;
			align-items: flex-end;
			width: 100%;
			min-height: 0;
			max-width: 100%;
			overflow: hidden;
		}

		.bar-label {
			order: 2;
			font-size: min(0.5em, 1vw);
			padding: 0 0.25rem;
			max-width: 100%;
			width: 100%;
			text-align: center;
			overflow: hidden;
			text-overflow: ellipsis;
			white-space: nowrap;
		}

		.label-mobile {
			display: none;
		}

		.label-desktop {
			display: inline;
		}
	}

	.bar-label {
		font-size: min(0.6em, 1.2vh);
		color: #666;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		font-weight: 500;
		line-height: 1.1;
		overflow: hidden;
		text-overflow: ellipsis;
	}

	.placeholder {
		height: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		text-align: center;
		color: #666;
		font-size: 0.9em;
		padding: 1rem;
		background: #f5f5f5;
		border-radius: 4px;
	}

	.inventory-view {
		padding: 1rem;
		overflow-y: auto;
		height: 100%;
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 1.5rem;
	}

	.inventory-view h2 {
		margin: 0 0 1rem 0;
		color: #2c3e50;
	}

	/* Slots Section Styles */
	.slots-section {
		background: white;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
		display: flex;
		flex-direction: column;
	}

	.slots-section.needs {
		border-top: 4px solid #3498db;
	}

	.slots-section.capacity {
		border-top: 4px solid #2ecc71;
	}

	.add-form {
		display: flex;
		gap: 0.5rem;
		margin-bottom: 1rem;
		flex-wrap: wrap;
	}

	.add-form input[type='text'] {
		flex: 1;
		min-width: 150px;
	}

	.add-form input,
	.add-form select {
		padding: 0.5rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 0.95rem;
	}

	.add-form input[type='number'] {
		width: 80px;
	}

	.slots-list {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		overflow-y: auto;
	}

	.quantity-control {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.quantity-control input {
		width: 80px;
		padding: 0.25rem 0.5rem;
		border: 1px solid #ddd;
		border-radius: 4px;
	}

	.empty-state {
		text-align: center;
		padding: 2rem;
		color: #999;
		font-style: italic;
	}

	.raw-data {
		margin-top: 0.75rem;
		font-size: 0.85rem;
	}

	.raw-data pre {
		background: #282c34;
		color: #abb2bf;
		padding: 1rem;
		border-radius: 4px;
		overflow-x: auto;
		max-height: 300px;
		overflow-y: auto;
	}

	.btn-primary {
		background: #3498db;
		color: white;
		border: none;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
		font-weight: 500;
	}

	.btn-primary:hover {
		background: #2980b9;
	}

	.btn-danger-small {
		background: #e74c3c;
		color: white;
		border: none;
		padding: 0.25rem 0.5rem;
		border-radius: 4px;
		cursor: pointer;
		font-size: 0.85rem;
	}

	.btn-danger-small:hover {
		background: #c0392b;
	}

	/* Slot Actions Row (used in Type component snippet) */
	.slot-actions-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
		flex-wrap: wrap;
	}

	.slot-actions-row .quantity-control {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.slot-actions-row .quantity-control label {
		font-size: 0.85rem;
		font-weight: 500;
		color: #666;
	}

	.slot-actions-row .quantity-control input {
		width: 80px;
		padding: 0.4rem;
		border: 1px solid #ddd;
		border-radius: 4px;
	}

	/* Responsive layout for mobile */
	@media (max-width: 768px) {
		.layout {
			grid-template-columns: 1fr;
			grid-template-rows: 1fr auto;
		}

		.bars {
			flex-direction: column;
			height: auto;
			gap: 0.5rem;
		}

		.placeholder {
			padding: 0.5rem;
			font-size: 0.8em;
		}

		.inventory-view {
			grid-template-columns: 1fr;
			gap: 1rem;
		}

		.add-form {
			flex-direction: column;
		}

		.add-form input[type='number'] {
			width: 100%;
		}
	}
	
	/* Loading state styles */
	.loading-container {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		min-height: 400px;
		gap: 1.5rem;
	}
	
	.loading-spinner {
		width: 48px;
		height: 48px;
		border: 4px solid rgba(0, 0, 0, 0.1);
		border-top-color: var(--primary-color, #4CAF50);
		border-radius: 50%;
		animation: spin 1s linear infinite;
	}
	
	@keyframes spin {
		to { transform: rotate(360deg); }
	}
	
	.loading-container p {
		color: var(--text-secondary, #666);
		font-size: 1rem;
		margin: 0;
	}
</style>
