import { writable, derived } from 'svelte/store';
import type { UserSlotComposition, NetworkSlotComposition } from '$lib/schema';
import {
	userCapacities,
	userNetworkCapacitiesWithSlotQuantities,
	contributorCapacityShares,
	networkCapacities,
	allocatedSlots,
	allocatedSlotAmounts,
	userDesiredSlotComposeFrom,
	userDesiredSlotComposeInto,
	networkDesiredSlotComposeFrom,
	networkDesiredSlotComposeInto
} from '$lib/state/core.svelte';
