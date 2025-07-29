<script lang="ts">
	import type { ProviderCapacity } from '$lib/schema';
	import Bar from './Bar.svelte';
	import TagPill from './TagPill.svelte';
	import DropDown from './DropDown.svelte';
	import Chat from './Chat.svelte';
	import CompositionItem from './CompositionItem.svelte';
	import { Rules, type JsonLogicRule } from '$lib/filters';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { get } from 'svelte/store';
	import {
		createCapacitiesDataProvider,
		createAllNetworkCapacitiesDataProvider,
		createSubtreesDataProvider,
		createContactsAndUsersDataProvider
	} from '$lib/utils/ui-providers.svelte';
	import {
		userDesiredComposeFrom,
		userDesiredComposeInto,
		networkDesiredComposeFrom,
		networkDesiredComposeInto
	} from '$lib/state/compose.svelte';
	import { globalState } from '$lib/global.svelte';
	import { ProviderCapacitySchema } from '$lib/schema';
	import { getReactiveUnreadCount } from '$lib/state/chat.svelte';
	import { userNamesOrAliasesCache } from '$lib/state/users.svelte';

	interface Props {
		capacity: ProviderCapacity;
		canDelete: boolean;
		onupdate?: (capacity: ProviderCapacity) => void;
		ondelete?: (id: string) => void;
	}

	let { capacity, canDelete, onupdate, ondelete }: Props = $props();

	// UI state for expanded capacity editing
	let expanded = $state(false);

	// UI state for expanded chat
	let chatExpanded = $state(false);

	// Get reactive unread message count for this capacity's chat
	let unreadCount = getReactiveUnreadCount(capacity.id);

	// UI state for expanded composition
	let compositionExpanded = $state(false);

	// UI state for expanded availability
	let availabilityExpanded = $state(false);

	// Filter state
	let selectedSubtrees = $state<string[]>([]);
	let selectedCapacities = $state<string[]>([]);
	let filterMode = $state<'include' | 'exclude'>('include');

	// Track if we're initializing from capacity to prevent loops
	let isInitializingFromCapacity = $state(false);

	// Initialize filter state from existing filter_rule
	$effect(() => {
		if (isInitializingFromCapacity) {
			return;
		}

		if (capacity.filter_rule) {
			const filters = extractFiltersFromRule(capacity.filter_rule);

			// Only update if the extracted values differ from current state
			if (
				JSON.stringify(filters.subtrees) !== JSON.stringify(selectedSubtrees) ||
				JSON.stringify(filters.capacities) !== JSON.stringify(selectedCapacities) ||
				filters.mode !== filterMode
			) {
				selectedSubtrees = filters.subtrees;
				selectedCapacities = filters.capacities;
				filterMode = filters.mode;
			}
		} else if (selectedSubtrees.length > 0 || selectedCapacities.length > 0) {
			// Clear filters if no rule exists
			selectedSubtrees = [];
			selectedCapacities = [];
			filterMode = 'include';
		}
	});

	// Dropdown states
	let showSubtreeDropdown = $state(false);
	let dropdownPosition = $state({ x: 0, y: 0 });
	let showCapacityDropdown = $state(false);
	let capacityDropdownPosition = $state({ x: 0, y: 0 });

	// Emoji picker state
	let showEmojiPicker = $state(false);
	let emojiPickerContainer: HTMLDivElement | undefined = $state();
	let emojiPickerElement: any = $state();

	// Create subtrees data provider for the dropdown
	let subtreesDataProvider = createSubtreesDataProvider();

	// Create contacts and users data provider for individual filtering
	let contactsAndUsersDataProvider = createContactsAndUsersDataProvider();

	// Create reactive capacity data providers for composition and dropdowns
	let composeFromDataProvider = createCapacitiesDataProvider(capacity.id); // Capacities we have shares in
	let composeIntoDataProvider = createAllNetworkCapacitiesDataProvider(capacity.id); // All network capacities except this one

	// Reactive capacity properties for proper binding (matching schema types)
	let capacityName = $state(capacity.name);
	let capacityEmoji = $state(capacity.emoji);
	let capacityQuantity = $state(capacity.quantity);
	let capacityUnit = $state(capacity.unit);
	let capacityDescription = $state(capacity.description);
	let capacityLocationType = $state(capacity.location_type);
	let capacityLongitude = $state(capacity.longitude);
	let capacityLatitude = $state(capacity.latitude);

	// Address fields
	let capacityStreetAddress = $state(capacity.street_address);
	let capacityCity = $state(capacity.city);
	let capacityStateProvince = $state(capacity.state_province);
	let capacityPostalCode = $state(capacity.postal_code);
	let capacityCountry = $state(capacity.country);

	let capacityAllDay = $state(capacity.all_day);
	let capacityStartDate = $state(capacity.start_date);
	let capacityEndDate = $state(capacity.end_date);
	let capacityStartTime = $state(capacity.start_time);
	let capacityEndTime = $state(capacity.end_time);
	let capacityTimeZone = $state(capacity.time_zone);
	let capacityRecurrence = $state(capacity.recurrence);
	let capacityCustomRecurrenceRepeatEvery = $state(capacity.custom_recurrence_repeat_every);
	let capacityCustomRecurrenceRepeatUnit = $state(capacity.custom_recurrence_repeat_unit);
	let capacityCustomRecurrenceEndType = $state(capacity.custom_recurrence_end_type);
	let capacityCustomRecurrenceEndValue = $state(capacity.custom_recurrence_end_value);
	let capacityMaxNaturalDiv = $state(capacity.max_natural_div);
	let capacityMaxPercentageDiv = $state(capacity.max_percentage_div);
	let capacityHiddenUntilRequestAccepted = $state(capacity.hidden_until_request_accepted);

	// Derived filter rule - automatically updates when filter state changes
	let filterRule = $derived(() => {
		// If no filters are selected, return null
		if (selectedSubtrees.length === 0 && selectedCapacities.length === 0) {
			return null;
		}

		let rules: JsonLogicRule[] = [];

		// Add subtree filters
		if (selectedSubtrees.length > 0) {
			// Convert reactive array to plain array
			const plainSubtrees = [...selectedSubtrees];
			if (plainSubtrees.length === 1) {
				rules.push(Rules.inSubtree(plainSubtrees[0]));
			} else {
				rules.push(Rules.inSubtrees(plainSubtrees));
			}
		}

		// Add individual capacity filters
		if (selectedCapacities.length > 0) {
			// Convert reactive array to plain array
			const plainCapacities = [...selectedCapacities];
			rules.push(Rules.includeNodes(plainCapacities));
		}

		// Combine rules
		let combinedRule: JsonLogicRule;
		if (rules.length === 1) {
			combinedRule = rules[0];
		} else {
			combinedRule = Rules.and(...rules);
		}

		// Apply exclude mode if needed
		if (filterMode === 'exclude') {
			combinedRule = Rules.not(combinedRule);
		}

		return combinedRule;
	});

	// Get subtree names for display
	let subtreeNames = $derived(() => {
		const names: Record<string, string> = {};
		selectedSubtrees.forEach((subtreeId) => {
			const item = $subtreesDataProvider.find((item) => item.id === subtreeId);
			if (item) {
				names[subtreeId] = item.name;
			}
		});
		return names;
	});

	// Load emoji picker client-side only
	onMount(async () => {
		if (browser) {
			try {
				// Dynamic import to avoid SSR issues
				await import('emoji-picker-element');
			} catch (error) {
				console.warn('Failed to load emoji picker:', error);
			}
		}
	});

	// Initialize emoji picker when container is available
	$effect(() => {
		if (emojiPickerContainer && browser && !emojiPickerElement) {
			try {
				const picker = document.createElement('emoji-picker');
				picker.style.position = 'absolute';
				picker.style.zIndex = '1000';
				picker.style.boxShadow = '0 10px 25px rgba(0, 0, 0, 0.2)';
				picker.style.border = '1px solid #e5e7eb';
				picker.style.borderRadius = '8px';
				picker.style.width = '320px';
				picker.style.height = '400px';

				// Listen for emoji selection
				picker.addEventListener('emoji-click', (event: any) => {
					capacityEmoji = event.detail.unicode;
					handleCapacityUpdate();
					showEmojiPicker = false;
				});

				emojiPickerElement = picker;
			} catch (error) {
				console.warn('Failed to create emoji picker:', error);
			}
		}
	});

	// Handle emoji picker visibility
	$effect(() => {
		if (emojiPickerElement && emojiPickerContainer) {
			if (showEmojiPicker) {
				emojiPickerContainer.appendChild(emojiPickerElement);
			} else if (emojiPickerElement.parentNode) {
				emojiPickerElement.parentNode.removeChild(emojiPickerElement);
			}
		}
	});

	// Convert recipient_shares to bar segments
	const recipientSegments = $derived(
		Object.entries(capacity.recipient_shares || {}).map(([userId, share]) => ({
			id: userId,
			value: share * 100 // Convert from 0-1 to 0-100 percentage
		}))
	);

	// Recurrence options
	const recurrenceOptions = [
		'Does not repeat',
		'Daily',
		'Weekly',
		'Monthly',
		'Annually',
		'Every weekday (Monday to Friday)',
		'Every 4 days',
		'Custom...'
	];

	// Helper to track original value on focus
	function handleFocus(fieldName: string, currentValue: any) {
		originalValues[fieldName] = currentValue;
	}

	// Helper to save only if value changed on blur
	function handleBlurIfChanged(fieldName: string, currentValue: any) {
		if (originalValues[fieldName] !== currentValue) {
			handleCapacityUpdate();
		}
	}

	// Handler for input events that updates capacity
	function handleCapacityUpdate() {
		try {
			// Set flag to prevent effect loop
			isInitializingFromCapacity = true;

			// Create updated capacity with current filter rule
			const updatedCapacity = {
				...capacity,
				name: capacityName,
				emoji: capacityEmoji,
				quantity: capacityQuantity,
				unit: capacityUnit,
				description: capacityDescription,
				location_type: capacityLocationType,
				longitude: capacityLongitude,
				latitude: capacityLatitude,

				// Address fields
				street_address: capacityStreetAddress,
				city: capacityCity,
				state_province: capacityStateProvince,
				postal_code: capacityPostalCode,
				country: capacityCountry,

				all_day: capacityAllDay,
				start_date: capacityStartDate,
				end_date: capacityEndDate,
				start_time: capacityStartTime,
				end_time: capacityEndTime,
				time_zone: capacityTimeZone,
				recurrence: capacityRecurrence,
				custom_recurrence_repeat_every: capacityCustomRecurrenceRepeatEvery,
				custom_recurrence_repeat_unit: capacityCustomRecurrenceRepeatUnit,
				custom_recurrence_end_type: capacityCustomRecurrenceEndType,
				custom_recurrence_end_value: capacityCustomRecurrenceEndValue,
				max_natural_div: capacityMaxNaturalDiv,
				max_percentage_div: capacityMaxPercentageDiv,
				hidden_until_request_accepted: capacityHiddenUntilRequestAccepted,
				filter_rule: filterRule()
			};

			// Validate using schema
			const validationResult = ProviderCapacitySchema.safeParse(updatedCapacity);

			if (!validationResult.success) {
				// Check for specific validation errors and show appropriate messages
				const errors = validationResult.error.issues;
				const unitErrors = errors.filter((issue) => issue.path.includes('unit'));

				if (unitErrors.length > 0) {
					globalState.showToast(unitErrors[0].message, 'warning');
					// Revert unit to previous valid value
					capacityUnit = capacity.unit || '';
					return;
				}

				// Handle other validation errors
				globalState.showToast('Invalid capacity data', 'error');
				console.error('Capacity validation failed:', validationResult.error);
				return;
			}

			// Validation passed, proceed with update
			const updatePromise = onupdate?.(validationResult.data);
			if (updatePromise instanceof Promise) {
				updatePromise.finally(() => {
					isInitializingFromCapacity = false;
				});
			} else {
				isInitializingFromCapacity = false;
			}
		} catch (error) {
			console.error('Error updating capacity:', error);
			globalState.showToast('Failed to update capacity', 'error');
	}

	// Delete this capacity
	function handleDelete() {
		ondelete?.(capacity.id);
	}

	// Toggle expanded state
	function toggleExpanded() {
		expanded = !expanded;
		// If we're expanding settings, close chat, composition, and availability
		if (expanded) {
			chatExpanded = false;
			compositionExpanded = false;
			availabilityExpanded = false;
		}
	}

	// Toggle chat state
	function toggleChat() {
		chatExpanded = !chatExpanded;
		// If we're expanding chat, close settings, composition, and availability
		if (chatExpanded) {
			expanded = false;
			compositionExpanded = false;
			availabilityExpanded = false;
		}
	}

	// Toggle composition state
	function toggleComposition() {
		compositionExpanded = !compositionExpanded;
		// If we're expanding composition, close settings, chat, and availability
		if (compositionExpanded) {
			expanded = false;
			chatExpanded = false;
			availabilityExpanded = false;
		}
	}

	// Toggle availability state
	function toggleAvailability() {
		availabilityExpanded = !availabilityExpanded;
		// If we're expanding availability, close settings, chat, and composition
		if (availabilityExpanded) {
			expanded = false;
			chatExpanded = false;
			compositionExpanded = false;
		}
	}

	// Handle adding a subtree filter
	function handleAddSubtreeFilter(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		dropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showSubtreeDropdown = true;
	}

	// Handle subtree selection from dropdown
	function handleSubtreeSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: subtreeId } = detail;

		// Don't add if already selected
		if (selectedSubtrees.includes(subtreeId)) {
			showSubtreeDropdown = false;
			return;
		}

		// Add to selected subtrees
		selectedSubtrees = [...selectedSubtrees, subtreeId];

		// Update capacity
		handleCapacityUpdate();

		// Close dropdown
		showSubtreeDropdown = false;
	}

	// Handle removing a subtree filter
	function handleRemoveSubtree(subtreeId: string) {
		selectedSubtrees = selectedSubtrees.filter((id) => id !== subtreeId);
		handleCapacityUpdate();
	}

	// Handle adding a capacity filter
	function handleAddCapacityFilter(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		capacityDropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showCapacityDropdown = true;
	}

	// Handle capacity selection from dropdown
	function handleCapacitySelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: capacityId } = detail;

		if (selectedCapacities.includes(capacityId)) {
			showCapacityDropdown = false;
			return;
		}

		selectedCapacities = [...selectedCapacities, capacityId];
		handleCapacityUpdate();
		showCapacityDropdown = false;
	}

	// Handle removing a capacity filter
	function handleRemoveCapacity(capacityId: string) {
		selectedCapacities = selectedCapacities.filter((id) => id !== capacityId);
		handleCapacityUpdate();
	}

	// Close dropdowns
	function handleDropdownClose() {
		showSubtreeDropdown = false;
	}

	function handleCapacityDropdownClose() {
		showCapacityDropdown = false;
	}

	// Handle emoji picker
	function handleEmojiPickerToggle(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();
		showEmojiPicker = !showEmojiPicker;
	}

	// Close emoji picker when clicking outside
	function handleClickOutside(event: MouseEvent) {
		if (emojiPickerContainer && !emojiPickerContainer.contains(event.target as Node)) {
			showEmojiPicker = false;
		}
	}

	// Helper function to extract subtree IDs from a rule (legacy compatibility)
	function extractSubtreeIdsFromRule(rule: JsonLogicRule): string[] {
		const result = extractFiltersFromRule(rule);
		return result.subtrees;
	}

	function extractFiltersFromRule(rule: JsonLogicRule | null | undefined): {
		subtrees: string[];
		capacities: string[];
		mode: 'include' | 'exclude';
	} {
		if (!rule) return { subtrees: [], capacities: [], mode: 'include' };

		let subtrees: string[] = [];
		let capacities: string[] = [];
		let mode: 'include' | 'exclude' = 'include';

		// Handle NOT rules (exclude mode)
		if ('!' in rule && rule['!']) {
			mode = 'exclude';
			rule = rule['!'] as JsonLogicRule;
		}

		// Handle AND rules (multiple filters)
		if ('and' in rule && rule.and && Array.isArray(rule.and)) {
			(rule.and as JsonLogicRule[]).forEach((subRule: JsonLogicRule) => {
				const subResult = extractFiltersFromRule(subRule);
				subtrees.push(...subResult.subtrees);
				capacities.push(...subResult.capacities);
			});
			return { subtrees, capacities, mode };
		}

		// Handle single subtree rule
		if ('in' in rule && rule.in && Array.isArray(rule.in) && rule.in.length === 2) {
			const secondArg = rule.in[1];
			if (
				typeof secondArg === 'object' &&
				secondArg !== null &&
				'var' in secondArg &&
				Array.isArray(secondArg.var) &&
				secondArg.var[0] === 'subtreeContributors'
			) {
				subtrees.push(secondArg.var[1] as string);
			} else if (Array.isArray(secondArg)) {
				capacities.push(...(secondArg as string[]));
			}
		}

		// Handle multiple subtree rule
		if ('some' in rule && rule.some && Array.isArray(rule.some) && rule.some.length === 2) {
			const subtreeIds = rule.some[0];
			if (Array.isArray(subtreeIds)) {
				subtrees.push(...(subtreeIds as string[]));
			}
		}

		return { subtrees, capacities, mode };
	}

	// Add click outside listener
	$effect(() => {
		if (showEmojiPicker) {
			document.addEventListener('click', handleClickOutside);
			return () => {
				document.removeEventListener('click', handleClickOutside);
			};
		}
	});

	// Format date for input
	function formatDateForInput(date: Date | undefined): string {
		if (!date) return '';
		return date.toISOString().split('T')[0];
	}

	// Helper functions for map marker integration
	export function hasValidCoordinates(): boolean {
		return (
			typeof capacityLatitude === 'number' &&
			typeof capacityLongitude === 'number' &&
			capacityLatitude >= -90 &&
			capacityLatitude <= 90 &&
			capacityLongitude >= -180 &&
			capacityLongitude <= 180
		);
	}

	export function getLngLat(): [number, number] | null {
		if (!hasValidCoordinates()) return null;
		return [capacityLongitude!, capacityLatitude!];
	}

	export function updateCoordinatesFromMarker(lnglat: { lng: number; lat: number }) {
		capacityLongitude = lnglat.lng;
		capacityLatitude = lnglat.lat;
		handleCapacityUpdate();
	}

	// Track capacities we want to compose from (our desires)
	let composeFromCapacities = $derived(() => {
		const ourCapacityDesires = $userDesiredComposeFrom[capacity.id];
		const result = ourCapacityDesires ? Object.keys(ourCapacityDesires) : [];
		console.log(
			`[CAPACITY-${capacity.id}] composeFromCapacities:`,
			result,
			'from store:',
			$userDesiredComposeFrom
		);
		return result;
	});

	// Track capacities that want to compose into our capacity (their desires)
	let networkComposeIntoCapacities = $derived(() => {
		const result = new Set<string>();

		// Look through network desires to find who wants to compose into our capacity
		Object.entries($networkDesiredComposeInto).forEach(([providerId, providerDesires]) => {
			Object.entries(providerDesires).forEach(([theirCapacityId, theirDesires]) => {
				if (theirDesires[capacity.id]) {
					result.add(theirCapacityId);
				}
			});
		});

		console.log(
			`[CAPACITY-${capacity.id}] networkComposeIntoCapacities:`,
			Array.from(result),
			'from store:',
			$networkDesiredComposeInto
		);
		return Array.from(result);
	});

	// Track capacities we want to compose into (our desires)
	let composeIntoCapacities = $derived(() => {
		const ourCapacityDesires = $userDesiredComposeInto[capacity.id];
		const result = ourCapacityDesires ? Object.keys(ourCapacityDesires) : [];
		console.log(
			`[CAPACITY-${capacity.id}] composeIntoCapacities:`,
			result,
			'from store:',
			$userDesiredComposeInto
		);
		return result;
	});

	// Track capacities that want to compose from our capacity (their desires)
	let networkComposeFromCapacities = $derived(() => {
		const result = new Set<string>();

		// Look through network desires to find who wants to compose from our capacity
		Object.entries($networkDesiredComposeFrom).forEach(([providerId, providerDesires]) => {
			Object.entries(providerDesires).forEach(([theirCapacityId, theirDesires]) => {
				if (theirDesires[capacity.id]) {
					result.add(theirCapacityId);
				}
			});
		});

		console.log(
			`[CAPACITY-${capacity.id}] networkComposeFromCapacities:`,
			Array.from(result),
			'from store:',
			$networkDesiredComposeFrom
		);
		return Array.from(result);
	});

	// State for tracking expanded composition items
	let expandedCompositions = $state<Record<string, boolean>>({});

	// State for description field expansion
	let descriptionExpanded = $state(false);

	// Location format state ('coordinates' or 'address')
	let locationFormat = $state<'coordinates' | 'address'>('address');

	// Track original values for change detection
	let originalValues = $state<Record<string, any>>({});

	// Dropdown states for adding capacities
	let showComposeFromDropdown = $state(false);
	let showComposeIntoDropdown = $state(false);
	let composeFromDropdownPosition = $state({ x: 0, y: 0 });
	let composeIntoDropdownPosition = $state({ x: 0, y: 0 });

	// Toggle composition expansion
	function toggleCompositionItem(compositionKey: string) {
		expandedCompositions[compositionKey] = !expandedCompositions[compositionKey];
	}

	// Handle adding capacity to compose from
	function handleAddComposeFrom(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		composeFromDropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showComposeFromDropdown = true;
	}

	// Handle adding capacity to compose into
	function handleAddComposeInto(event: MouseEvent) {
		event.preventDefault();
		event.stopPropagation();

		const rect = (event.target as HTMLElement).getBoundingClientRect();
		composeIntoDropdownPosition = {
			x: rect.left,
			y: rect.bottom + 5
		};

		showComposeIntoDropdown = true;
	}

	// Handle selecting capacity for compose from
	function handleComposeFromSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: capacityId } = detail;

		// Add to store with initial quantity of 0
		const current = get(userDesiredComposeFrom);
		const updated = {
			...current,
			[capacity.id]: {
				...current[capacity.id],
				[capacityId]: 0
			}
		};
		userDesiredComposeFrom.set(updated);

		showComposeFromDropdown = false;
	}

	// Handle selecting capacity for compose into
	function handleComposeIntoSelect(detail: { id: string; name: string; metadata?: any }) {
		const { id: capacityId } = detail;

		// Add to store with initial quantity of 0
		const current = get(userDesiredComposeInto);
		const updated = {
			...current,
			[capacity.id]: {
				...current[capacity.id],
				[capacityId]: 0
			}
		};
		userDesiredComposeInto.set(updated);

		showComposeIntoDropdown = false;
	}

	// Remove capacity from compose from list
	function removeComposeFrom(capacityId: string) {
		const current = get(userDesiredComposeFrom);
		if (current[capacity.id]) {
			const { [capacityId]: _, ...remainingDesires } = current[capacity.id];
			const updated = {
				...current,
				[capacity.id]: remainingDesires
			};
			userDesiredComposeFrom.set(updated);
		}
	}

	// Remove capacity from compose into list
	function removeComposeInto(capacityId: string) {
		const current = get(userDesiredComposeInto);
		if (current[capacity.id]) {
			const { [capacityId]: _, ...remainingDesires } = current[capacity.id];
			const updated = {
				...current,
				[capacity.id]: remainingDesires
			};
			userDesiredComposeInto.set(updated);
		}
	}

	// Close dropdowns
	function handleComposeFromDropdownClose() {
		showComposeFromDropdown = false;
	}

	function handleComposeIntoDropdownClose() {
		showComposeIntoDropdown = false;
	}
</script>

<div class="capacity-item" class:chat-expanded={chatExpanded}>
	<!-- Recipient shares bar -->
	{#if recipientSegments.length > 0}
		<div class="recipient-shares-bar mb-1">
			<Bar
				segments={recipientSegments}
				height="8px"
				rounded={true}
				backgroundColor="#f3f4f6"
				showLabelsAboveOnSelect={true}
			/>
		</div>
	{/if}

	<div class="capacity-row flex flex-wrap items-center gap-2 rounded bg-white p-2 shadow-sm">
		<!-- Emoji picker button -->
		<div class="relative">
			<button
				type="button"
				class="emoji-btn"
				onclick={handleEmojiPickerToggle}
				title="Select emoji"
			>
				{capacityEmoji || '📦'}
			</button>
			<!-- Emoji picker container -->
			{#if showEmojiPicker}
				<div bind:this={emojiPickerContainer} class="emoji-picker-container"></div>
			{/if}
		</div>

		<!-- Main capacity inputs -->
		<input
			type="text"
			class="capacity-input name auto-size"
			bind:value={capacityName}
			placeholder="Name"
			onfocus={() => handleFocus('name', capacityName)}
			onblur={() => handleBlurIfChanged('name', capacityName)}
			style="width: {Math.max(capacityName?.length || 0, 'Name'.length) +
				3}ch; min-width: {Math.max(6, 'Name'.length + 2)}ch;"
		/>
		<input
			type="number"
			class="capacity-input qty auto-size text-right"
			min="0"
			step="0.01"
			bind:value={capacityQuantity}
			placeholder="Qty"
			onfocus={() => handleFocus('quantity', capacityQuantity)}
			onblur={() => handleBlurIfChanged('quantity', capacityQuantity)}
			style="width: {Math.max(capacityQuantity?.toString().length || 0, 'Qty'.length) +
				3}ch; min-width: {Math.max(5, 'Qty'.length + 2)}ch;"
		/>
		<input
			type="text"
			class="capacity-input unit auto-size"
			bind:value={capacityUnit}
			placeholder="Unit"
			onfocus={() => handleFocus('unit', capacityUnit)}
			onblur={() => handleBlurIfChanged('unit', capacityUnit)}
			style="width: {Math.max(capacityUnit?.length || 0, 'Unit'.length) +
				3}ch; min-width: {Math.max(6, 'Unit'.length + 2)}ch;"
		/>
		<!-- Description field with integrated toggle -->
		<div class="description-field-container">
			{#if descriptionExpanded}
				<textarea
					class="capacity-input description-textarea auto-size"
					bind:value={capacityDescription}
					placeholder="Description"
					onfocus={() => handleFocus('description', capacityDescription)}
					onblur={() => handleBlurIfChanged('description', capacityDescription)}
					rows="3"
				></textarea>
			{:else}
				<input
					type="text"
					class="capacity-input description auto-size"
					bind:value={capacityDescription}
					placeholder="Description"
					onfocus={() => handleFocus('description', capacityDescription)}
					onblur={() => handleBlurIfChanged('description', capacityDescription)}
					style="width: {Math.max(capacityDescription?.length || 0, 'Description'.length) +
						3}ch; min-width: {Math.max(12, 'Description'.length + 2)}ch;"
				/>
			{/if}
			{#if capacityDescription}
				<button
					type="button"
					class="description-expand-btn"
					onclick={() => (descriptionExpanded = !descriptionExpanded)}
					title={descriptionExpanded ? 'Collapse' : 'Expand'}
				>
					{descriptionExpanded ? '▼' : '▲'}
				</button>
			{/if}
		</div>

		<!-- Action buttons -->
		<button
			type="button"
			class="chat-btn relative ml-1"
			onclick={toggleChat}
			title="Chat about this capacity"
		>
			💬
			{#if $unreadCount > 0}
				<span class="unread-badge">{$unreadCount > 99 ? '99+' : $unreadCount}</span>
			{/if}
		</button>
		<button
			type="button"
			class="composition-btn ml-1"
			onclick={toggleComposition}
			title="Compose with other capacities"
		>
			🔄
		</button>
		<button
			type="button"
			class="availability-btn ml-1"
			onclick={toggleAvailability}
			title="Set availability and scheduling"
		>
			📅
		</button>
		<button type="button" class="settings-btn ml-1" onclick={toggleExpanded}> ⚙️ </button>
		<button type="button" class="remove-btn ml-1" onclick={handleDelete} disabled={!canDelete}
			>✖️</button
		>
	</div>

	<!-- Filter tags section -->
	<div class="filter-section mt-2">
		<div class="filter-header">
			<!-- Include/Exclude toggle -->
			{#if selectedSubtrees.length > 0 || selectedCapacities.length > 0}
				<div class="filter-mode-toggle">
					<button
						type="button"
						class="mode-btn"
						class:active={filterMode === 'include'}
						onclick={() => {
							filterMode = 'include';
							handleCapacityUpdate();
						}}
					>
						Include
					</button>
					<button
						type="button"
						class="mode-btn"
						class:active={filterMode === 'exclude'}
						onclick={() => {
							filterMode = 'exclude';
							handleCapacityUpdate();
						}}
					>
						Exclude
					</button>
				</div>
			{/if}

			<!-- Add filter buttons -->
			<div class="filter-button-container">
				<button type="button" class="add-filter-btn" onclick={handleAddSubtreeFilter}>
					<span class="add-icon">+</span>
					<span class="add-text">Subtree</span>
				</button>
				<button type="button" class="add-filter-btn" onclick={handleAddCapacityFilter}>
					<span class="add-icon">+</span>
					<span class="add-text">Individual</span>
				</button>
			</div>
		</div>

		<div class="filter-layout">
			<!-- Filter tags -->
			{#if selectedSubtrees.length > 0 || selectedCapacities.length > 0}
				<div class="filter-tags-container">
					{#each selectedSubtrees as subtreeId}
						<div class="filter-tag-wrapper">
							<TagPill
								userId={subtreeId}
								displayName={subtreeNames()[subtreeId] || subtreeId}
								truncateLength={15}
								removable={true}
								onClick={() => {}}
								onRemove={() => handleRemoveSubtree(subtreeId)}
							/>
						</div>
					{/each}
					{#each selectedCapacities as capacityId}
						<div class="filter-tag-wrapper">
							<TagPill
								userId={capacityId}
								displayName={$userNamesOrAliasesCache[capacityId] ||
									capacityId.substring(0, 8) + '...'}
								truncateLength={15}
								removable={true}
								onClick={() => {}}
								onRemove={() => handleRemoveCapacity(capacityId)}
							/>
						</div>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Filter description -->
		{#if selectedSubtrees.length > 0 || selectedCapacities.length > 0}
			<div class="filter-description">
				<span class="filter-desc-text">
					{filterMode === 'include' ? 'Only' : 'Exclude'} contributors
					{#if selectedSubtrees.length > 0 && selectedCapacities.length > 0}
						from {selectedSubtrees.length} subtree{selectedSubtrees.length > 1 ? 's' : ''} and {selectedCapacities.length}
						individual{selectedCapacities.length > 1 ? 's' : ''}
					{:else if selectedSubtrees.length > 0}
						from {selectedSubtrees.length === 1
							? 'this subtree'
							: `${selectedSubtrees.length} subtrees`}
					{:else}
						{selectedCapacities.length === 1
							? 'this individual'
							: `${selectedCapacities.length} individuals`}
					{/if}
				</span>
			</div>
		{/if}
	</div>

	<!-- Expanded chat section -->
	{#if chatExpanded}
		<div class="chat-container rounded border border-gray-200 bg-gray-50 p-3">
			<div class="chat-header mb-2">
				<h4 class="text-sm font-medium text-gray-700">
					💬 Chat about {capacity.emoji || '📦'}
					{capacity.name}
				</h4>
				<p class="mt-1 text-xs text-gray-500">
					Discuss this capacity with recipients and other stakeholders
				</p>
			</div>
			<Chat chatId={capacity.id} placeholder={`Discuss ${capacity.name}...`} maxLength={200} />
		</div>
	{/if}

	<!-- Expanded composition section -->
	{#if compositionExpanded}
		<div class="composition-section mb-4 rounded border border-gray-200 bg-blue-50 p-3">
			<div class="composition-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">🔄 Composition</h4>
				<p class="mt-1 text-xs text-gray-500">
					Enhance {capacity.emoji || '📦'}
					{capacity.name} by composing with other capacities
				</p>
			</div>

			<div class="composition-columns grid grid-cols-1 gap-4 md:grid-cols-2">
				<!-- Compose FROM column -->
				<div class="compose-from-column">
					<div class="column-header mb-3">
						<h5 class="flex items-center gap-2 text-sm font-medium text-gray-700">
							<span>FROM</span>
						</h5>
						<p class="mt-1 text-xs text-gray-500">Use their capacities to enhance yours</p>
					</div>

					<div class="column-content">
						<!-- Add capacity button -->
						<button type="button" class="add-capacity-btn mb-3" onclick={handleAddComposeFrom}>
							<span class="add-icon">+</span>
							<span class="add-text">Add capacity to compose from</span>
						</button>

						<!-- Compose from items -->
						{#if composeFromCapacities().length > 0 || networkComposeIntoCapacities().length > 0}
							<div class="composition-items space-y-2">
								<!-- Show items we want to compose from -->
								{#each composeFromCapacities() as capacityId}
									{@const theirCapacity = $composeFromDataProvider.find(
										(item) => item.id === capacityId
									)?.metadata}
									{#if theirCapacity}
										{@const compositionKey = `${capacity.id}-${capacityId}-from`}
										<CompositionItem
											ourCapacity={{
												id: capacity.id,
												name: capacity.name,
												emoji: capacity.emoji,
												unit: capacity.unit || 'unit',
												quantity: capacity.quantity || 0,
												provider_name: 'You'
											}}
											theirCapacity={{
												...theirCapacity,
												id: capacityId,
												unit: theirCapacity.unit || 'unit',
												quantity: theirCapacity.quantity || 0,
												provider_id:
													(theirCapacity as any).owner_id ||
													(theirCapacity as any).provider_id ||
													'unknown'
											}}
											direction="from"
											expanded={expandedCompositions[compositionKey] || false}
											onToggle={() => toggleCompositionItem(compositionKey)}
										/>
									{/if}
								{/each}

								<!-- Show items that want to compose into our capacity -->
								{#each networkComposeIntoCapacities() as capacityId}
									{#if !composeFromCapacities().includes(capacityId)}
										{@const theirCapacity = $composeFromDataProvider.find(
											(item) => item.id === capacityId
										)?.metadata}
										{#if theirCapacity}
											{@const compositionKey = `${capacity.id}-${capacityId}-from`}
											<CompositionItem
												ourCapacity={{
													id: capacity.id,
													name: capacity.name,
													emoji: capacity.emoji,
													unit: capacity.unit || 'unit',
													quantity: capacity.quantity || 0,
													provider_name: 'You'
												}}
												theirCapacity={{
													...theirCapacity,
													id: capacityId,
													unit: theirCapacity.unit || 'unit',
													quantity: theirCapacity.quantity || 0,
													provider_id:
														(theirCapacity as any).owner_id ||
														(theirCapacity as any).provider_id ||
														'unknown'
												}}
												direction="from"
												expanded={expandedCompositions[compositionKey] || false}
												onToggle={() => toggleCompositionItem(compositionKey)}
											/>
										{/if}
									{/if}
								{/each}
							</div>
						{:else}
							<div class="empty-state py-4 text-center text-xs text-gray-500 italic">
								No capacities selected for composition
							</div>
						{/if}
					</div>
				</div>

				<!-- Compose INTO column -->
				<div class="compose-into-column">
					<div class="column-header mb-3">
						<h5 class="flex items-center gap-2 text-sm font-medium text-gray-700">
							<span>INTO</span>
						</h5>
						<p class="mt-1 text-xs text-gray-500">Use your capacity to enhance theirs</p>
					</div>

					<div class="column-content">
						<!-- Add capacity button -->
						<button type="button" class="add-capacity-btn mb-3" onclick={handleAddComposeInto}>
							<span class="add-icon">+</span>
							<span class="add-text">Add capacity to compose into</span>
						</button>

						<!-- Compose into items -->
						{#if composeIntoCapacities().length > 0 || networkComposeFromCapacities().length > 0}
							<div class="composition-items space-y-2">
								<!-- Show items we want to compose into -->
								{#each composeIntoCapacities() as capacityId}
									{@const theirCapacity = $composeIntoDataProvider.find(
										(item) => item.id === capacityId
									)?.metadata}
									{#if theirCapacity}
										{@const compositionKey = `${capacity.id}-${capacityId}-into`}
										<CompositionItem
											ourCapacity={{
												id: capacity.id,
												name: capacity.name,
												emoji: capacity.emoji,
												unit: capacity.unit || 'unit',
												quantity: capacity.quantity || 0,
												provider_name: 'You'
											}}
											theirCapacity={{
												...theirCapacity,
												id: capacityId,
												unit: theirCapacity.unit || 'unit',
												quantity: theirCapacity.quantity || 0,
												provider_id:
													(theirCapacity as any).owner_id ||
													(theirCapacity as any).provider_id ||
													'unknown'
											}}
											direction="into"
											expanded={expandedCompositions[compositionKey] || false}
											onToggle={() => toggleCompositionItem(compositionKey)}
										/>
									{/if}
								{/each}

								<!-- Show items that want to compose from our capacity -->
								{#each networkComposeFromCapacities() as capacityId}
									{#if !composeIntoCapacities().includes(capacityId)}
										{@const theirCapacity = $composeIntoDataProvider.find(
											(item) => item.id === capacityId
										)?.metadata}
										{#if theirCapacity}
											{@const compositionKey = `${capacity.id}-${capacityId}-into`}
											<CompositionItem
												ourCapacity={{
													id: capacity.id,
													name: capacity.name,
													emoji: capacity.emoji,
													unit: capacity.unit || 'unit',
													quantity: capacity.quantity || 0,
													provider_name: 'You'
												}}
												theirCapacity={{
													...theirCapacity,
													id: capacityId,
													unit: theirCapacity.unit || 'unit',
													quantity: theirCapacity.quantity || 0,
													provider_id:
														(theirCapacity as any).owner_id ||
														(theirCapacity as any).provider_id ||
														'unknown'
												}}
												direction="into"
												expanded={expandedCompositions[compositionKey] || false}
												onToggle={() => toggleCompositionItem(compositionKey)}
											/>
										{/if}
									{/if}
								{/each}
							</div>
						{:else}
							<div class="empty-state py-4 text-center text-xs text-gray-500 italic">
								No capacities selected for composition
							</div>
						{/if}
					</div>
				</div>
			</div>
		</div>
	{/if}

	<!-- Expanded availability section -->
	{#if availabilityExpanded}
		<div class="availability-section mt-2 mb-4 rounded-md bg-green-50 p-3 shadow-sm">
			<div class="availability-header mb-3">
				<h4 class="text-sm font-medium text-gray-700">📅 Availability</h4>
				<p class="mt-1 text-xs text-gray-500">
					Set location, timing, and scheduling for this capacity
				</p>
			</div>

			<div class="space-time-options mb-6 ml-2">
				<div class="flex flex-wrap gap-x-8 gap-y-3">
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{capacity.id}"
							value="Undefined"
							bind:group={capacityLocationType}
							class="mr-3"
							onchange={handleCapacityUpdate}
						/>
						<span class="text-sm text-gray-600">Undefined</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{capacity.id}"
							value="LiveLocation"
							bind:group={capacityLocationType}
							class="mr-3"
							onchange={handleCapacityUpdate}
						/>
						<span class="text-sm text-gray-600">Live location</span>
					</label>
					<label class="inline-flex items-center">
						<input
							type="radio"
							name="location-type-{capacity.id}"
							value="Specific"
							bind:group={capacityLocationType}
							class="mr-3"
							onchange={handleCapacityUpdate}
						/>
						<span class="text-sm text-gray-600">Specific</span>
					</label>
				</div>
			</div>

			{#if capacityLocationType === 'LiveLocation' || capacityLocationType === 'Specific'}
				<div class="hidden-option mb-4 ml-2">
					<label class="inline-flex items-center">
						<input
							type="checkbox"
							bind:checked={capacityHiddenUntilRequestAccepted}
							class="mr-3 h-4 w-4"
							onchange={handleCapacityUpdate}
						/>
						<span class="text-sm font-medium text-gray-600">Hidden till Request Accepted</span>
					</label>
				</div>
			{/if}

			{#if capacityLocationType === 'Specific'}
				<div class="date-time-section mb-6 ml-2">
					<!-- Location input type toggle -->
					<div class="mb-6">
						<h5 class="mb-4 text-sm font-medium text-gray-600">Location Format</h5>
						<div class="mb-4 flex flex-wrap gap-x-8 gap-y-3">
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-format-{capacity.id}"
									value="address"
									checked={locationFormat === 'address'}
									class="mr-3"
									onchange={() => {
										locationFormat = 'address';
									}}
								/>
								<span class="text-sm text-gray-600">Address</span>
							</label>
							<label class="inline-flex items-center">
								<input
									type="radio"
									name="location-format-{capacity.id}"
									value="coordinates"
									checked={locationFormat === 'coordinates'}
									class="mr-3"
									onchange={() => {
										locationFormat = 'coordinates';
									}}
								/>
								<span class="text-sm text-gray-600">Coordinates</span>
							</label>
						</div>
					</div>

					<!-- Address section -->
					{#if locationFormat === 'address'}
						<div class="address-section mb-6">
							<h5 class="mb-4 text-sm font-medium text-gray-600">Address</h5>
							<div class="space-y-4">
								<div>
									<label class="mb-2 block text-xs text-gray-500">Street Address</label>
									<input
										type="text"
										class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacityStreetAddress}
										placeholder="e.g. 123 Main St"
										onfocus={() => handleFocus('streetAddress', capacityStreetAddress)}
										onblur={() => handleBlurIfChanged('streetAddress', capacityStreetAddress)}
									/>
								</div>
								<div class="grid grid-cols-2 gap-4">
									<div>
										<label class="mb-2 block text-xs text-gray-500">City</label>
										<input
											type="text"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityCity}
											placeholder="e.g. San Francisco"
											onfocus={() => handleFocus('city', capacityCity)}
											onblur={() => handleBlurIfChanged('city', capacityCity)}
										/>
									</div>
									<div>
										<label class="mb-2 block text-xs text-gray-500">State/Province</label>
										<input
											type="text"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityStateProvince}
											placeholder="e.g. CA"
											onfocus={() => handleFocus('stateProvince', capacityStateProvince)}
											onblur={() => handleBlurIfChanged('stateProvince', capacityStateProvince)}
										/>
									</div>
								</div>
								<div class="grid grid-cols-2 gap-4">
									<div>
										<label class="mb-2 block text-xs text-gray-500">Postal Code</label>
										<input
											type="text"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityPostalCode}
											placeholder="e.g. 94102"
											onfocus={() => handleFocus('postalCode', capacityPostalCode)}
											onblur={() => handleBlurIfChanged('postalCode', capacityPostalCode)}
										/>
									</div>
									<div>
										<label class="mb-2 block text-xs text-gray-500">Country</label>
										<input
											type="text"
											class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
											bind:value={capacityCountry}
											placeholder="e.g. United States"
											onfocus={() => handleFocus('country', capacityCountry)}
											onblur={() => handleBlurIfChanged('country', capacityCountry)}
										/>
									</div>
								</div>
							</div>
						</div>
					{/if}

					<!-- Geographic coordinates section -->
					{#if locationFormat === 'coordinates'}
						<div class="coordinates-section mb-6">
							<h5 class="mb-4 text-sm font-medium text-gray-600">Geographic Coordinates</h5>
							<div class="grid grid-cols-2 gap-4">
								<div>
									<label class="mb-2 block text-xs text-gray-500">Latitude (-90 to 90)</label>
									<input
										type="number"
										min="-90"
										max="90"
										step="0.000001"
										class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacityLatitude}
										placeholder="e.g. 37.7749"
										onfocus={() => handleFocus('latitude', capacityLatitude)}
										onblur={() => handleBlurIfChanged('latitude', capacityLatitude)}
									/>
								</div>
								<div>
									<label class="mb-2 block text-xs text-gray-500">Longitude (-180 to 180)</label>
									<input
										type="number"
										min="-180"
										max="180"
										step="0.000001"
										class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
										bind:value={capacityLongitude}
										placeholder="e.g. -122.4194"
										onfocus={() => handleFocus('longitude', capacityLongitude)}
										onblur={() => handleBlurIfChanged('longitude', capacityLongitude)}
									/>
								</div>
							</div>
						</div>
					{/if}

					<div class="mb-4 ml-1">
						<label class="inline-flex items-center">
							<input
								type="checkbox"
								bind:checked={capacityAllDay}
								class="mr-3 h-4 w-4"
								onchange={handleCapacityUpdate}
							/>
							<span class="text-sm text-gray-600">All day</span>
						</label>
					</div>

					<div class="mb-4 flex flex-col gap-6">
						<div>
							<h5 class="mb-2 text-sm text-gray-600">Start Date</h5>
							<input
								type="date"
								class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
								bind:value={capacityStartDate}
								onfocus={() => handleFocus('startDate', capacityStartDate)}
								onblur={() => handleBlurIfChanged('startDate', capacityStartDate)}
							/>
						</div>

						<div>
							<h5 class="mb-2 text-sm text-gray-600">End Date</h5>
							<input
								type="date"
								class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
								bind:value={capacityEndDate}
								onfocus={() => handleFocus('endDate', capacityEndDate)}
								onblur={() => handleBlurIfChanged('endDate', capacityEndDate)}
							/>
						</div>
					</div>

					{#if !capacityAllDay}
						<div class="mb-4 flex flex-col gap-4 md:flex-row">
							<div class="md:w-1/2">
								<input
									type="time"
									class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
									bind:value={capacityStartTime}
									onfocus={() => handleFocus('startTime', capacityStartTime)}
									onblur={() => handleBlurIfChanged('startTime', capacityStartTime)}
								/>
							</div>

							<div class="flex items-center md:w-1/2">
								<span class="mx-2 hidden text-gray-400 md:inline">to</span>
								<input
									type="time"
									class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
									bind:value={capacityEndTime}
									onfocus={() => handleFocus('endTime', capacityEndTime)}
									onblur={() => handleBlurIfChanged('endTime', capacityEndTime)}
								/>
							</div>
						</div>

						<div class="mb-4">
							<button class="text-sm text-blue-600 hover:text-blue-800 focus:outline-none">
								Time zone
							</button>
							<div class="mt-2">
								<input
									type="text"
									class="capacity-input w-full rounded-md bg-gray-100 px-3 py-2"
									bind:value={capacityTimeZone}
									onfocus={() => handleFocus('timeZone', capacityTimeZone)}
									onblur={() => handleBlurIfChanged('timeZone', capacityTimeZone)}
								/>
							</div>
						</div>
					{/if}

					<!-- Recurrence dropdown -->
					<div class="mb-6">
						<div class="relative w-full md:w-auto">
							<select
								class="capacity-select w-full appearance-none rounded-md bg-gray-100 px-3 py-2"
								bind:value={capacityRecurrence}
								onchange={handleCapacityUpdate}
							>
								{#each recurrenceOptions as option}
									<option value={option}>{option}</option>
								{/each}
							</select>
						</div>
					</div>

					<!-- Custom recurrence options -->
					{#if capacityRecurrence === 'Custom...' && capacityCustomRecurrenceRepeatEvery !== undefined}
						<div class="custom-recurrence mt-4 rounded-md bg-gray-50 p-6">
							<div class="mb-6 flex items-center">
								<span class="mr-3 text-sm font-medium text-gray-600">Repeat every</span>
								<input
									type="number"
									min="1"
									class="capacity-input qty w-16 text-right"
									bind:value={capacityCustomRecurrenceRepeatEvery}
									onfocus={() =>
										handleFocus('customRecurrenceRepeatEvery', capacityCustomRecurrenceRepeatEvery)}
									onblur={() =>
										handleBlurIfChanged(
											'customRecurrenceRepeatEvery',
											capacityCustomRecurrenceRepeatEvery
										)}
								/>
								<select
									class="capacity-select ml-3 w-28"
									bind:value={capacityCustomRecurrenceRepeatUnit}
									onchange={handleCapacityUpdate}
								>
									<option value="days">days</option>
									<option value="weeks">weeks</option>
									<option value="months">months</option>
									<option value="years">years</option>
								</select>
							</div>

							<div class="ends-section ml-2">
								<span class="mb-4 block text-sm font-medium text-gray-600">Ends</span>

								<div class="radio-options space-y-4">
									<label class="inline-flex items-center">
										<input
											type="radio"
											name="ends-{capacity.id}"
											value="never"
											checked={capacityCustomRecurrenceEndType === 'never'}
											onchange={() => {
												capacityCustomRecurrenceEndType = 'never';
												handleCapacityUpdate();
											}}
											class="mr-3"
										/>
										<span class="text-sm text-gray-600">Never</span>
									</label>

									<div class="flex items-center">
										<label class="inline-flex items-center">
											<input
												type="radio"
												name="ends-{capacity.id}"
												value="endsOn"
												checked={capacityCustomRecurrenceEndType === 'endsOn'}
												onchange={() => {
													capacityCustomRecurrenceEndType = 'endsOn';
													capacityCustomRecurrenceEndValue = formatDateForInput(new Date());
													handleCapacityUpdate();
												}}
												class="mr-3"
											/>
											<span class="text-sm text-gray-600">On</span>
										</label>
										{#if capacityCustomRecurrenceEndType === 'endsOn'}
											<div class="ml-6">
												<input
													type="date"
													class="capacity-input w-40 rounded-md bg-gray-100 px-3 py-2"
													bind:value={capacityCustomRecurrenceEndValue}
													onfocus={() =>
														handleFocus(
															'customRecurrenceEndValue',
															capacityCustomRecurrenceEndValue
														)}
													onblur={() =>
														handleBlurIfChanged(
															'customRecurrenceEndValue',
															capacityCustomRecurrenceEndValue
														)}
												/>
											</div>
										{/if}
									</div>

									<div class="flex items-center">
										<label class="inline-flex items-center">
											<input
												type="radio"
												name="ends-{capacity.id}"
												value="endsAfter"
												checked={capacityCustomRecurrenceEndType === 'endsAfter'}
												onchange={() => {
													capacityCustomRecurrenceEndType = 'endsAfter';
													capacityCustomRecurrenceEndValue = '5';
													handleCapacityUpdate();
												}}
												class="mr-3"
											/>
											<span class="text-sm text-gray-600">After</span>
										</label>
										{#if capacityCustomRecurrenceEndType === 'endsAfter'}
											<div class="ml-6 flex items-center">
												<input
													type="number"
													min="1"
													class="capacity-input qty w-16 text-right"
													bind:value={capacityCustomRecurrenceEndValue}
													onfocus={() =>
														handleFocus(
															'customRecurrenceEndValue',
															capacityCustomRecurrenceEndValue
														)}
													onblur={() =>
														handleBlurIfChanged(
															'customRecurrenceEndValue',
															capacityCustomRecurrenceEndValue
														)}
												/>
												<span class="ml-2 text-sm text-gray-600">occurrences</span>
											</div>
										{/if}
									</div>
								</div>
							</div>
						</div>
					{/if}
				</div>
			{/if}
		</div>
	{/if}

	<!-- Expanded settings (other options only) -->
	{#if expanded}
		<div class="expanded-settings mt-2 rounded-md bg-white shadow-sm">
			<div class="settings-content">
				<div class="other-options mb-4">
					<div class="max-divisibility-section mb-6">
						<h4 class="mb-4 text-sm font-medium text-gray-700">Max-divisibility</h4>
						<div class="grid grid-cols-2 gap-8">
							<div>
								<input
									type="number"
									min="1"
									step="1"
									class="capacity-input qty w-full text-right"
									bind:value={capacityMaxNaturalDiv}
									placeholder="Natural"
									onfocus={() => handleFocus('maxNaturalDiv', capacityMaxNaturalDiv)}
									onblur={() => handleBlurIfChanged('maxNaturalDiv', capacityMaxNaturalDiv)}
								/>
							</div>
							<div>
								<input
									type="number"
									min="0"
									max="1"
									step="0.01"
									class="capacity-input qty w-full text-right"
									bind:value={capacityMaxPercentageDiv}
									placeholder="Percentage (0-1)"
									onfocus={() => handleFocus('maxPercentageDiv', capacityMaxPercentageDiv)}
									onblur={() => handleBlurIfChanged('maxPercentageDiv', capacityMaxPercentageDiv)}
								/>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	{/if}
</div>

<!-- Subtree dropdown for adding filters -->
{#if showSubtreeDropdown}
	<DropDown
		position={dropdownPosition}
		show={showSubtreeDropdown}
		title="Select Subtree Category"
		searchPlaceholder="Search subtrees..."
		dataProvider={subtreesDataProvider}
		select={handleSubtreeSelect}
		close={handleDropdownClose}
	/>
{/if}

<!-- Compose from dropdown -->
{#if showComposeFromDropdown}
	<DropDown
		position={composeFromDropdownPosition}
		show={showComposeFromDropdown}
		title="Select Capacity to Compose From"
		searchPlaceholder="Search capacities you have shares in..."
		dataProvider={composeFromDataProvider}
		select={handleComposeFromSelect}
		close={handleComposeFromDropdownClose}
	/>
{/if}

<!-- Compose into dropdown -->
{#if showComposeIntoDropdown}
	<DropDown
		position={composeIntoDropdownPosition}
		show={showComposeIntoDropdown}
		title="Select Capacity to Compose Into"
		searchPlaceholder="Search all network capacities..."
		dataProvider={composeIntoDataProvider}
		select={handleComposeIntoSelect}
		close={handleComposeIntoDropdownClose}
	/>
{/if}

<!-- Capacity dropdown for adding individual filters -->
{#if showCapacityDropdown}
	<DropDown
		position={capacityDropdownPosition}
		show={showCapacityDropdown}
		title="Select Individual"
		searchPlaceholder="Search people..."
		dataProvider={contactsAndUsersDataProvider}
		select={handleCapacitySelect}
		close={handleCapacityDropdownClose}
	/>
{/if}

<style>
	/* Make sure inputs and selects match our design */
	.capacity-input {
		font-size: 1rem;
		padding: 6px 8px;
		border: none;
		border-bottom: 1.5px solid #e5e7eb;
		background: transparent;
		outline: none;
		transition: all 0.2s ease;
		min-width: 0;
		flex-shrink: 0;
	}

	.capacity-input.auto-size {
		max-width: 200px; /* Prevent inputs from becoming too wide */
	}

	.capacity-input::placeholder {
		color: #cbd5e1;
	}
	.capacity-input:focus {
		border-bottom: 1.5px solid #3b82f6;
		box-shadow: 0 1px 0 0 rgba(59, 130, 246, 0.2);
	}

	.capacity-select {
		font-size: 1rem;
		padding: 6px 8px;
		border: none;
		border-bottom: 1.5px solid #e5e7eb;
		background: transparent;
		outline: none;
		transition: all 0.2s ease;
		appearance: none;
		background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 24 24' fill='none' stroke='%23a1a1aa' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
		background-repeat: no-repeat;
		background-position: right 6px center;
		background-size: 16px;
		padding-right: 28px;
		border-radius: 0;
	}
	.capacity-select:focus {
		border-bottom: 1.5px solid #3b82f6;
		box-shadow: 0 1px 0 0 rgba(59, 130, 246, 0.2);
	}

	/* Icon buttons */
	.remove-btn,
	.settings-btn,
	.chat-btn,
	.composition-btn,
	.availability-btn {
		background: none;
		border: none;
		color: #cbd5e1;
		font-size: 1em;
		cursor: pointer;
		padding: 0 4px;
		line-height: 1;
		border-radius: 50%;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 24px;
		height: 24px;
	}

	.emoji-btn {
		background: none;
		border: 1px solid #e5e7eb;
		border-radius: 4px;
		padding: 4px 6px;
		font-size: 1.2em;
		cursor: pointer;
		transition: all 0.2s ease;
		display: flex;
		align-items: center;
		justify-content: center;
		min-width: 32px;
		height: 32px;
	}

	.emoji-btn:hover {
		background: #f9fafb;
		border-color: #3b82f6;
		transform: scale(1.05);
	}

	.emoji-picker-container {
		position: absolute;
		top: 100%;
		left: 0;
		z-index: 1000;
		margin-top: 4px;
	}

	.settings-btn:hover {
		background: #f3f4f6;
		color: #4b5563;
		transform: scale(1.05);
	}

	.chat-btn:hover {
		background: #f0f9ff;
		color: #3b82f6;
		transform: scale(1.05);
	}

	.composition-btn:hover {
		background: #f0fdf4;
		color: #059669;
		transform: scale(1.05);
	}

	.availability-btn:hover {
		background: #f0fdf4;
		color: #16a34a;
		transform: scale(1.05);
	}

	/* Unread message badge */
	.unread-badge {
		position: absolute;
		top: -2px;
		right: -2px;
		background: #ef4444;
		color: white;
		font-size: 0.65rem;
		font-weight: 600;
		line-height: 1;
		min-width: 16px;
		height: 16px;
		border-radius: 8px;
		display: flex;
		align-items: center;
		justify-content: center;
		padding: 0 4px;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2);
		z-index: 10;
		border: 1px solid white;
	}

	.remove-btn:disabled {
		color: #e5e7eb;
		cursor: not-allowed;
	}

	.remove-btn:hover:not(:disabled) {
		background: #fef2f2;
		color: #ef4444;
		transform: scale(1.05);
	}

	/* Item container */
	.capacity-item {
		display: flex;
		flex-direction: column;
	}

	.capacity-item.chat-expanded {
		margin-bottom: 2rem;
	}

	/* Filter section styling (similar to Child.svelte contributor layout) */
	.filter-section {
		background: rgba(255, 255, 255, 0.8);
		border-radius: 6px;
		padding: 8px 12px;
		border: 1px solid rgba(229, 231, 235, 0.6);
	}

	.filter-layout {
		display: flex;
		align-items: center;
		gap: 8px;
		flex-wrap: wrap;
	}

	.filter-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 8px;
		gap: 8px;
	}

	.filter-mode-toggle {
		display: flex;
		border-radius: 4px;
		overflow: hidden;
		border: 1px solid #e5e7eb;
		background: #f9fafb;
	}

	.mode-btn {
		padding: 4px 8px;
		border: none;
		background: transparent;
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.mode-btn.active {
		background: #3b82f6;
		color: white;
	}

	.mode-btn:hover:not(.active) {
		background: #e5e7eb;
		color: #374151;
	}

	.filter-button-container {
		display: flex;
		align-items: center;
		gap: 4px;
	}

	.add-filter-btn {
		display: inline-flex;
		align-items: center;
		gap: 4px;
		padding: 4px 8px;
		border: 1px dashed #d1d5db;
		border-radius: 12px;
		background: rgba(249, 250, 251, 0.8);
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		height: 22px;
		line-height: 1;
	}

	.add-filter-btn:hover {
		border-color: #3b82f6;
		color: #3b82f6;
		background: rgba(248, 250, 252, 0.9);
		transform: scale(1.02);
	}

	.add-icon {
		font-size: 0.875rem;
		font-weight: bold;
		line-height: 1;
	}

	.add-text {
		font-weight: 500;
	}

	.filter-tags-container {
		display: flex;
		flex-wrap: wrap;
		gap: 4px;
		align-items: center;
		flex: 1;
	}

	.filter-tag-wrapper {
		display: flex;
		align-items: center;
	}

	.filter-description {
		margin-top: 6px;
		padding-top: 6px;
		border-top: 1px solid rgba(229, 231, 235, 0.4);
	}

	.filter-desc-text {
		font-size: 0.75rem;
		color: #6b7280;
		font-style: italic;
	}

	/* Composition section styling */
	.composition-section {
		animation: slideDown 0.2s ease-out;
	}

	.composition-header h4 {
		margin: 0;
	}

	.composition-header p {
		margin: 0;
	}

	/* Availability section styling */
	.availability-section {
		animation: slideDown 0.2s ease-out;
	}

	.availability-header h4 {
		margin: 0;
	}

	.availability-header p {
		margin: 0;
	}

	.composition-columns {
		gap: 1rem;
	}

	.compose-from-column,
	.compose-into-column {
		background: rgba(255, 255, 255, 0.5);
		border-radius: 8px;
		padding: 16px;
		border: 1px solid rgba(229, 231, 235, 0.6);
	}

	.column-header h5 {
		margin: 0;
	}

	.column-header p {
		margin: 0;
	}

	.add-capacity-btn {
		display: inline-flex;
		align-items: center;
		gap: 6px;
		padding: 8px 12px;
		border: 1px dashed #d1d5db;
		border-radius: 6px;
		background: rgba(249, 250, 251, 0.8);
		color: #6b7280;
		font-size: 0.75rem;
		cursor: pointer;
		transition: all 0.2s ease;
		width: 100%;
		justify-content: center;
	}

	.add-capacity-btn:hover {
		border-color: #3b82f6;
		color: #3b82f6;
		background: rgba(248, 250, 252, 0.9);
		transform: scale(1.02);
	}

	.add-capacity-btn .add-icon {
		font-size: 0.875rem;
		font-weight: bold;
	}

	.remove-composition-btn {
		background: rgba(239, 68, 68, 0.1);
		color: #dc2626;
		border: 1px solid rgba(239, 68, 68, 0.2);
		border-radius: 4px;
		padding: 4px 8px;
		cursor: pointer;
		transition: all 0.2s ease;
		margin-top: 4px;
	}

	.remove-composition-btn:hover {
		background: rgba(239, 68, 68, 0.2);
		border-color: #dc2626;
	}

	.composition-items {
		max-height: 400px;
		overflow-y: auto;
	}

	.empty-state {
		background: rgba(243, 244, 246, 0.5);
		border-radius: 6px;
		border: 1px dashed #d1d5db;
	}

	/* Expanded settings styling */
	.expanded-settings {
		font-size: 0.875rem;
		animation: slideDown 0.25s ease-out;
		border-top: 1px solid rgba(229, 231, 235, 0.5);
		box-shadow:
			0 4px 6px -1px rgba(0, 0, 0, 0.05),
			0 2px 4px -1px rgba(0, 0, 0, 0.03);
	}

	.settings-content {
		padding: 1.5rem;
	}

	.custom-recurrence {
		border: 1px solid rgba(229, 231, 235, 0.7);
	}

	/* Animation for expanding the settings */
	@keyframes slideDown {
		from {
			opacity: 0;
			transform: translateY(-10px);
		}
		to {
			opacity: 1;
			transform: translateY(0);
		}
	}

	/* Radio buttons */
	input[type='radio'] {
		appearance: auto;
		margin-right: 0.5rem;
		color: #3b82f6;
	}

	/* Chat container styling */
	.chat-container {
		animation: slideDown 0.2s ease-out;
	}

	.chat-header h4 {
		margin: 0;
	}

	.chat-header p {
		margin: 0;
	}

	/* Capacity row responsive behavior */
	.capacity-row {
		gap: 8px; /* Slightly smaller gap for better wrapping */
	}

	/* Ensure buttons don't wrap unnecessarily */
	.capacity-row button {
		flex-shrink: 0;
	}

	/* Description field with integrated toggle */
	.description-field-container {
		position: relative;
		display: flex;
		align-items: flex-start;
	}

	.description-expand-btn {
		position: absolute;
		right: 4px;
		top: 50%;
		transform: translateY(-50%);
		background: none;
		border: none;
		color: #9ca3af;
		font-size: 0.75rem;
		cursor: pointer;
		padding: 2px 4px;
		border-radius: 2px;
		transition: all 0.2s ease;
		z-index: 1;
		line-height: 1;
	}

	.description-expand-btn:hover {
		background: #f3f4f6;
		color: #6b7280;
	}

	.description-textarea {
		resize: vertical;
		min-height: 60px;
		padding-right: 24px; /* Make room for the button */
		font-family: inherit;
		font-size: 1rem;
		line-height: 1.4;
	}

	/* Adjust input padding when there's a toggle button */
	.description-field-container input {
		padding-right: 24px;
	}
</style>
