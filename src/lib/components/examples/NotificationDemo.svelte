<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { notificationService } from '../../services/notifications.js';

	interface ScheduledNotification {
		id: string;
		title: string;
		body: string;
		scheduledAt: Date;
		delivered: number; // 0 = false, 1 = true
		recurring?: {
			interval: 'daily' | 'weekly' | 'monthly';
			days?: number[];
		};
		tags?: string[];
		createdAt: Date;
	}

	let isInitialized = false;
	let permissionGranted = false;
	let notifications: ScheduledNotification[] = [];
	let loading = false;

	// Form state
	let title = '';
	let body = '';
	let scheduledDate = '';
	let scheduledTime = '';
	let isRecurring = false;
	let recurringInterval: 'daily' | 'weekly' | 'monthly' = 'daily';

	onMount(async () => {
		try {
			loading = true;
			await notificationService.init();
			isInitialized = true;
			permissionGranted = Notification.permission === 'granted';
			await refreshNotifications();
		} catch (error) {
			console.error('Failed to initialize notification service:', error);
		} finally {
			loading = false;
		}
	});

	onDestroy(() => {
		notificationService.destroy();
	});

	async function refreshNotifications() {
		if (!isInitialized) return;
		notifications = await notificationService.getScheduledNotifications();
	}

	async function requestPermission() {
		const granted = await notificationService.requestPermission();
		permissionGranted = granted;
		if (!granted) {
			alert('Notification permission is required for this demo to work.');
		}
	}

	async function scheduleNotification() {
		if (!permissionGranted) {
			await requestPermission();
			return;
		}

		if (!title || !body) {
			alert('Please enter both title and body');
			return;
		}

		try {
			let scheduledAt: Date;

			if (scheduledDate && scheduledTime) {
				scheduledAt = new Date(`${scheduledDate}T${scheduledTime}`);
			} else {
				// Default to 1 minute from now
				scheduledAt = new Date();
				scheduledAt.setMinutes(scheduledAt.getMinutes() + 1);
			}

			const options = isRecurring
				? {
						recurring: { interval: recurringInterval }
					}
				: undefined;

			await notificationService.scheduleNotification(title, body, scheduledAt, options);

			// Reset form
			title = '';
			body = '';
			scheduledDate = '';
			scheduledTime = '';
			isRecurring = false;

			await refreshNotifications();
			alert('Notification scheduled successfully!');
		} catch (error) {
			console.error('Failed to schedule notification:', error);
			alert('Failed to schedule notification');
		}
	}

	async function deleteNotification(id: string) {
		await notificationService.deleteNotification(id);
		await refreshNotifications();
	}

	async function scheduleQuickTest() {
		if (!permissionGranted) {
			await requestPermission();
			return;
		}

		await notificationService.scheduleIn(
			'Test Notification',
			'This is a test notification scheduled 10 seconds ago!',
			0 // 0 minutes = immediate
		);

		setTimeout(async () => {
			await refreshNotifications();
		}, 1000);
	}

	function formatDate(date: Date): string {
		return new Intl.DateTimeFormat('en-US', {
			dateStyle: 'short',
			timeStyle: 'short'
		}).format(date);
	}

	// Set default date/time to 1 minute from now
	function setDefaultDateTime() {
		const now = new Date();
		now.setMinutes(now.getMinutes() + 1);

		scheduledDate = now.toISOString().split('T')[0];
		scheduledTime = now.toTimeString().split(' ')[0].slice(0, 5);
	}

	onMount(() => {
		setDefaultDateTime();
	});
</script>

<div class="mx-auto max-w-4xl space-y-6 p-6">
	<div class="rounded-lg bg-white p-6 shadow-lg">
		<h1 class="mb-4 text-2xl font-bold text-gray-900">Browser-Only Self-Notifications Demo</h1>
		<p class="mb-4 text-gray-600">
			This demo showcases browser-only notifications using @vite-pwa/sveltekit + push.js + idb. All
			notifications are stored locally in IndexedDB and delivered entirely within the browser.
		</p>

		{#if loading}
			<div class="flex items-center justify-center py-8">
				<div class="h-8 w-8 animate-spin rounded-full border-b-2 border-blue-600"></div>
				<span class="ml-2">Initializing notification service...</span>
			</div>
		{:else if !isInitialized}
			<div class="rounded-md border border-red-200 bg-red-50 p-4">
				<p class="text-red-800">Failed to initialize notification service</p>
			</div>
		{:else}
			<!-- Permission Status -->
			<div class="mb-6">
				<div class="flex items-center space-x-2">
					<div
						class={`h-3 w-3 rounded-full ${permissionGranted ? 'bg-green-500' : 'bg-red-500'}`}
					></div>
					<span class="text-sm">
						Notification Permission: {permissionGranted ? 'Granted' : 'Not Granted'}
					</span>
					{#if !permissionGranted}
						<button
							on:click={requestPermission}
							class="ml-2 rounded bg-blue-600 px-3 py-1 text-xs text-white hover:bg-blue-700"
						>
							Request Permission
						</button>
					{/if}
				</div>
			</div>

			<!-- Quick Test -->
			<div class="mb-6">
				<button
					on:click={scheduleQuickTest}
					class="rounded bg-green-600 px-4 py-2 text-white hover:bg-green-700 focus:ring-2 focus:ring-green-500 focus:outline-none"
				>
					Test Instant Notification
				</button>
				<p class="mt-1 text-sm text-gray-500">Triggers a notification immediately</p>
			</div>

			<!-- Schedule New Notification -->
			<div class="mb-6 rounded-lg bg-gray-50 p-4">
				<h2 class="mb-4 text-lg font-semibold text-gray-900">Schedule New Notification</h2>

				<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
					<div>
						<label for="title" class="mb-1 block text-sm font-medium text-gray-700"> Title </label>
						<input
							id="title"
							bind:value={title}
							type="text"
							placeholder="Notification title"
							class="w-full rounded-md border border-gray-300 px-3 py-2 shadow-sm focus:border-blue-500 focus:ring-blue-500 focus:outline-none"
						/>
					</div>

					<div>
						<label for="body" class="mb-1 block text-sm font-medium text-gray-700"> Message </label>
						<input
							id="body"
							bind:value={body}
							type="text"
							placeholder="Notification message"
							class="w-full rounded-md border border-gray-300 px-3 py-2 shadow-sm focus:border-blue-500 focus:ring-blue-500 focus:outline-none"
						/>
					</div>
				</div>

				<div class="mb-4 grid grid-cols-1 gap-4 md:grid-cols-2">
					<div>
						<label for="date" class="mb-1 block text-sm font-medium text-gray-700">
							Scheduled Date
						</label>
						<input
							id="date"
							bind:value={scheduledDate}
							type="date"
							class="w-full rounded-md border border-gray-300 px-3 py-2 shadow-sm focus:border-blue-500 focus:ring-blue-500 focus:outline-none"
						/>
					</div>

					<div>
						<label for="time" class="mb-1 block text-sm font-medium text-gray-700">
							Scheduled Time
						</label>
						<input
							id="time"
							bind:value={scheduledTime}
							type="time"
							class="w-full rounded-md border border-gray-300 px-3 py-2 shadow-sm focus:border-blue-500 focus:ring-blue-500 focus:outline-none"
						/>
					</div>
				</div>

				<div class="mb-4">
					<label class="flex items-center">
						<input
							bind:checked={isRecurring}
							type="checkbox"
							class="focus:ring-opacity-50 rounded border-gray-300 text-blue-600 shadow-sm focus:border-blue-300 focus:ring focus:ring-blue-200"
						/>
						<span class="ml-2 text-sm font-medium text-gray-700">Recurring notification</span>
					</label>

					{#if isRecurring}
						<div class="mt-2">
							<select
								bind:value={recurringInterval}
								class="rounded-md border border-gray-300 px-3 py-2 shadow-sm focus:border-blue-500 focus:ring-blue-500 focus:outline-none"
							>
								<option value="daily">Daily</option>
								<option value="weekly">Weekly</option>
								<option value="monthly">Monthly</option>
							</select>
						</div>
					{/if}
				</div>

				<button
					on:click={scheduleNotification}
					disabled={!permissionGranted}
					class="rounded bg-blue-600 px-4 py-2 text-white hover:bg-blue-700 focus:ring-2 focus:ring-blue-500 focus:outline-none disabled:cursor-not-allowed disabled:opacity-50"
				>
					Schedule Notification
				</button>
			</div>

			<!-- Scheduled Notifications List -->
			<div>
				<div class="mb-4 flex items-center justify-between">
					<h2 class="text-lg font-semibold text-gray-900">Scheduled Notifications</h2>
					<button
						on:click={refreshNotifications}
						class="rounded bg-gray-600 px-3 py-1 text-sm text-white hover:bg-gray-700"
					>
						Refresh
					</button>
				</div>

				{#if notifications.length === 0}
					<p class="py-8 text-center text-gray-500">No notifications scheduled</p>
				{:else}
					<div class="space-y-3">
						{#each notifications as notification (notification.id)}
							<div
								class="flex items-center justify-between rounded-lg border border-gray-200 bg-white p-4"
							>
								<div class="flex-1">
									<h3 class="font-medium text-gray-900">{notification.title}</h3>
									<p class="mt-1 text-sm text-gray-600">{notification.body}</p>
									<div class="mt-2 flex items-center space-x-4 text-xs text-gray-500">
										<span>Scheduled: {formatDate(new Date(notification.scheduledAt))}</span>
										<span
											class={`rounded-full px-2 py-1 ${notification.delivered ? 'bg-green-100 text-green-800' : 'bg-yellow-100 text-yellow-800'}`}
										>
											{notification.delivered ? 'Delivered' : 'Pending'}
										</span>
										{#if notification.recurring}
											<span class="rounded-full bg-blue-100 px-2 py-1 text-blue-800">
												{notification.recurring.interval}
											</span>
										{/if}
									</div>
								</div>
								<button
									on:click={() => deleteNotification(notification.id)}
									class="ml-4 rounded bg-red-600 px-3 py-1 text-sm text-white hover:bg-red-700"
								>
									Delete
								</button>
							</div>
						{/each}
					</div>
				{/if}
			</div>
		{/if}
	</div>
</div>

<style>
	/* Add any additional styles here */
</style>
