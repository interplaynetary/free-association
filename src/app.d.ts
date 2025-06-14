// See https://kit.svelte.dev/docs/types#app
// for information about these interfaces

declare global {
	namespace App {
		// interface Error {}
		// interface Locals {}
		// interface PageData {}
		// interface PageState {}
		// interface Platform {}
	}

	// PWA and Service Worker types
	interface ServiceWorkerGlobalScope extends EventTarget {
		__WB_MANIFEST: any;
		registration: ServiceWorkerRegistration;
		clients: Clients;
		addEventListener: typeof addEventListener;
		skipWaiting(): Promise<void>;
	}

	interface Window {
		workbox?: any;
	}

	interface ExtendableEvent extends Event {
		waitUntil(f: Promise<any>): void;
	}

	interface PushEvent extends ExtendableEvent {
		readonly data: PushMessageData | null;
	}

	interface NotificationEvent extends ExtendableEvent {
		readonly action: string;
		readonly notification: Notification;
	}

	interface SyncEvent extends ExtendableEvent {
		readonly lastChance: boolean;
		readonly tag: string;
	}

	interface PushMessageData {
		arrayBuffer(): ArrayBuffer;
		blob(): Blob;
		json(): any;
		text(): string;
	}
}

// Virtual module declarations for PWA
declare module 'virtual:pwa-info' {
	interface PWAInfo {
		webManifest: {
			linkTag: string;
		};
	}
	export const pwaInfo: PWAInfo | undefined;
}

declare module 'virtual:pwa-register' {
	export interface RegisterSWOptions {
		immediate?: boolean;
		onRegistered?: (registration: ServiceWorkerRegistration) => void;
		onRegisterError?: (error: Error) => void;
	}
	export function registerSW(options?: RegisterSWOptions): void;
}

export {};
