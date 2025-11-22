// PWA type declarations - may not be available in all environments
declare module 'virtual:pwa-info' {
	export interface PwaInfo {
		webManifest: {
			linkTag: string;
			href: string;
		};
	}
	export const pwaInfo: PwaInfo | undefined;
}

declare module 'virtual:pwa-register/svelte' {
	import type { Writable } from 'svelte/store';
	
	export interface RegisterSWOptions {
		immediate?: boolean;
		onNeedRefresh?: () => void;
		onOfflineReady?: () => void;
		onRegistered?: (registration: ServiceWorkerRegistration | undefined) => void;
		onRegisterError?: (error: any) => void;
	}
	
	export interface RegisterSWReturn {
		needRefresh: Writable<boolean>;
		offlineReady: Writable<boolean>;
		updateServiceWorker: (reloadPage?: boolean) => Promise<void>;
	}
	
	export function useRegisterSW(options?: RegisterSWOptions): RegisterSWReturn;
}

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
}

export {};
