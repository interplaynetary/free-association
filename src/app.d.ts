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

// PWA Virtual Module Declarations
declare module 'virtual:pwa-register' {
	export interface RegisterSWOptions {
		immediate?: boolean;
		onNeedRefresh?: () => void;
		onOfflineReady?: () => void;
		onRegistered?: (registration: ServiceWorkerRegistration | undefined) => void;
		onRegisterError?: (error: any) => void;
	}
	
	export function registerSW(options?: RegisterSWOptions): (reloadPage?: boolean) => Promise<void>;
}

declare module 'virtual:pwa-info' {
	export interface PwaInfo {
		webManifest: {
			linkTag: string;
		};
	}
	
	export const pwaInfo: PwaInfo | undefined;
}

export {};
