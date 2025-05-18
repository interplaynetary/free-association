// See https://kit.svelte.dev/docs/types#app
// for information about these interfaces
import type { User } from '$lib/server/auth';
import type { Database } from 'better-sqlite3';
import type { DefaultSession } from '@auth/core/types';

declare global {
	namespace App {
		// interface Error {}
		interface Locals {
			user: import('@auth/core/types').User | null;
			db: any;
			auth: import('@auth/sveltekit').SvelteKitAuth;
		}
		interface PageData {
			session: import('@auth/sveltekit').Session | null;
		}
		// interface PageState {}
		// interface Platform {}
	}
}

// Auth.js adds this
declare module '@auth/core/types' {
	interface User {
		role?: string;
	}
	interface Session {
		user: {
			role?: string;
		} & DefaultSession['user'];
	}
}

// Auth.js extends the standard Session type with a user id
declare module '@auth/sveltekit' {
	interface Session {
		user?: {
			id?: string;
			name?: string | null;
			email?: string | null;
			image?: string | null;
			role?: string | null;
		};
	}
}

export {};
