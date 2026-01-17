/**
 * Lightweight IndexedDB Key-Value Store
 * 
 * A minimal wrapper around IndexedDB for simple key-value storage.
 * Used as a replacement for localStorage to avoid quota limits.
 * 
 * Database: free-association-db
 * Store: keyval
 */

const DB_NAME = 'free-association-db';
const STORE_NAME = 'keyval';
const DB_VERSION = 1;

/**
 * Open the database connection
 */
function openDB(): Promise<IDBDatabase> {
	return new Promise((resolve, reject) => {
		const request = indexedDB.open(DB_NAME, DB_VERSION);

		request.onupgradeneeded = (event) => {
			const db = (event.target as IDBOpenDBRequest).result;
			if (!db.objectStoreNames.contains(STORE_NAME)) {
				db.createObjectStore(STORE_NAME);
			}
		};

		request.onsuccess = (event) => {
			resolve((event.target as IDBOpenDBRequest).result);
		};

		request.onerror = (event) => {
			reject((event.target as IDBOpenDBRequest).error);
		};
	});
}

/**
 * Get a value by key
 */
export async function get<T>(key: string): Promise<T | undefined> {
	if (typeof window === 'undefined') return undefined;
	
	try {
		const db = await openDB();
		return new Promise((resolve, reject) => {
			const transaction = db.transaction(STORE_NAME, 'readonly');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.get(key);

			request.onsuccess = () => {
				resolve(request.result as T);
			};

			request.onerror = () => {
				reject(request.error);
			};
		});
	} catch (error) {
		console.error('[IndexedDB] Error getting key:', key, error);
		return undefined;
	}
}

/**
 * Set a value by key
 */
export async function set(key: string, value: any): Promise<void> {
	if (typeof window === 'undefined') return;

	try {
		const db = await openDB();
		return new Promise((resolve, reject) => {
			const transaction = db.transaction(STORE_NAME, 'readwrite');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.put(value, key);

			request.onsuccess = () => {
				resolve();
			};

			request.onerror = () => {
				reject(request.error);
			};
		});
	} catch (error) {
		console.error('[IndexedDB] Error setting key:', key, error);
		throw error;
	}
}

/**
 * Delete a value by key
 */
export async function del(key: string): Promise<void> {
	if (typeof window === 'undefined') return;

	try {
		const db = await openDB();
		return new Promise((resolve, reject) => {
			const transaction = db.transaction(STORE_NAME, 'readwrite');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.delete(key);

			request.onsuccess = () => {
				resolve();
			};

			request.onerror = () => {
				reject(request.error);
			};
		});
	} catch (error) {
		console.error('[IndexedDB] Error deleting key:', key, error);
		throw error;
	}
}

/**
 * Clear all keys
 */
export async function clear(): Promise<void> {
	if (typeof window === 'undefined') return;

	try {
		const db = await openDB();
		return new Promise((resolve, reject) => {
			const transaction = db.transaction(STORE_NAME, 'readwrite');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.clear();

			request.onsuccess = () => {
				resolve();
			};

			request.onerror = () => {
				reject(request.error);
			};
		});
	} catch (error) {
		console.error('[IndexedDB] Error clearing store', error);
		throw error;
	}
}
