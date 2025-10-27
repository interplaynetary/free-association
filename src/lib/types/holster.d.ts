declare module '@mblaney/holster/src/holster.js' {
	// Holster initialization options
	export interface HolsterOptions {
		// Core Options
		secure?: boolean;
		peers?: string | string[];
		
		// Storage Options
		file?: string;
		indexedDB?: boolean;
		store?: any;
		
		// Radisk Options
		batch?: number;
		write?: number;
		size?: number;
		cache?: boolean;
		memoryLimit?: number;
		log?: (message: string) => void;
		
		// Network Options
		port?: number;
		server?: any;
		wss?: any;
		maxConnections?: number;
		maxMessageSize?: number;
		maxQueueLength?: number;
	}

	// Lex query for filtering properties
	export interface LexQuery {
		'.': string | { '*'?: string; '<'?: string; '>'?: string };
	}

	// Options for get/next/on operations
	export interface QueryOptions {
		wait?: number; // milliseconds to wait for response, default 100
	}

	// User credentials object
	export interface UserCredentials {
		username: string;
		pub: string;
		priv: string;
		epub: string;
		epriv: string;
	}

	// SEA pair object (subset of UserCredentials)
	export interface SEAPair {
		pub?: string;
		priv?: string;
		epub?: string;
		epriv?: string;
	}

	// Encrypted data format
	export interface EncryptedData {
		ct: string;
		iv: string;
		s: string;
	}

	// Signed data format
	export interface SignedData {
		m: any; // message
		s: string; // signature
	}

	// Callback types
	// Get callbacks receive data or null (errors are logged, not returned)
	export type GetCallback = (data: any) => void;
	// Put callbacks receive error or null on success
	export type PutCallback = (err: any) => void;

	// Core Holster API chain
	export interface HolsterChain {
		get(key: string): HolsterChain;
		get(key: string, callback: GetCallback): HolsterChain;
		get(key: string, lex: LexQuery, callback: GetCallback, options?: QueryOptions): HolsterChain;
		
		next(key: string): HolsterChain;
		next(key: string, callback: GetCallback): HolsterChain;
		next(key: string, lex: LexQuery, callback: GetCallback, options?: QueryOptions): HolsterChain;
		
		put(value: any): HolsterChain;
		put(value: any, callback: PutCallback): HolsterChain;
		put(value: any, set: boolean, callback?: PutCallback): HolsterChain;
		
		on(callback: GetCallback): HolsterChain;
		on(callback: GetCallback, get: boolean): HolsterChain;
		on(lex: LexQuery, callback: GetCallback, get?: boolean, options?: QueryOptions): HolsterChain;
		
		off(): void;
		off(callback: GetCallback): void;
	}

	// User API extends Holster API with authentication
	export interface HolsterUser extends Omit<HolsterChain, 'get'> {
		is: UserCredentials | null;
		
		// User management
		create(username: string, password: string, callback?: PutCallback): void;
		auth(username: string, password: string, callback?: PutCallback): void;
		change(username: string, currentPassword: string, newPassword: string, callback?: PutCallback): void;
		delete(username: string, password: string, callback?: PutCallback): void;
		
		// Session management
		recall(): void;
		leave(): void;
		store(useLocalStorage?: boolean): void;
		
		// User-specific get (can query own data or other users' public data)
		get(key: string): HolsterChain;
		get(key: string, callback: GetCallback): HolsterChain;
		get(key: string, lex: LexQuery, callback: GetCallback, options?: QueryOptions): HolsterChain;
		get(key: [string, string]): HolsterChain; // [pub, key] format
		get(key: [string, string], callback: GetCallback): HolsterChain;
		get(key: [string, string], lex: LexQuery, callback: GetCallback, options?: QueryOptions): HolsterChain;
		
		// Access to SEA and Wire APIs
		SEA: SEAAPI;
		wire: WireAPI;
	}

	// SEA (Security, Encryption, Authorization) API
	export interface SEAAPI {
		pair(): Promise<SEAPair>;
		pair(callback: (pair: SEAPair) => void): void;
		
		encrypt(data: any, pair: SEAPair): Promise<EncryptedData>;
		encrypt(data: any, pair: SEAPair, callback: (encrypted: EncryptedData) => void): void;
		
		decrypt(encrypted: EncryptedData, pair: SEAPair): Promise<any>;
		decrypt(encrypted: EncryptedData, pair: SEAPair, callback: (data: any) => void): void;
		
		sign(data: any, pair: SEAPair): Promise<SignedData>;
		sign(data: any, pair: SEAPair, callback: (signed: SignedData) => void): void;
		
		verify(signed: SignedData, pair: SEAPair): Promise<any>;
		verify(signed: SignedData, pair: SEAPair, callback: (data: any) => void): void;
		
		work(data: any, salt?: string): Promise<SEAPair>;
		work(data: any, callback: (pair: SEAPair) => void): void;
		work(data: any, salt: string, callback: (pair: SEAPair) => void): void;
		
		secret(theirPair: SEAPair, myPair: SEAPair): Promise<SEAPair>;
		secret(theirPair: SEAPair, myPair: SEAPair, callback: (secret: SEAPair) => void): void;
	}

	// Wire API (lower-level graph operations)
	export interface WireLexQuery {
		'#': string; // node id
		'.'?: string; // optional property
	}

	// Rel (relationship) - references another node by id
	export interface RelValue {
		'#': string;
	}

	export interface GraphNode {
		_: {
			'#': string; // node id
			'>': { [key: string]: number }; // state vector
		};
		// Properties can be: string, number, boolean, null, or RelValue
		[key: string]: any;
	}

	export interface GraphData {
		[nodeId: string]: GraphNode;
	}

	export interface WireAPI {
		get(lex: WireLexQuery, callback: (data: any) => void, options?: any): void;
		put(graph: GraphData, callback: PutCallback): void;
		on(lex: WireLexQuery, callback: (data: any) => void): void;
		off(lex: WireLexQuery): void;
		off(lex: WireLexQuery, callback: (data: any) => void): void;
	}

	// Main Holster instance
	export interface HolsterInstance extends HolsterChain {
		user(): HolsterUser;
		SEA: SEAAPI;
		wire: WireAPI;
	}

	function Holster(options?: HolsterOptions | string | string[]): HolsterInstance;
	export default Holster;
}

