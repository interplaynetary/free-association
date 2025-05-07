// Core Data Types for free association system
// Translated from Haskell to TypeScript

// ==== Cache Types ====
export type CacheValue =
	| { type: 'float'; value: number }
	| { type: 'int'; value: number }
	| { type: 'stringList'; value: string[] }
	| { type: 'shareMap'; value: ShareMap };

export interface Cache<K> {
	cacheMap: Map<K, CacheValue>;
	cacheMisses: number;
	cacheHits: number;
}

// ==== Core Types ====
export interface Points {
	value: number;
}

export interface Ctx {
	ctxParent: Node;
	ctxSiblings: Map<string, Node>;
	ctxAncestors: Ctx[];
}

export interface TreeZipper {
	zipperCurrent: Node;
	zipperContext: Ctx | null;
}

export type Forest = Map<string, TreeZipper>;

// ==== Share Types ====
export type ShareMap = Map<string, number>;
export type VisitedSet = Set<string>;

// ==== Capacity Types ====
export enum RecurrenceUnit {
	Days = 'days',
	Weeks = 'weeks',
	Months = 'months',
	Years = 'years'
}

export type RecurrenceEnd =
	| { type: 'never' }
	| { type: 'endsOn'; date: Date }
	| { type: 'endsAfter'; count: number };

export interface CustomRecurrence {
	repeatEvery: number;
	repeatUnit: RecurrenceUnit;
	recurrenceEnd: RecurrenceEnd;
}

export enum LocationType {
	Undefined = 'undefined',
	LiveLocation = 'liveLocation',
	Specific = 'specific'
}

export interface SpaceTimeCoordinates {
	locationType: LocationType;
	allDay: boolean;
	recurrence: string | null;
	customRecurrence: CustomRecurrence | null;
	startDate: Date;
	startTime: Date;
	endDate: Date;
	endTime: Date;
	timeZone: string;
}

export interface MaxDivisibility {
	naturalDiv: number;
	percentageDiv: number;
}

export interface Capacity {
	capacityId: string;
	capacityName: string;
	quantity: number;
	unit: string;
	shareDepth: number;
	expanded: boolean;
	coordinates: SpaceTimeCoordinates;
	maxDivisibility: MaxDivisibility;
	hiddenUntilRequestAccepted: boolean;
}

export interface CapacityShare {
	targetCapacity: Capacity;
	sharePercentage: number;
	computedQuantity: number; // Derived from percentage * capacity quantity, respecting maxDivisibility
}

export type CapacityInventory = Map<string, Capacity>;
export type CapacityShares = Map<string, CapacityShare>;

// ==== Node Type ====
export interface Node {
	nodeId: string;
	nodeName: string;
	nodePoints: Points;
	nodeChildren: Map<string, Node>;
	nodeContributors: Set<string>;
	nodeManualFulfillment: number | null;
	nodeCapacities: CapacityInventory;
	nodeCapacityShares: CapacityShares;
	nodeCache: Cache<string>;
}

// ==== Provider Shares Cache ====
export interface ProviderSharesCache {
	sharesCache: Cache<string>;
}

// ==== Navigation ====
export type NavigationPath = string[];
