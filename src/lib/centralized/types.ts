// ==== Core Data Types ====
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

// ==== Node Type (Union Type) ====
export type Node = RootNode | NonRootNode;

export interface RootNode {
	type: 'root';
	nodeId: string;
	nodeName: string;
	nodeChildren: Map<string, Node>;
	nodeManualFulfillment: number | null;
	// Fields that should only exist at root level
	nodeCapacities: CapacityInventory;
	nodeCapacityShares: CapacityShares;
	nodeSOGFMap: ShareMap | null;
	nodeProviderSharesMap: Map<number, ShareMap>;
}

export interface NonRootNode {
	type: 'non-root';
	nodeId: string;
	nodeName: string;
	nodePoints: number;
	nodeChildren: Map<string, Node>;
	nodeContributors: Set<string>;
	nodeManualFulfillment: number | null;
}

// ==== Navigation ====
export type NavigationPath = string[];
