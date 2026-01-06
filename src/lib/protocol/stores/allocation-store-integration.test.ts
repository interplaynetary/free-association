import { describe, it, expect, beforeEach, mock } from "bun:test";
import { get, writable, derived } from "svelte/store";
import { resolve } from "path";

// Logic to resolve absolute paths
const storesPath = resolve(import.meta.dir, "stores.svelte");
const ipfPath = resolve(import.meta.dir, "../allocation-ipf-distributed.ts"); // .ts extension might be needed
const schemasPath = resolve(import.meta.dir, "../schemas.ts");

console.log("Mocking stores at:", storesPath);

// Mock schemas
mock.module(schemasPath, () => ({
    CommitmentSchema: {},
    normalizeGlobalRecognitionWeights: (w: any) => w,
}));

// Mock distributed logic
const mockUpdateProvider = mock(() => ({ rowScalings: { "p1": 0.5 }, colScalings: {}, cachedRemoteScalings: {} }));
const mockUpdateRecipient = mock(() => ({ rowScalings: {}, colScalings: { "n1": 0.8 }, cachedRemoteScalings: {} }));
const mockGenerateProposals = mock(() => ([{
    capacity_slot_id: "c1",
    need_slot_id: "n1",
    proposed_quantity: 10,
    provider_id: "me",
    recipient_id: "them",
    recipient_pubkey: "them", // Added to match code expectations
    priority: 1.0
}]));

mock.module(ipfPath, () => ({
    updateProviderState: mockUpdateProvider,
    updateRecipientState: mockUpdateRecipient,
    generateFlowProposals: mockGenerateProposals,
}));

// Mock stores.svelte
const mockMyCommitmentStore = writable<any>(null);
const mockNetworkCommitments = writable<any>({});
const mockNetworkAllocations = writable<any>(new Map());
const mockNetworkNeedSlots = writable<any>([]); // List of needs
const mockMyDistributedIPFState = writable<any>({ rowScalings: {}, colScalings: {}, cachedRemoteScalings: {} });

mock.module(storesPath, () => {
    console.log("stores.svelte mock factory called!");
    return {
        myCommitmentStore: mockMyCommitmentStore,
        networkCommitments: mockNetworkCommitments,
        networkAllocations: mockNetworkAllocations,
        networkNeedSlots: mockNetworkNeedSlots,
        myDistributedIPFState: mockMyDistributedIPFState,
        myCurrentNeeds: derived(mockMyCommitmentStore, ($c: any) => {
            const map: any = {};
            if ($c?.need_slots) {
                $c.need_slots.forEach((s: any) => map[s.id] = s);
            }
            return map;
        }),

        getAllCommitmentsRecord: () => ({}),
        networkRecognitionWeights: writable(new Map()),
        networkNeedsIndex: writable({ byAll: new Map(), byType: new Map(), byTypeAndLocation: new Map(), byTypeAndTime: new Map() }),
        myRecognitionWeights: writable({}),
        myMutualRecognition: writable({}),
        totalReceivedBySlot: writable({}),
        holsterUserPub: writable("my-pub-key")
    };
});

// Mock util dependencies
mock.module("@playnet/free-association/utils/match", () => ({
    slotsCompatible: () => true,
    passesSlotFilters: () => true,
    getTimeBucketKey: () => "time",
    getLocationBucketKey: () => "loc"
}));

mock.module("$lib/network/holster.svelte", () => ({
    holsterUserPub: writable("my-pub-key")
}));


describe("Allocation Store Integration", () => {
    let myAllocationsAsProvider: any;

    // Load module dynamically so mocks apply!
    beforeEach(async () => {
        const module = await import("./allocation.svelte");
        myAllocationsAsProvider = module.myAllocationsAsProvider;
        module.enableDistributedAllocation(); // Start the loops!

        // Reset stores
        mockMyCommitmentStore.set(null);
        mockNetworkNeedSlots.set([]);
        mockMyDistributedIPFState.set({ rowScalings: {}, colScalings: {}, cachedRemoteScalings: {} });
        mockUpdateProvider.mockClear();
        mockGenerateProposals.mockClear();
        mockUpdateRecipient.mockClear();
    });

    it("should trigger provider update when capacity and needs exist", async () => {
        // Setup Inputs
        const myCapacity = [{ id: "c1", quantity: 100, type_id: "food" }];
        mockMyCommitmentStore.set({ capacity_slots: myCapacity, need_slots: [] });

        const networkNeeds = [{ id: "n1", quantity: 50, type_id: "food" }];
        mockNetworkNeedSlots.set(networkNeeds);

        // Wait for reactivity (Svelte stores are sync but derived might take a tick?)
        // In simple svelte/store, derived updates immediately on subscription.
        // We need to subscribe to myAllocationsAsProvider to drive the derived chain.
        const unsubscribe = myAllocationsAsProvider.subscribe(() => { });

        // Check if provider update was called
        // The side-effect is in a separate subscribe in allocation.svelte.ts
        // But that subscribe is top-level.

        // Wait a tiny bit for the top-level subscribes to fire? 
        // Typically they fire synchronously upon store update.

        expect(mockUpdateProvider).toHaveBeenCalled();
        expect(mockGenerateProposals).toHaveBeenCalled();

        unsubscribe();
    });

    it("should update myAllocationsAsProvider based on proposals", () => {
        const myCapacity = [{ id: "c1", quantity: 100, type_id: "food" }];
        mockMyCommitmentStore.set({ capacity_slots: myCapacity });
        mockNetworkNeedSlots.set([{ id: "n1" }]);

        const result = get(myAllocationsAsProvider) as any;

        expect(result.allocations).toHaveLength(1);
        expect(result.allocations[0].quantity).toBe(10); // From mockGenerateProposals
        expect(result.allocations[0].recipient_need_slot_id).toBe("n1");
    });

    it("should trigger recipient update when allocations are received", async () => {
        // Setup Inputs
        const myNeeds = [{ id: "n1", quantity: 50, type_id: "food" }];
        mockMyCommitmentStore.set({ capacity_slots: [], need_slots: myNeeds });

        // Simulate incoming allocation
        const incomingAlloc = [{
            availability_slot_id: "c1",
            recipient_need_slot_id: "n1",
            quantity: 5,
            recipient_pubkey: "me",
            provider_pubkey: "them"
        }];

        const allocMap = new Map();
        allocMap.set("them", incomingAlloc);
        mockNetworkAllocations.set(allocMap);

        // Subscribe to force reactivity
        const unsubscribe = myAllocationsAsProvider.subscribe(() => { });

        // Wait for reactivity? Recipient loop is separate derived. 
        // We need to verify if updateRecipientState was called.
        // The recipient loop is a standalone derived().subscribe(...) in allocation.svelte.ts.
        // Importing the module (done in beforeEach) starts the subscriptions.

        // We might need to wait a tick
        await new Promise(r => setTimeout(r, 10));

        expect(mockUpdateRecipient).toHaveBeenCalled();
        unsubscribe();
    });
});
