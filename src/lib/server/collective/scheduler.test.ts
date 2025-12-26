
import { describe, it, expect } from "bun:test";
import { computeAllocations } from "./scheduler";
import type { AvailabilitySlot, NeedSlot } from "$lib/protocol/schemas";

describe("Scheduler Adapter Logic", () => {

    it("should sanitize nullable recurrence in capacity", () => {
        const capacity: AvailabilitySlot = {
            id: "cap1",
            quantity: 100,
            need_type_id: "food",
            name: "My Capacity",
            recurrence: null as any, // Simulate DB returning null
        };

        const need: NeedSlot = {
            id: "need1",
            quantity: 50,
            need_type_id: "food",
            name: "My Need",
            recurrence: null as any,
        };

        const needsMap = new Map<string, NeedSlot>();
        needsMap.set("need1", need);

        // Should not throw
        const result = computeAllocations(capacity, needsMap, new Map());

        expect(result.total_allocated).toBeGreaterThan(0);
        expect(result.allocations.length).toBeGreaterThan(0);
    });

    it("should correctly map allocations to result structure", () => {
        const capacity: AvailabilitySlot = {
            id: "cap1",
            quantity: 100,
            need_type_id: "labor",
            name: "Labor Cap",
        };

        const need: NeedSlot = {
            id: "need1",
            quantity: 100,
            need_type_id: "labor",
            name: "Labor Need",
        };

        const needsMap = new Map([["n1", need]]);

        const result = computeAllocations(capacity, needsMap, new Map());

        expect(result.total_capacity).toBe(100);
        expect(result.total_allocated).toBeCloseTo(100, 1);
        expect(result.allocations[0].capacity_slot_id).toBe("cap1");
    });
});
