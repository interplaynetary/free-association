
import { describe, it, expect } from "bun:test";
import { calculateIPFAllocation, type SlotAllocationRecord } from "./allocation-ipf";
import type { AvailabilitySlot, NeedSlot, Commitment } from "./schemas";

describe("IPF Fuzzing / Stress Tests", () => {

    const SLOT_COUNT = 50; // Total providers
    const NEED_COUNT = 50; // Total recipients

    // Helper to generate random string
    const rnd = () => Math.random().toString(36).substring(7);

    it("should stabilize on large random networks", () => {
        const capacitySlots: AvailabilitySlot[] = [];
        const needSlots: NeedSlot[] = [];
        const commitments: Record<string, Commitment> = {};

        // 1. Generate Providers
        for (let i = 0; i < SLOT_COUNT; i++) {
            const pid = `prov_${i}`;
            const cid = `cap_${i}`;
            const qty = Math.floor(Math.random() * 100) + 1; // 1-100

            const cap: AvailabilitySlot = {
                id: cid,
                name: `Cap ${i}`,
                quantity: qty,
                type_id: "resource",
                // Randomly link to owner?
                // Just use separate commitments logic
            };
            capacitySlots.push(cap);

            commitments[pid] = {
                timestamp: Date.now(),
                capacity_slots: [cap],
                need_slots: [],
                itcStamp: null,
                global_recognition_weights: {} // Populated later
            };
        }

        // 2. Generate Needs (Recipients)
        for (let i = 0; i < NEED_COUNT; i++) {
            const rid = `rec_${i}`;
            const nid = `need_${i}`;
            const qty = Math.floor(Math.random() * 100) + 1;

            const need: NeedSlot = {
                id: nid,
                name: `Need ${i}`,
                quantity: qty,
                type_id: "resource"
            };
            needSlots.push(need);

            commitments[rid] = {
                timestamp: Date.now(),
                capacity_slots: [],
                need_slots: [need],
                itcStamp: null,
                global_recognition_weights: {}
            };
        }

        // 3. Generate Random Trust (Recognition)
        // Only if P recognizes R do we get flow (mostly)
        const allPids = Object.keys(commitments).filter(k => k.startsWith('prov'));
        const allRids = Object.keys(commitments).filter(k => k.startsWith('rec'));

        // Populate Provider commitments (Recognize Recipients)
        for (const pid of allPids) {
            // Recognize random 20% of recipients
            allRids.forEach(rid => {
                if (Math.random() < 0.2) {
                    commitments[pid].global_recognition_weights![rid] = Math.random();
                }
            });
        }

        // Populate Recipient commitments (Recognize Providers) - Gamma influence
        for (const rid of allRids) {
            allPids.forEach(pid => {
                if (Math.random() < 0.2) {
                    commitments[rid].global_recognition_weights![pid] = Math.random();
                }
            });
        }

        console.log(`[FUZZ] Running on ${SLOT_COUNT} caps, ${NEED_COUNT} needs...`);
        const start = performance.now();

        const allocations = calculateIPFAllocation(
            capacitySlots,
            needSlots,
            commitments,
            { debug: false, maxIterations: 50, gamma: 0.5 }
        );

        const duration = performance.now() - start;
        console.log(`[FUZZ] Completed in ${duration.toFixed(2)}ms. Allocations: ${allocations.length}`);

        // 4. Assertions

        // Capacity Constraint Check
        const allocatedPerCap = new Map<string, number>();
        for (const a of allocations) {
            const current = allocatedPerCap.get(a.capacity_slot_id) || 0;
            allocatedPerCap.set(a.capacity_slot_id, current + a.quantity);
        }

        for (const c of capacitySlots) {
            const total = allocatedPerCap.get(c.id!) || 0;
            expect(total).toBeLessThanOrEqual(c.quantity + 0.001); // Float tolerance
        }

        // Need Constraint Check
        const allocatedPerNeed = new Map<string, number>();
        for (const a of allocations) {
            const current = allocatedPerNeed.get(a.need_slot_id) || 0;
            allocatedPerNeed.set(a.need_slot_id, current + a.quantity);
        }

        for (const n of needSlots) {
            const total = allocatedPerNeed.get(n.id!) || 0;
            expect(total).toBeLessThanOrEqual(n.quantity + 0.001);
        }

        expect(allocations.length).toBeGreaterThan(0);
    });
});
