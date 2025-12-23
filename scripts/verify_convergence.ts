
/**
 * Convergence Verification Script
 * 
 * Simulates a 2-provider, 1-recipient scenario to compare:
 * 1. Current Implementation (Input Damping)
 * 2. Proposed Implementation (Update Damping)
 * 
 * Target: Need = 100
 * Providers: A (Capacity=100), B (Capacity=100)
 * Damping: 0.8
 */

const NEED = 100;
const CAPACITY_A = 100;
const CAPACITY_B = 100;
const DAMPING = 0.8;
const MAX_ITER = 20;

type Strategy = 'input-damping' | 'update-damping';

function runSimulation(strategy: Strategy) {
    console.log(`\n--- Running Simulation: ${strategy} ---`);

    let allocA = 0;
    let allocB = 0;

    for (let t = 1; t <= MAX_ITER; t++) {
        const totalAlloc = allocA + allocB;
        const satisfaction = (totalAlloc / NEED) * 100;

        console.log(`Iter ${t}: A=${allocA.toFixed(2)}, B=${allocB.toFixed(2)}, Total=${totalAlloc.toFixed(2)} (${satisfaction.toFixed(1)}%)`);

        if (Math.abs(totalAlloc - NEED) < 0.01) {
            console.log(`✅ Converged in ${t} iterations!`);
            return;
        }

        // Provider A Perspective
        // 1. Calculate what B provided
        const othersAmountA = allocB;

        // 2. Calculate Allocation
        let newAllocA = 0;

        if (strategy === 'input-damping') {
            // CURRENT LOGIC:
            // remaining = Need - others;
            // active = remaining * damping;
            const remaining = Math.max(0, NEED - othersAmountA);
            const active = remaining * DAMPING;
            // Allocation cannot exceed capacity or active need
            newAllocA = Math.min(CAPACITY_A, active);
        } else {
            // PROPOSED LOGIC (Update Damping):
            // target = Need - others; (Full Need!)
            // diff = target - current;
            // new = current + diff * damping;
            const target = Math.max(0, NEED - othersAmountA);
            // Constrain target by capacity
            const constrainedTarget = Math.min(CAPACITY_A, target);

            const diff = constrainedTarget - allocA;
            newAllocA = allocA + (diff * DAMPING);
        }

        // Provider B Perspective (Symmetric)
        const othersAmountB = allocA;
        let newAllocB = 0;

        if (strategy === 'input-damping') {
            const remaining = Math.max(0, NEED - othersAmountB);
            const active = remaining * DAMPING;
            newAllocB = Math.min(CAPACITY_B, active);
        } else {
            const target = Math.max(0, NEED - othersAmountB);
            const constrainedTarget = Math.min(CAPACITY_B, target);
            const diff = constrainedTarget - allocB;
            newAllocB = allocB + (diff * DAMPING);
        }

        // Update State (Synchronous update)
        allocA = newAllocA;
        allocB = newAllocB;
    }

    console.log(`❌ Failed to converge after ${MAX_ITER} iterations.`);
}

console.log(`Simulation Parameters: Need=${NEED}, Damping=${DAMPING}`);
runSimulation('input-damping');
runSimulation('update-damping');
