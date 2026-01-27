
import {
    type NeedSlot,
    type AvailabilitySlot,
    type Contact
} from './resources.js';
import { type GlobalRecognitionWeights } from './recognition.js';
import {
    FeasibilityStatusSchema,
    type FeasibilityStatus,
    type SlotRelationship,
    FeasibilityScoresSchema,
    type FeasibilityScores
} from './desire.js';
import {
    availabilityWindowsOverlapWithTimezone,
    skillsCompatible,
    calculateAvailabilityIntersection
} from './match.js';
import {
    computeH3Index,
    getCellsInRadius,
    cellsCompatible,
    haversineDistance,
    REMOTE_H3_INDEX,
    DEFAULT_SEARCH_RADIUS_KM
} from './spatial.js';

// ═══════════════════════════════════════════════════════════════════
// SCORE CALCULATORS (7 Dimensions)
// ═══════════════════════════════════════════════════════════════════

/**
 * 1. TIME SCORE (Binary + Duration Check)
 * Returns 1.0 if meaningful overlap exists, 0.0 if not.
 * HEURISTIC: "Minimum Usable Chunk" - Overlap must be > min_atomic_size.
 */
function scoreTime(need: NeedSlot, capacity: AvailabilitySlot): number {
    const overlaps = availabilityWindowsOverlapWithTimezone(
        need.availability_window,
        capacity.availability_window,
        need.time_zone,
        capacity.time_zone
    );

    if (!overlaps) return 0.0;

    // Check Minimum Atomic Size (Granularity)
    // Primary: min_atomic_size.
    const minSize = need.min_atomic_size;

    // We need to calculate the *size* of the overlap.
    if (minSize && minSize > 0) {
        if (!need.availability_window || !capacity.availability_window) {
            return 1.0; // Optimistic if windows missing (should be handled by overlaps check above theoretically)
        }

        const intersection = calculateAvailabilityIntersection(
            need.availability_window,
            capacity.availability_window,
            need.time_zone,
            capacity.time_zone
        );

        let maxBlockMinutes = 0;
        if (intersection.day_schedules) {
            for (const ds of intersection.day_schedules) {
                for (const range of ds.time_ranges) {
                    const [h1, m1] = range.start_time.split(':').map(Number);
                    const [h2, m2] = range.end_time.split(':').map(Number);
                    const mins = (h2 * 60 + m2) - (h1 * 60 + m1);
                    if (mins > maxBlockMinutes) maxBlockMinutes = mins;
                }
            }
        }

        if (maxBlockMinutes < minSize) {
            return 0.0; // Overlap is too fragmented/short to be useful
        }
    }

    return 1.0;
}

// ... (scoreLocation, scoreSkills, scoreTravel, scoreResources... unchanged) ...

/**
 * 7. CONTINUITY SCORE (Fragmentation)
 * Penalizes fragmented time overlaps.
 * HEURISTIC: If avg block size < min_duration, heavily penalize.
 */
function scoreContinuity(need: NeedSlot, capacity: AvailabilitySlot): number {
    if (!need.availability_window || !capacity.availability_window) return 1.0;

    // Calculate intersection
    const intersection = calculateAvailabilityIntersection(
        need.availability_window,
        capacity.availability_window,
        need.time_zone,
        capacity.time_zone
    );

    let blockCount = 0;
    let totalMinutes = 0;

    if (intersection.day_schedules) {
        for (const ds of intersection.day_schedules) {
            blockCount += ds.time_ranges.length;
            for (const range of ds.time_ranges) {
                const [h1, m1] = range.start_time.split(':').map(Number);
                const [h2, m2] = range.end_time.split(':').map(Number);
                totalMinutes += ((h2 * 60 + m2) - (h1 * 60 + m1));
            }
        }
    }

    if (blockCount === 0) return 0.0; // Should be handled by scoreTime, but safety first
    if (blockCount <= 1) return 1.0;

    // Penalty for fragmentation
    let score = 1.0 / blockCount;

    // Additional penalty if blocks are tiny (below preferred duration)
    // If we have a min_duration, use it as a soft target
    const targetBlockSize = need.min_atomic_size ?? 60;
    const avgBlockSize = totalMinutes / blockCount;

    if (avgBlockSize < targetBlockSize) {
        score *= (avgBlockSize / targetBlockSize); // Linear penalty for short blocks
    }

    return score;
}

/**
 * 2. LOCATION SCORE (Distance Decay)
 * 1.0 at distance 0, decaying to 0.0 at max_radius.
 */
function scoreLocation(need: NeedSlot, capacity: AvailabilitySlot): number {
    // If either is remote, perfect match
    if (need.h3_index === REMOTE_H3_INDEX || capacity.h3_index === REMOTE_H3_INDEX) {
        return 1.0;
    }

    // If no coordinates (and not remote), fail or optimistic 1.0?
    // Let's be strict: if location matching required but missing coords, fail?
    // For now, if missing coords but passed index check, assume 1.0 (optimistic)
    if (!need.latitude || !need.longitude || !capacity.latitude || !capacity.longitude) {
        return 1.0;
    }

    const dist = haversineDistance(
        need.latitude, need.longitude,
        capacity.latitude, capacity.longitude
    );

    const maxRadius = need.search_radius_km ?? DEFAULT_SEARCH_RADIUS_KM;

    if (dist > maxRadius) return 0.0;

    // Simple linear decay: 1.0 at 0km, 0.0 at maxRadius
    return Math.max(0, 1.0 - (dist / maxRadius));
}

/**
 * 3. SKILLS SCORE (Level Aware)
 * 1.0 if all requirements met, 0.0 otherwise.
 * HEURISTIC: Checks `level` if present.
 */
function scoreSkills(
    need: NeedSlot,
    capacity: AvailabilitySlot,
    provider?: Contact,
    seeker?: Contact
): number {
    // 1. Forward Check (Provider has skills for Need)
    if (need.required_skills && need.required_skills.length > 0) {
        if (!provider) return 1.0; // Optimistic if unknown provider

        for (const req of need.required_skills) {
            const match = provider.skills.find(s => s.id === req.id);
            if (!match) return 0.0; // Missing skill

            // Level Check (if both have levels)
            if (req.level !== undefined && match.level !== undefined) {
                if (Number(match.level) < Number(req.level)) return 0.0; // Level too low
            }
        }
    }

    // 2. Reverse Check (Seeker meets Capacity rqmts)
    if (capacity.required_skills && capacity.required_skills.length > 0) {
        if (!seeker) return 1.0;

        for (const req of capacity.required_skills) {
            const match = seeker.skills.find(s => s.id === req.id);
            if (!match) return 0.0;

            if (req.level !== undefined && match.level !== undefined) {
                if (Number(match.level) < Number(req.level)) return 0.0;
            }
        }
    }

    return 1.0;
}

/**
 * 4. TRAVEL SCORE (Spatio-Temporal + Tortuosity)
 * Checks if this match contradicts *other* commitments due to travel time.
 * HEURISTIC: Tortuosity Factor (1.5x) for real-world travel estimation.
 */
function scoreTravel(
    need: NeedSlot,
    capacity: AvailabilitySlot,
    previousCommitment?: { latitude: number; longitude: number; end_time: string }
): number {
    if (!previousCommitment) return 1.0; // No previous context = feasible
    if (!capacity.latitude || !capacity.longitude) return 1.0; // No loc data = optimistic

    // 1. Calculate Distance (km)
    const rawDist = haversineDistance(
        previousCommitment.latitude, previousCommitment.longitude,
        capacity.latitude, capacity.longitude
    );

    if (rawDist <= 0.1) return 1.0; // Same location

    // HEURISTIC: Tortuosity Factor
    // Real road distance is usually ~1.4 to 1.5x the straight line distance.
    const TORTUOSITY_FACTOR = 1.5;
    const realDist = rawDist * TORTUOSITY_FACTOR;

    // 2. Calculate Time Delta (hours)
    // Assume capacity start time is the target time.
    // Need to parse times. Simple HH:MM comparison for now (assuming same day/UTC).
    // TODO: Full Date comparison from `start_date` if available.

    // Helper to get minutes from HH:MM
    const getMinutes = (t: string) => {
        const [h, m] = t.split(':').map(Number);
        return h * 60 + m;
    };

    // Get earliest start of new slot
    const newStartStr = capacity.availability_window?.time_ranges?.[0]?.start_time;
    if (!newStartStr) return 1.0;

    const prevEndMins = getMinutes(previousCommitment.end_time);
    const newStartMins = getMinutes(newStartStr);

    let deltaMins = newStartMins - prevEndMins;

    // Handle day wrap (not perfect without dates, but defensive)
    if (deltaMins < 0) return 0.0; // Sequence error (new starts before old ends)

    const deltaHours = deltaMins / 60;
    if (deltaHours <= 0) return 0.0; // Instant teleportation required

    // 3. Calculate Required Speed
    const requiredSpeedKmH = realDist / deltaHours;
    const MAX_SPEED_KMH = 80; // Reasonable urban travel limit
    const COMFORT_SPEED_KMH = 30; // Stress-free travel

    if (requiredSpeedKmH > MAX_SPEED_KMH) {
        return 0.0; // Impossible
    }

    // 4. Score based on "Rush Factor"
    // If we need max speed, score is low (risky).
    // If we can take our time, score is high.
    // 30km/h = 1.0 score. 80km/h = 0.5 score? 
    // Linear interpolation:
    if (requiredSpeedKmH <= COMFORT_SPEED_KMH) return 1.0;

    // Decay from 1.0 at 30km/h to 0.1 at 80km/h
    const ratio = (requiredSpeedKmH - COMFORT_SPEED_KMH) / (MAX_SPEED_KMH - COMFORT_SPEED_KMH);
    return Math.max(0.1, 1.0 - (ratio * 0.9));
}

/**
 * 5. RESOURCES SCORE (Quantity)
 * 1.0 if enough quantity offered, penalized if partial.
 */
function scoreResources(need: NeedSlot, capacity: AvailabilitySlot): number {
    if (capacity.quantity >= need.quantity) return 1.0;
    if (capacity.quantity <= 0) return 0.0;

    // Partial fulfillment
    return capacity.quantity / need.quantity;
}

/**
 * 6. AFFINITY SCORE (Social Trust)
 * Uses global recognition weights if available.
 */
function scoreAffinity(
    targetPubkey: string | undefined,
    weights?: GlobalRecognitionWeights | null
): number {
    if (!targetPubkey || !weights) return 1.0; // No data = Neutral/Optimistic

    const weight = weights[targetPubkey];
    return weight !== undefined ? weight : 0.1; // Low default if not trusted?
}



// ═══════════════════════════════════════════════════════════════════
// MAIN FEASIBILITY CHECKER
// ═══════════════════════════════════════════════════════════════════

export interface FeasibilityContext {
    provider?: Contact;
    seeker?: Contact;
    providerWeights?: GlobalRecognitionWeights | null; // Seeker's recognition of Provider
    seekerWeights?: GlobalRecognitionWeights | null;   // Provider's recognition of Seeker

    // Context for Spatio-Temporal checks
    previousCommitment?: {
        latitude: number;
        longitude: number;
        end_time: string; // HH:MM
    };
}

export function calculateFeasibility(
    need: NeedSlot,
    capacity: AvailabilitySlot,
    context: FeasibilityContext = {}
): FeasibilityStatus {
    const scores: FeasibilityScores = {
        time: scoreTime(need, capacity),
        location: scoreLocation(need, capacity),
        skills: scoreSkills(need, capacity, context.provider, context.seeker),
        travel: scoreTravel(need, capacity, context.previousCommitment),
        resources: scoreResources(need, capacity),

        // Two-way affinity? 
        // Usually "Does Seeker trust Provider?" AND "Does Provider trust Seeker?"
        // We take the MINIMUM (bottleneck).
        affinity: Math.min(
            scoreAffinity(capacity.offered_by, context.seekerWeights), // Seeker -> Provider
            scoreAffinity(need.offered_by, context.providerWeights)    // Provider -> Seeker
        ),

        continuity: scoreContinuity(need, capacity)
    };

    const reasons: string[] = [];
    if (scores.time === 0) reasons.push('TIME_MISMATCH');
    if (scores.location === 0) reasons.push('LOCATION_MISMATCH');
    if (scores.skills === 0) reasons.push('SKILL_MISMATCH');
    if (scores.travel === 0) reasons.push('TRAVEL_TIME_VIOLATION');
    if (scores.resources === 0) reasons.push('OTHER');
    if (scores.affinity === 0) reasons.push('OTHER'); // "Blocked"?

    if (reasons.length > 0) {
        return {
            type: 'impossible',
            reasons: reasons as any[],
            scores
        };
    }

    // Confidence = Product of scores? Or Minimum?
    // "Chain is as strong as weakest link" -> Product or Min.
    // Product penalizes more heavily (0.9 * 0.9 = 0.81).
    // Let's use Product for aggregate confidence.
    const confidence =
        scores.time *
        scores.location *
        scores.skills *
        scores.travel *
        scores.resources *
        scores.affinity *
        scores.continuity;

    const risk_factors: string[] = [];
    if (scores.continuity < 1) risk_factors.push('FRAGMENTED_TIME');
    if (scores.travel < 1) risk_factors.push('TIGHT_TRAVEL_TIME');
    if (scores.resources < 1) risk_factors.push('PARTIAL_QUANTITY');
    if (scores.affinity < 0.5) risk_factors.push('LOW_TRUST');

    return {
        type: 'possible',
        confidence,
        risk_factors: risk_factors.length > 0 ? risk_factors : undefined,
        scores
    };
}
