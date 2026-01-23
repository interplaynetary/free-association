
import { describe, it, expect } from 'vitest';
import { slotsCompatible } from '../match';
import type { NeedSlot, AvailabilitySlot } from '../resources';

// Helper to create minimal valid slots
const createNeed = (overrides: Partial<NeedSlot> = {}): NeedSlot => ({
    id: 'need1',
    name: 'Test Need',
    type_id: 'type1',
    quantity: 1,
    ...overrides
});

const createCap = (overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot => ({
    id: 'cap1',
    name: 'Test Cap',
    type_id: 'type1',
    quantity: 10,
    ...overrides
});

describe('Generalized Flow Constraints in Matcher', () => {

    describe('1. Granularity (min_atomic_size)', () => {
        it('should reject if need quantity is below atomic size', () => {
            const cap = createCap({ min_atomic_size: 10 });
            const need = createNeed({ quantity: 5 });
            expect(slotsCompatible(need, cap)).toBe(false);
        });

        it('should accept if need quantity is equal to or above atomic size', () => {
            const cap = createCap({ min_atomic_size: 10 });
            const need1 = createNeed({ quantity: 10 });
            const need2 = createNeed({ quantity: 15 });

            expect(slotsCompatible(need1, cap)).toBe(true);
            expect(slotsCompatible(need2, cap)).toBe(true);
        });
    });

    describe('2. Physics Floor (min_calendar_duration)', () => {
        it('should reject if overlap is fragmented and chunks are too small', () => {
            // Cap: 10:00-10:20, 10:40-11:00 (two 20m chunks)
            // Need: 10:00-11:00
            // Min Duration: 30m
            const cap = createCap({
                min_calendar_duration: 30, // minutes (stored as number, usually minutes or hours depending on system, let's assume minutes here for test simplicity, though system might use hours. Protocol uses generic units, usually hours for duration? Let's check impl. resources.ts says positive number. usually hours in this codebase.)
                // Actually resources.ts says: min_calendar_duration: z.number().positive().optional() // Physics floor (Min total time)
                // Let's assume hours to match advance_notice_hours
                availability_window: {
                    time_ranges: [
                        { start_time: '10:00', end_time: '10:20' }, // 20m = 0.33h
                        { start_time: '10:40', end_time: '11:00' }
                    ]
                }
            });
            // Override min_calendar_duration to 0.5 hours (30 mins)
            cap.min_calendar_duration = 0.5;

            const need = createNeed({
                start_date: '2024-01-01',
                availability_window: {
                    time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
                }
            });

            // Need to provide a date for cap/need context so overlap can be calculated
            need.start_date = '2024-01-01';

            // This relies on match.ts supporting this check. currently it fails (returns true)
            expect(slotsCompatible(need, cap)).toBe(false);
        });

        it('should accept if at least one continuous overlap meets the min duration', () => {
            const cap = createCap({
                min_calendar_duration: 1.0, // 1 hour
                availability_window: {
                    time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
                }
            });

            const need = createNeed({
                start_date: '2024-01-01',
                availability_window: {
                    time_ranges: [{ start_time: '10:00', end_time: '11:30' }] // 1.5h overlap
                }
            });

            expect(slotsCompatible(need, cap)).toBe(true);
        });
    });

    describe('3. Lead Time (advance_notice_hours)', () => {
        // Assume slotsCompatible signature updated to: (need, cap, referenceTime?)
        // referenceTime is ISO string or Date object
        const now = new Date('2024-01-01T08:00:00Z'); // 8 AM UTC

        const cap = createCap({
            advance_notice_hours: 2, // Requires booking 2h in advance
            availability_window: {
                time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
            }
        });

        it('should reject if slot starts too soon relative to now', () => {
            const need = createNeed({
                start_date: '2024-01-01', // Converted to 09:00 start (matches cap)
                // 09:00 is only 1h from 08:00. Required 2h.
                availability_window: {
                    time_ranges: [{ start_time: '09:00', end_time: '10:00' }]
                }
            });

            // @ts-ignore - Argument length check until updated
            expect(slotsCompatible(need, cap, now.toISOString())).toBe(false);
        });

        it('should accept if slot is far enough in future', () => {
            const need = createNeed({
                start_date: '2024-01-01',
                availability_window: {
                    time_ranges: [{ start_time: '11:00', end_time: '12:00' }]
                }
            });
            // 11:00 is 3h from 08:00. Required 2h. Safe.

            // @ts-ignore
            expect(slotsCompatible(need, cap, now.toISOString())).toBe(true);
        });
    });

    describe('4. Booking Window (booking_window_hours)', () => {
        const now = new Date('2024-01-01T08:00:00Z');
        const cap = createCap({
            booking_window_hours: 24, // Can only book within next 24h
        });

        it('should reject if slot is too far in future', () => {
            const need = createNeed({
                start_date: '2024-01-05', // 4 days later
            });

            // @ts-ignore
            expect(slotsCompatible(need, cap, now.toISOString())).toBe(false);
        });

        it('should accept if slot is within window', () => {
            const need = createNeed({
                start_date: '2024-01-01', // Same day
            });

            // @ts-ignore
            expect(slotsCompatible(need, cap, now.toISOString())).toBe(true);
        });
    });

    describe('5. Participation / Fan-In (max_participation)', () => {
        it('should reject if need members exceed max participation', () => {
            const cap = createCap({ max_participation: 2 });
            const need = createNeed({
                members: ['alice', 'bob', 'charlie'] // 3 members
            });

            expect(slotsCompatible(need, cap)).toBe(false);
        });

        it('should accept if need members fit within limit', () => {
            const cap = createCap({ max_participation: 5 });
            const need = createNeed({
                members: ['alice', 'bob']
            });

            expect(slotsCompatible(need, cap)).toBe(true);
        });
        describe('6. Validity Bounds (start_date / end_date)', () => {
            const recurringCap = createCap({
                start_date: '2024-01-01',
                end_date: '2024-06-30', // Valid only Jan-June
                recurrence: 'weekly',
                availability_window: {
                    day_schedules: [{
                        days: ['monday'],
                        time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
                    }]
                }
            });

            it('should accept if one-time need falls within validity period', () => {
                const need = createNeed({
                    start_date: '2024-03-04', // A Monday in March (Inside)
                    start_time: '10:00',
                    end_time: '11:00'
                });
                expect(slotsCompatible(need, recurringCap)).toBe(true);
            });

            it('should reject if one-time need falls AFTER validity period', () => {
                const need = createNeed({
                    start_date: '2024-07-01', // A Monday in July (Outside)
                    // Adding availability_window forces the "structured" path in match.ts
                    availability_window: {
                        time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
                    }
                });
                expect(slotsCompatible(need, recurringCap)).toBe(false);
            });

            it('should reject if one-time need falls BEFORE validity period', () => {
                const need = createNeed({
                    start_date: '2023-12-25', // A Monday in Dec 2023 (Outside)
                    availability_window: {
                        time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
                    }
                });
                expect(slotsCompatible(need, recurringCap)).toBe(false);
            });
        });
    });

});
