/**
 * Time Intersection Logic Tests
 */

import { describe, it, expect } from 'vitest';
import {
    intersectTimeRanges,
    calculateAvailabilityIntersection
} from '../match';
import type { AvailabilityWindow, TimeRange } from '../resources';

describe('Time Intersection', () => {

    describe('intersectTimeRanges', () => {
        it('should find intersection of simple overlapping ranges', () => {
            const r1 = [{ start_time: '09:00', end_time: '12:00' }];
            const r2 = [{ start_time: '11:00', end_time: '14:00' }];

            const result = intersectTimeRanges(r1, r2);
            expect(result).toHaveLength(1);
            expect(result[0]).toEqual({ start_time: '11:00', end_time: '12:00' });
        });

        it('should return empty for non-overlapping ranges', () => {
            const r1 = [{ start_time: '09:00', end_time: '10:00' }];
            const r2 = [{ start_time: '11:00', end_time: '12:00' }];

            const result = intersectTimeRanges(r1, r2);
            expect(result).toHaveLength(0);
        });

        it('should handle subset ranges', () => {
            const r1 = [{ start_time: '09:00', end_time: '17:00' }];
            const r2 = [{ start_time: '12:00', end_time: '13:00' }];

            const result = intersectTimeRanges(r1, r2);
            expect(result).toHaveLength(1);
            expect(result[0]).toEqual({ start_time: '12:00', end_time: '13:00' });
        });

        it('should handle multi-segment intersections', () => {
            // A:   [9-12]       [14-17]
            // B:      [11--13]      [16--18]
            // Res:    [11-12]       [16-17]
            const r1 = [
                { start_time: '09:00', end_time: '12:00' },
                { start_time: '14:00', end_time: '17:00' }
            ];
            const r2 = [
                { start_time: '11:00', end_time: '13:00' },
                { start_time: '16:00', end_time: '18:00' }
            ];

            const result = intersectTimeRanges(r1, r2);
            expect(result).toHaveLength(2);
            expect(result[0]).toEqual({ start_time: '11:00', end_time: '12:00' });
            expect(result[1]).toEqual({ start_time: '16:00', end_time: '17:00' });
        });
    });

    describe('calculateAvailabilityIntersection', () => {
        it('should intersect availability windows on same day', () => {
            const w1: AvailabilityWindow = {
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
                }]
            };
            const w2: AvailabilityWindow = {
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '12:00', end_time: '14:00' }]
                }]
            };

            const result = calculateAvailabilityIntersection(w1, w2);
            expect(result.day_schedules).toHaveLength(1);
            expect(result.day_schedules![0].days).toEqual(['monday']);
            expect(result.day_schedules![0].time_ranges[0]).toEqual({
                start_time: '12:00', end_time: '14:00'
            });
        });

        it('should handle different days (no intersection)', () => {
            const w1: AvailabilityWindow = {
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
                }]
            };
            const w2: AvailabilityWindow = {
                day_schedules: [{
                    days: ['tuesday'],
                    time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
                }]
            };

            const result = calculateAvailabilityIntersection(w1, w2);
            expect(result.day_schedules).toHaveLength(0);
        });

        // Test with timezone awareness if supported (mocking timezone logic)
        it('should handle simple UTC matching', () => {
            const w1: AvailabilityWindow = {
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '10:00', end_time: '12:00' }]
                }]
            };
            const w2: AvailabilityWindow = {
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '11:00', end_time: '13:00' }]
                }]
            };

            const result = calculateAvailabilityIntersection(w1, w2, 'UTC', 'UTC');
            expect(result.day_schedules).toHaveLength(1);
            expect(result.day_schedules![0].time_ranges[0]).toEqual({
                start_time: '11:00', end_time: '12:00'
            });
        });
    });
});

describe('Complex Schedules & Timezones', () => {
    const { availabilityWindowsOverlapWithTimezone } = require('../match');

    it('should respect Month-Specific patterns', () => {
        // Window available ONLY in February
        const w1: AvailabilityWindow = {
            month_schedules: [{
                month: 2, // February
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
                }]
            }]
        };

        const w2: AvailabilityWindow = {
            time_ranges: [{ start_time: '09:00', end_time: '17:00' }] // "Every day"
        };

        // Test on a Monday in February (2024-02-05) -> Should Match
        const matchFeb = availabilityWindowsOverlapWithTimezone(w1, w2, 'UTC', 'UTC', '2024-02-05');
        expect(matchFeb).toBe(true);

        // Test on a Monday in March (2024-03-04) -> Should NOT Match
        const matchMar = availabilityWindowsOverlapWithTimezone(w1, w2, 'UTC', 'UTC', '2024-03-04');
        expect(matchMar).toBe(false);
    });

    it('should respect Week-Specific patterns', () => {
        // Window available ONLY in 1st Week
        const w1: AvailabilityWindow = {
            week_schedules: [{
                weeks: [1],
                day_schedules: [{
                    days: ['monday'],
                    time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
                }]
            }]
        };

        const w2: AvailabilityWindow = {
            day_schedules: [{ days: ['monday'], time_ranges: [{ start_time: '09:00', end_time: '17:00' }] }]
        };

        // Test on 1st Monday of Month (2024-02-05) -> Should Match (Feb 5 is in first 7 days? Week 1 usually days 1-7)
        // 2024-02-05 is day 5. Week = ceil(5/7) = 1.
        const matchWeek1 = availabilityWindowsOverlapWithTimezone(w1, w2, 'UTC', 'UTC', '2024-02-05');
        expect(matchWeek1).toBe(true);

        // Test on 2nd Monday (2024-02-12) -> Should NOT Match
        // 2024-02-12 is day 12. Week = ceil(12/7) = 2.
        const matchWeek2 = availabilityWindowsOverlapWithTimezone(w1, w2, 'UTC', 'UTC', '2024-02-12');
        expect(matchWeek2).toBe(false);
    });

    it('should handle Timezone Day-Shifts', () => {
        // Provider: Los Angeles (UTC-8), Mondays 11pm (23:00)
        // Local: Mon 23:00 -> UTC: Tue 07:00 (next day)
        const wLA: AvailabilityWindow = {
            day_schedules: [{
                days: ['monday'],
                time_ranges: [{ start_time: '23:00', end_time: '23:59' }]
            }]
        };

        // Seeker: London (UTC+0), Tuesdays 7am (07:00)
        const wLondon: AvailabilityWindow = {
            day_schedules: [{
                days: ['tuesday'],
                time_ranges: [{ start_time: '07:00', end_time: '08:00' }]
            }]
        };

        // Check overlap using a sample date.
        // We need a sample date where "Monday in LA" corresponds to "Tuesday in London".
        // e.g. Mon Feb 5th LA -> Tue Feb 6th London.
        // Using '2024-02-05' (Monday).

        const overlaps = availabilityWindowsOverlapWithTimezone(
            wLA,
            wLondon,
            'America/Los_Angeles',
            'Europe/London',
            '2024-02-05'
        );

        expect(overlaps).toBe(true);
    });
});

