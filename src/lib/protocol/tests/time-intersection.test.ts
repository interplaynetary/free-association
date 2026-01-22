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
