/**
 * Comprehensive Test Suite for Free Association Protocol
 * 
 * Tests all components:
 * - Core matrix operations (RS, MR, MRS, SCMRS, MRD)
 * - Slot matching (time, location, multi-dimensional)
 * - Allocation engine (damping, divisibility, convergence)
 * - Sparse matrix optimization
 * - RPC interfaces and validation
 * - Edge cases and error handling
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  MatrixComputer,
  SlotManager,
  AllocationEngine,
  DampeningSystem,
  DivisibilityConstraints,
  LargestRemainderMethod,
  ConvergenceTracker,
  TimeMatching,
  LocationMatching,
  SlotMatching,
  SpaceTimeIndex,
  type NeedSlot,
  type AvailabilitySlot,
  type MatrixResult,
  type ConvergenceMetrics
} from './index';
import { Sparse } from './sparse-matrix';

// ============================================================================
// CORE MATRIX OPERATIONS TESTS
// ============================================================================

describe('MatrixComputer - Core Operations', () => {
  let computer: MatrixComputer;
  
  beforeEach(() => {
    computer = new MatrixComputer(3);
  });
  
  describe('Recognition Matrix Setup', () => {
    it('should initialize with correct dimensions', () => {
      expect(() => computer.setRecognition(0, 0, 0.5)).not.toThrow();
      expect(() => computer.setRecognition(2, 2, 0.5)).not.toThrow();
    });
    
    it('should reject out-of-bounds indices', () => {
      expect(() => computer.setRecognition(-1, 0, 0.5)).toThrow('Index out of bounds');
      expect(() => computer.setRecognition(0, 3, 0.5)).toThrow('Index out of bounds');
      expect(() => computer.setRecognition(3, 0, 0.5)).toThrow('Index out of bounds');
    });
    
    it('should reject invalid recognition values', () => {
      expect(() => computer.setRecognition(0, 1, -0.1)).toThrow('must be in [0, 1]');
      expect(() => computer.setRecognition(0, 1, 1.1)).toThrow('must be in [0, 1]');
    });
    
    it('should set and get recognition values correctly', () => {
      computer.setRecognition(0, 1, 0.6);
      expect(computer.getRecognition(0, 1)).toBe(0.6);
      expect(computer.getRecognition(1, 0)).toBe(0); // Unset should be 0
    });
    
    it('should handle sparse matrices efficiently', () => {
      // Set only a few values in large matrix
      const largeComputer = new MatrixComputer(100);
      largeComputer.setRecognition(0, 1, 0.5);
      largeComputer.setRecognition(1, 2, 0.5);
      largeComputer.setRecognition(2, 0, 0.5);
      
      const stats = largeComputer.getMemoryStats();
      expect(stats.entries).toBe(3);
      expect(parseFloat(stats.sparsity)).toBeGreaterThan(99); // >99% sparse
    });
  });
  
  describe('Budget Constraint Validation (Axiom 1)', () => {
    it('should validate budget constraint for normalized rows', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      expect(computer.validateBudgetConstraint()).toBe(true);
    });
    
    it('should reject non-normalized rows', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.5); // Sum = 1.1 > 1.0
      
      expect(computer.validateBudgetConstraint()).toBe(false);
    });
    
    it('should accept empty rows (sum = 0)', () => {
      computer.setRecognition(1, 0, 0.5);
      computer.setRecognition(1, 2, 0.5);
      // Row 0 is empty - this is valid (participant hasn't allocated yet)
      
      expect(computer.validateBudgetConstraint()).toBe(true);
    });
  });
  
  describe('Recognition-Shares (RS)', () => {
    it('should compute RS correctly for normalized matrix', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const result = computer.computeRS();
      
      // RS should equal R when already normalized
      expect(result.get(0, 1)).toBeCloseTo(0.6);
      expect(result.get(0, 2)).toBeCloseTo(0.4);
      expect(result.get(1, 0)).toBeCloseTo(0.3);
      expect(result.get(1, 2)).toBeCloseTo(0.7);
    });
    
    it('should normalize rows that sum to non-1 values', () => {
      computer.setRecognition(0, 1, 60);
      computer.setRecognition(0, 2, 40); // Sum = 100
      
      const result = computer.computeRS();
      
      expect(result.get(0, 1)).toBeCloseTo(0.6); // 60/100
      expect(result.get(0, 2)).toBeCloseTo(0.4); // 40/100
    });
    
    it('should verify row normalization', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const result = computer.computeRS();
      
      expect(result.verifyRowNormalization()).toBe(true);
    });
  });
  
  describe('Mutual-Recognition (MR) - Axiom 2', () => {
    it('should compute MR using min(R[i,j], R[j,i])', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const result = computer.computeMR();
      
      // MR[0,1] = min(0.6, 0.3) = 0.3
      expect(result.get(0, 1)).toBeCloseTo(0.3);
      // MR[0,2] = min(0.4, 0.5) = 0.4
      expect(result.get(0, 2)).toBeCloseTo(0.4);
      // MR[1,2] = min(0.7, 0.5) = 0.5
      expect(result.get(1, 2)).toBeCloseTo(0.5);
    });
    
    it('should be symmetric: MR[i,j] = MR[j,i]', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const result = computer.computeMR();
      
      expect(result.verifySymmetry()).toBe(true);
      
      // Explicit checks
      expect(result.get(0, 1)).toBeCloseTo(result.get(1, 0));
      expect(result.get(0, 2)).toBeCloseTo(result.get(2, 0));
      expect(result.get(1, 2)).toBeCloseTo(result.get(2, 1));
    });
    
    it('should handle zero recognition (no relationship)', () => {
      computer.setRecognition(0, 1, 0.0);
      computer.setRecognition(1, 0, 1.0);
      
      const result = computer.computeMR();
      
      // MR[0,1] = min(0.0, 1.0) = 0.0
      expect(result.get(0, 1)).toBe(0);
    });
  });
  
  describe('Total Mutual Recognition (t)', () => {
    it('should compute row sums of MR correctly', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const t = computer.computeTotalMR();
      
      // t[0] = MR[0,1] + MR[0,2] = 0.3 + 0.4 = 0.7
      expect(t[0]).toBeCloseTo(0.7);
      // t[1] = MR[1,0] + MR[1,2] = 0.3 + 0.5 = 0.8
      expect(t[1]).toBeCloseTo(0.8);
      // t[2] = MR[2,0] + MR[2,1] = 0.4 + 0.5 = 0.9
      expect(t[2]).toBeCloseTo(0.9);
    });
  });
  
  describe('Mutual-Recognition-Shares (MRS)', () => {
    it('should compute MRS by normalizing MR rows by total MR', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const result = computer.computeMRS();
      
      // MRS[0,1] = MR[0,1] / t[0] = 0.3 / 0.7 ≈ 0.429
      expect(result.get(0, 1)).toBeCloseTo(0.429, 2);
      // MRS[0,2] = MR[0,2] / t[0] = 0.4 / 0.7 ≈ 0.571
      expect(result.get(0, 2)).toBeCloseTo(0.571, 2);
      // MRS[1,0] = MR[1,0] / t[1] = 0.3 / 0.8 = 0.375
      expect(result.get(1, 0)).toBeCloseTo(0.375, 2);
    });
    
    it('should have rows that sum to 1', () => {
      computer.setRecognition(0, 1, 0.6);
      computer.setRecognition(0, 2, 0.4);
      computer.setRecognition(1, 0, 0.3);
      computer.setRecognition(1, 2, 0.7);
      computer.setRecognition(2, 0, 0.5);
      computer.setRecognition(2, 1, 0.5);
      
      const result = computer.computeMRS();
      
      expect(result.verifyRowNormalization()).toBe(true);
    });
  });
});

describe('MatrixComputer - Collective Operations', () => {
  let computer: MatrixComputer;
  
  beforeEach(() => {
    computer = new MatrixComputer(3);
    // Set up standard test matrix
    computer.setRecognition(0, 1, 0.6);
    computer.setRecognition(0, 2, 0.4);
    computer.setRecognition(1, 0, 0.3);
    computer.setRecognition(1, 2, 0.7);
    computer.setRecognition(2, 0, 0.5);
    computer.setRecognition(2, 1, 0.5);
  });
  
  describe('SCMRS (Weighted)', () => {
    it('should compute collective shares correctly', () => {
      const collective = [0, 1, 2];
      const scmrs = computer.computeSCMRS_weighted(collective);
      
      // Expected: [0.292, 0.333, 0.375]
      expect(scmrs[0]).toBeCloseTo(0.292, 2);
      expect(scmrs[1]).toBeCloseTo(0.333, 2);
      expect(scmrs[2]).toBeCloseTo(0.375, 2);
    });
    
    it('should have shares that sum to 1', () => {
      const collective = [0, 1, 2];
      const scmrs = computer.computeSCMRS_weighted(collective);
      
      const sum = scmrs.reduce((a, b) => a + b, 0);
      expect(sum).toBeCloseTo(1.0, 5);
    });
    
    it('should handle subset collectives', () => {
      const collective = [0, 1]; // Only participants 0 and 1
      const scmrs = computer.computeSCMRS_weighted(collective);
      
      // Only members should have non-zero shares
      expect(scmrs[0]).toBeGreaterThan(0);
      expect(scmrs[1]).toBeGreaterThan(0);
      expect(scmrs[2]).toBe(0); // Non-member
    });
    
    it('should return zeros for empty collective', () => {
      const collective: number[] = [];
      const scmrs = computer.computeSCMRS_weighted(collective);
      
      expect(scmrs.every(s => s === 0)).toBe(true);
    });
  });
  
  describe('SCRMRS (Equal Voice)', () => {
    it('should give equal weight to each member vote', () => {
      const collective = [0, 1, 2];
      const scrmrs = computer.computeSCRMRS_equal(collective);
      
      // Each member's vote is weighted equally (1/3 each)
      const sum = scrmrs.reduce((a, b) => a + b, 0);
      expect(sum).toBeCloseTo(1.0, 5);
    });
  });
  
  describe('Mutual Recognition Density (MRD)', () => {
    it('should compute MRD correctly', () => {
      const collective = [0, 1, 2];
      const mrd = computer.computeMRD(collective, 0);
      
      // Expected: 0.875
      expect(mrd).toBeCloseTo(0.875, 2);
    });
    
    it('should compute MRD for all participants', () => {
      const collective = [0, 1, 2];
      const mrdValues = computer.computeAllMRD(collective);
      
      // Expected: [0.875, 1.0, 1.125]
      expect(mrdValues[0]).toBeCloseTo(0.875, 2);
      expect(mrdValues[1]).toBeCloseTo(1.0, 2);
      expect(mrdValues[2]).toBeCloseTo(1.125, 2);
    });
    
    it('should determine membership based on threshold', () => {
      const collective = [0, 1, 2];
      const members = computer.determineMembership(collective, 0.5, 'collective');
      
      // All should be above 0.5 threshold
      expect(members).toHaveLength(3);
      expect(members).toContain(0);
      expect(members).toContain(1);
      expect(members).toContain(2);
    });
    
    it('should filter members below threshold', () => {
      const collective = [0, 1, 2];
      const members = computer.determineMembership(collective, 1.1, 'collective');
      
      // Only participant 2 has MRD > 1.1
      expect(members).toHaveLength(1);
      expect(members).toContain(2);
    });
    
    it('should handle first member (empty collective)', () => {
      const collective: number[] = [];
      const mrd = computer.computeMRD(collective, 0);
      
      // First member has no one to compare to
      expect(mrd).toBe(0);
    });
  });
});

// ============================================================================
// SLOT MATCHING TESTS
// ============================================================================

describe('TimeMatching', () => {
  describe('Timezone Conversion', () => {
    it('should keep UTC times unchanged', () => {
      const result = TimeMatching.convertTimeToUTC('14:00', '2024-01-01', 'UTC');
      expect(result).toBe('14:00');
    });
    
    it('should convert times between timezones', () => {
      // This is timezone-dependent, so we test the mechanism
      const result = TimeMatching.convertTimeToUTC('14:00', '2024-01-01', 'America/New_York');
      expect(result).toMatch(/^\d{2}:\d{2}$/); // Valid time format
    });
    
    it('should handle invalid timezone gracefully', () => {
      const result = TimeMatching.convertTimeToUTC('14:00', '2024-01-01', 'Invalid/Timezone');
      expect(result).toBe('14:00'); // Falls back to input
    });
  });
  
  describe('Time Range Overlap', () => {
    it('should detect overlapping ranges', () => {
      const range1 = { start_time: '09:00', end_time: '12:00' };
      const range2 = { start_time: '10:00', end_time: '13:00' };
      
      expect(TimeMatching.timeRangesOverlap(range1, range2)).toBe(true);
    });
    
    it('should detect non-overlapping ranges', () => {
      const range1 = { start_time: '09:00', end_time: '12:00' };
      const range2 = { start_time: '13:00', end_time: '15:00' };
      
      expect(TimeMatching.timeRangesOverlap(range1, range2)).toBe(false);
    });
    
    it('should handle adjacent ranges (no overlap)', () => {
      const range1 = { start_time: '09:00', end_time: '12:00' };
      const range2 = { start_time: '12:00', end_time: '15:00' };
      
      expect(TimeMatching.timeRangesOverlap(range1, range2)).toBe(false);
    });
    
    it('should handle contained ranges', () => {
      const range1 = { start_time: '09:00', end_time: '15:00' };
      const range2 = { start_time: '10:00', end_time: '12:00' };
      
      expect(TimeMatching.timeRangesOverlap(range1, range2)).toBe(true);
    });
  });
  
  describe('Availability Window Overlap', () => {
    it('should match when both have no constraints', () => {
      const result = TimeMatching.availabilityWindowsOverlap(undefined, undefined);
      expect(result).toBe(true);
    });
    
    it('should not match when only one has constraints', () => {
      const window1 = { time_ranges: [{ start_time: '09:00', end_time: '12:00' }] };
      const result = TimeMatching.availabilityWindowsOverlap(window1, undefined);
      expect(result).toBe(false);
    });
    
    it('should match overlapping time ranges', () => {
      const window1 = { time_ranges: [{ start_time: '09:00', end_time: '12:00' }] };
      const window2 = { time_ranges: [{ start_time: '10:00', end_time: '13:00' }] };
      
      const result = TimeMatching.availabilityWindowsOverlap(window1, window2);
      expect(result).toBe(true);
    });
    
    it('should match overlapping days and times', () => {
      const window1 = {
        day_schedules: [{
          days: ['monday' as const, 'tuesday' as const],
          time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
        }]
      };
      const window2 = {
        day_schedules: [{
          days: ['tuesday' as const, 'wednesday' as const],
          time_ranges: [{ start_time: '10:00', end_time: '13:00' }]
        }]
      };
      
      const result = TimeMatching.availabilityWindowsOverlap(window1, window2);
      expect(result).toBe(true);
    });
    
    it('should not match non-overlapping days', () => {
      const window1 = {
        day_schedules: [{
          days: ['monday' as const],
          time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
        }]
      };
      const window2 = {
        day_schedules: [{
          days: ['wednesday' as const],
          time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
        }]
      };
      
      const result = TimeMatching.availabilityWindowsOverlap(window1, window2);
      expect(result).toBe(false);
    });
  });
});

describe('LocationMatching', () => {
  describe('Distance Calculation', () => {
    it('should calculate distance between coordinates', () => {
      // New York to Los Angeles (roughly 3940 km)
      const distance = LocationMatching.calculateDistance(
        40.7128, -74.0060,  // NYC
        34.0522, -118.2437  // LA
      );
      
      expect(distance).toBeGreaterThan(3900);
      expect(distance).toBeLessThan(4000);
    });
    
    it('should return 0 for same coordinates', () => {
      const distance = LocationMatching.calculateDistance(
        40.7128, -74.0060,
        40.7128, -74.0060
      );
      
      expect(distance).toBeCloseTo(0, 1);
    });
  });
  
  describe('Location Compatibility', () => {
    it('should match when both have no location', () => {
      const result = LocationMatching.locationsCompatible(undefined, undefined);
      expect(result).toBe(true);
    });
    
    it('should not match when only one has location', () => {
      const loc1 = { type: 'physical' as const, city: 'New York' };
      const result = LocationMatching.locationsCompatible(loc1, undefined);
      expect(result).toBe(false);
    });
    
    it('should match both online', () => {
      const loc1 = { type: 'online' as const };
      const loc2 = { type: 'online' as const };
      
      const result = LocationMatching.locationsCompatible(loc1, loc2);
      expect(result).toBe(true);
    });
    
    it('should match online with physical', () => {
      const loc1 = { type: 'online' as const };
      const loc2 = { type: 'physical' as const, city: 'New York' };
      
      const result = LocationMatching.locationsCompatible(loc1, loc2);
      expect(result).toBe(true);
    });
    
    it('should match same city', () => {
      const loc1 = { type: 'physical' as const, city: 'New York' };
      const loc2 = { type: 'physical' as const, city: 'New York' };
      
      const result = LocationMatching.locationsCompatible(loc1, loc2);
      expect(result).toBe(true);
    });
    
    it('should not match different countries', () => {
      const loc1 = { type: 'physical' as const, country: 'USA' };
      const loc2 = { type: 'physical' as const, country: 'Canada' };
      
      const result = LocationMatching.locationsCompatible(loc1, loc2);
      expect(result).toBe(false);
    });
    
    it('should match nearby coordinates within max distance', () => {
      const loc1 = {
        type: 'physical' as const,
        latitude: 40.7128,
        longitude: -74.0060
      };
      const loc2 = {
        type: 'physical' as const,
        latitude: 40.7589, // ~5 km away
        longitude: -73.9851
      };
      
      const result = LocationMatching.locationsCompatible(loc1, loc2, 10);
      expect(result).toBe(true);
    });
    
    it('should not match distant coordinates beyond max distance', () => {
      const loc1 = {
        type: 'physical' as const,
        latitude: 40.7128,
        longitude: -74.0060
      };
      const loc2 = {
        type: 'physical' as const,
        latitude: 34.0522, // ~3940 km away
        longitude: -118.2437
      };
      
      const result = LocationMatching.locationsCompatible(loc1, loc2, 50);
      expect(result).toBe(false);
    });
  });
});

describe('SlotMatching', () => {
  const createNeedSlot = (overrides: Partial<NeedSlot> = {}): NeedSlot => ({
    id: 'need-1',
    participantId: 'alice@example.com',
    need_type_id: 'tutoring',
    quantity: 5,
    name: 'Math Tutoring',
    time_zone: 'UTC',
    ...overrides
  });
  
  const createAvailSlot = (overrides: Partial<AvailabilitySlot> = {}): AvailabilitySlot => ({
    id: 'avail-1',
    participantId: 'bob@example.com',
    need_type_id: 'tutoring',
    quantity: 10,
    name: 'Math Tutoring Available',
    time_zone: 'UTC',
    ...overrides
  });
  
  it('should match compatible slots', () => {
    const need = createNeedSlot();
    const avail = createAvailSlot();
    
    expect(SlotMatching.slotsCompatible(need, avail)).toBe(true);
  });
  
  it('should not match different need types', () => {
    const need = createNeedSlot({ need_type_id: 'tutoring' });
    const avail = createAvailSlot({ need_type_id: 'mentoring' });
    
    expect(SlotMatching.slotsCompatible(need, avail)).toBe(false);
  });
  
  it('should not match incompatible locations', () => {
    const need = createNeedSlot({
      location: { type: 'physical', country: 'USA' }
    });
    const avail = createAvailSlot({
      location: { type: 'physical', country: 'Canada' }
    });
    
    expect(SlotMatching.slotsCompatible(need, avail)).toBe(false);
  });
  
  it('should match compatible time windows', () => {
    const need = createNeedSlot({
      availability_window: {
        time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
      }
    });
    const avail = createAvailSlot({
      availability_window: {
        time_ranges: [{ start_time: '10:00', end_time: '13:00' }]
      }
    });
    
    expect(SlotMatching.slotsCompatible(need, avail)).toBe(true);
  });
  
  it('should filter compatible providers', () => {
    const need = createNeedSlot({ need_type_id: 'tutoring' });
    const availSlots = [
      createAvailSlot({ id: 'avail-1', need_type_id: 'tutoring' }),
      createAvailSlot({ id: 'avail-2', need_type_id: 'mentoring' }),
      createAvailSlot({ id: 'avail-3', need_type_id: 'tutoring' })
    ];
    
    const compatible = SlotMatching.getCompatibleProviders(need, availSlots);
    
    expect(compatible).toHaveLength(2);
    expect(compatible.map(s => s.id)).toContain('avail-1');
    expect(compatible.map(s => s.id)).toContain('avail-3');
  });
});

// ============================================================================
// ALLOCATION ENGINE TESTS
// ============================================================================

describe('DampeningSystem', () => {
  it('should return 1.0 damping for no history', () => {
    const factor = DampeningSystem.calculateDampingFactor([]);
    expect(factor).toBe(1.0);
  });
  
  it('should reduce damping with over-allocation history', () => {
    const history = [0.2, 0.3, 0.25]; // Consistent overshooting
    const factor = DampeningSystem.calculateDampingFactor(history);
    
    expect(factor).toBeLessThan(1.0);
    expect(factor).toBeGreaterThanOrEqual(0.1); // MIN_DAMPING
  });
  
  it('should not reduce damping below minimum', () => {
    const history = [1.0, 1.0, 1.0, 1.0, 1.0]; // Severe overshooting
    const factor = DampeningSystem.calculateDampingFactor(history);
    
    expect(factor).toBeGreaterThanOrEqual(0.1);
  });
  
  it('should update damping state correctly', () => {
    const state = {
      overAllocationHistory: [0.1],
      dampingFactor: 0.95
    };
    
    const updated = DampeningSystem.updateDampingState(state, 120, 100);
    
    expect(updated.overAllocationHistory).toHaveLength(2);
    expect(updated.overAllocationHistory[1]).toBeCloseTo(0.2); // 20% overshoot
    expect(updated.dampingFactor).toBeLessThan(0.95);
  });
  
  it('should apply damping to allocations', () => {
    const raw = 100;
    const damping = 0.8;
    
    const damped = DampeningSystem.applyDamping(raw, damping);
    
    expect(damped).toBe(80);
  });
});

describe('DivisibilityConstraints', () => {
  it('should accept allocations meeting minimum percentage', () => {
    const constraints = { min_allocation_percentage: 0.1 };
    
    const result = DivisibilityConstraints.satisfiesConstraints(15, 100, constraints);
    
    expect(result).toBe(true);
  });
  
  it('should reject allocations below minimum percentage', () => {
    const constraints = { min_allocation_percentage: 0.1 };
    
    const result = DivisibilityConstraints.satisfiesConstraints(5, 100, constraints);
    
    expect(result).toBe(false);
  });
  
  it('should calculate minimum allocation', () => {
    const constraints = { min_allocation_percentage: 0.1 };
    
    const min = DivisibilityConstraints.getMinimumAllocation(100, constraints);
    
    expect(min).toBe(10);
  });
  
  it('should round to natural units', () => {
    const constraints = { max_natural_div: 4 }; // Can divide into 4 parts
    
    const rounded = DivisibilityConstraints.roundToNaturalUnit(23, 100, constraints);
    
    // Unit size = 100/4 = 25, so 23 rounds to 25
    expect(rounded).toBe(25);
  });
  
  it('should not round without constraints', () => {
    const rounded = DivisibilityConstraints.roundToNaturalUnit(23.7, 100, undefined);
    
    expect(rounded).toBe(23.7);
  });
});

describe('LargestRemainderMethod', () => {
  it('should allocate integer quantities fairly', () => {
    const shares = {
      'provider-1': 0.333,
      'provider-2': 0.333,
      'provider-3': 0.334
    };
    
    const result = LargestRemainderMethod.allocate(shares, 10);
    
    // Should sum to exactly 10
    const sum = Object.values(result).reduce((a, b) => a + b, 0);
    expect(sum).toBe(10);
    
    // Each should get close to their share (3-4 units)
    expect(result['provider-1']).toBeGreaterThanOrEqual(3);
    expect(result['provider-1']).toBeLessThanOrEqual(4);
  });
  
  it('should handle remainders correctly', () => {
    const shares = {
      'provider-1': 0.4,  // 4.0 → 4
      'provider-2': 0.35, // 3.5 → 3 + 1 (largest remainder)
      'provider-3': 0.25  // 2.5 → 2
    };
    
    const result = LargestRemainderMethod.allocate(shares, 10);
    
    expect(result['provider-1']).toBe(4);
    expect(result['provider-2']).toBe(4); // Gets the remainder
    expect(result['provider-3']).toBe(2);
  });
  
  it('should allocate all units', () => {
    const shares = {
      'a': 0.7,
      'b': 0.3
    };
    
    const result = LargestRemainderMethod.allocate(shares, 100);
    
    expect(result['a'] + result['b']).toBe(100);
  });
});

describe('ConvergenceTracker', () => {
  const createNeedSlot = (quantity: number): NeedSlot => ({
    id: `need-${Math.random()}`,
    participantId: 'alice@example.com',
    need_type_id: 'tutoring',
    quantity,
    name: 'Test Need',
    time_zone: 'UTC'
  });
  
  const createAvailSlot = (quantity: number): AvailabilitySlot => ({
    id: `avail-${Math.random()}`,
    participantId: 'bob@example.com',
    need_type_id: 'tutoring',
    quantity,
    name: 'Test Availability',
    time_zone: 'UTC'
  });
  
  it('should calculate basic metrics', () => {
    const needs = [createNeedSlot(100), createNeedSlot(50)];
    const capacity = [createAvailSlot(200)];
    const allocations = [
      {
        needSlotId: needs[0].id,
        availabilitySlotId: capacity[0].id,
        providerId: 'bob@example.com',
        recipientId: 'alice@example.com',
        allocatedQuantity: 100,
        timestamp: Date.now()
      }
    ];
    
    const metrics = ConvergenceTracker.calculateMetrics(needs, capacity, allocations);
    
    expect(metrics.totalNeed).toBe(150);
    expect(metrics.totalCapacity).toBe(200);
    expect(metrics.totalAllocated).toBe(100);
    expect(metrics.satisfactionRate).toBeCloseTo(100 / 150);
    expect(metrics.allocationEfficiency).toBeCloseTo(100 / 200);
  });
  
  it('should detect convergence', () => {
    const previous: ConvergenceMetrics = {
      totalNeed: 100,
      totalCapacity: 100,
      totalAllocated: 95,
      satisfactionRate: 0.95,
      allocationEfficiency: 0.95
    };
    
    const current: ConvergenceMetrics = {
      totalNeed: 100,
      totalCapacity: 100,
      totalAllocated: 96,
      satisfactionRate: 0.96,
      allocationEfficiency: 0.96
    };
    
    const converged = ConvergenceTracker.hasConverged(current, previous, 0.02);
    
    expect(converged).toBe(true); // Change is 0.01, below threshold of 0.02
  });
  
  it('should detect non-convergence', () => {
    const previous: ConvergenceMetrics = {
      totalNeed: 100,
      totalCapacity: 100,
      totalAllocated: 80,
      satisfactionRate: 0.80,
      allocationEfficiency: 0.80
    };
    
    const current: ConvergenceMetrics = {
      totalNeed: 100,
      totalCapacity: 100,
      totalAllocated: 95,
      satisfactionRate: 0.95,
      allocationEfficiency: 0.95
    };
    
    const converged = ConvergenceTracker.hasConverged(current, previous, 0.02);
    
    expect(converged).toBe(false); // Change is 0.15, above threshold
  });
});

describe('SpaceTimeIndex', () => {
  let index: SpaceTimeIndex;
  
  beforeEach(() => {
    index = new SpaceTimeIndex();
  });
  
  it('should index slots by type', () => {
    const slot: NeedSlot = {
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'tutoring',
      quantity: 5,
      name: 'Test',
      time_zone: 'UTC'
    };
    
    index.addSlot(slot);
    
    const matches = index.findMatching(slot);
    expect(matches.has('alice@example.com')).toBe(true);
  });
  
  it('should filter by type', () => {
    const slot1: NeedSlot = {
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'tutoring',
      quantity: 5,
      name: 'Test',
      time_zone: 'UTC'
    };
    
    const slot2: NeedSlot = {
      id: 'need-2',
      participantId: 'bob@example.com',
      need_type_id: 'mentoring',
      quantity: 5,
      name: 'Test',
      time_zone: 'UTC'
    };
    
    index.addSlot(slot1);
    index.addSlot(slot2);
    
    const matches = index.findMatching(slot1);
    expect(matches.has('alice@example.com')).toBe(true);
    expect(matches.has('bob@example.com')).toBe(false);
  });
  
  it('should index by location', () => {
    const slot: NeedSlot = {
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'tutoring',
      quantity: 5,
      name: 'Test',
      time_zone: 'UTC',
      location: { type: 'physical', city: 'New York' }
    };
    
    index.addSlot(slot);
    
    const matches = index.findMatching(slot);
    expect(matches.size).toBeGreaterThan(0);
  });
  
  it('should clear index', () => {
    const slot: NeedSlot = {
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'tutoring',
      quantity: 5,
      name: 'Test',
      time_zone: 'UTC'
    };
    
    index.addSlot(slot);
    index.clear();
    
    const matches = index.findMatching(slot);
    expect(matches.size).toBe(0);
  });
});

// ============================================================================
// INTEGRATION TESTS
// ============================================================================

describe('End-to-End Integration', () => {
  it('should complete full allocation workflow', () => {
    // 1. Setup matrix
    const computer = new MatrixComputer(3);
    computer.setRecognition(0, 1, 0.6);
    computer.setRecognition(0, 2, 0.4);
    computer.setRecognition(1, 0, 0.3);
    computer.setRecognition(1, 2, 0.7);
    computer.setRecognition(2, 0, 0.5);
    computer.setRecognition(2, 1, 0.5);
    
    // 2. Compute MRS
    const mrs = computer.computeMRS();
    expect(mrs.verifyRowNormalization()).toBe(true);
    
    // 3. Create slots
    const needSlots: NeedSlot[] = [{
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'tutoring',
      quantity: 10,
      name: 'Math Tutoring Needed',
      time_zone: 'UTC'
    }];
    
    const availSlots: AvailabilitySlot[] = [{
      id: 'avail-1',
      participantId: 'bob@example.com',
      need_type_id: 'tutoring',
      quantity: 20,
      name: 'Math Tutoring Available',
      time_zone: 'UTC'
    }];
    
    // 4. Setup allocation engine
    const engine = new AllocationEngine(computer);
    
    // 5. Run allocation
    const result = engine.allocateSlots(needSlots, availSlots);
    
    // 6. Verify results
    expect(result.allocations.length).toBeGreaterThan(0);
    expect(result.metrics.totalNeed).toBe(10);
    expect(result.metrics.totalCapacity).toBe(20);
    expect(result.metrics.satisfactionRate).toBeGreaterThan(0);
  });
  
  it('should handle multi-provider allocation', () => {
    // Setup 3 participants
    const computer = new MatrixComputer(3);
    computer.setRecognition(0, 1, 0.5);
    computer.setRecognition(0, 2, 0.5);
    computer.setRecognition(1, 0, 0.5);
    computer.setRecognition(1, 2, 0.5);
    computer.setRecognition(2, 0, 0.5);
    computer.setRecognition(2, 1, 0.5);
    
    // Alice needs 100 units
    const needSlots: NeedSlot[] = [{
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'hours',
      quantity: 100,
      name: 'Time Needed',
      time_zone: 'UTC'
    }];
    
    // Bob and Carol each have 60 units available
    const availSlots: AvailabilitySlot[] = [
      {
        id: 'avail-1',
        participantId: 'bob@example.com',
        need_type_id: 'hours',
        quantity: 60,
        name: 'Time Available',
        time_zone: 'UTC'
      },
      {
        id: 'avail-2',
        participantId: 'carol@example.com',
        need_type_id: 'hours',
        quantity: 60,
        name: 'Time Available',
        time_zone: 'UTC'
      }
    ];
    
    const engine = new AllocationEngine(computer);
    const result = engine.allocateSlots(needSlots, availSlots);
    
    // Should allocate from both providers
    expect(result.allocations.length).toBeGreaterThanOrEqual(1);
    
    // Should satisfy the need (120 available, 100 needed)
    expect(result.metrics.satisfactionRate).toBeGreaterThan(0.9);
  });
});

// ============================================================================
// PERFORMANCE TESTS
// ============================================================================

describe('Performance', () => {
  it('should handle large sparse matrices efficiently', () => {
    const n = 1000;
    const computer = new MatrixComputer(n);
    
    // Add only 10 connections per participant (1% density)
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < 10; j++) {
        const target = (i + j + 1) % n;
        computer.setRecognition(i, target, 0.1);
      }
    }
    
    const start = performance.now();
    const mrs = computer.computeMRS();
    const duration = performance.now() - start;
    
    // Should complete in reasonable time (< 100ms)
    expect(duration).toBeLessThan(100);
    
    // Should use sparse storage
    const stats = computer.getMemoryStats();
    expect(stats.entries).toBe(10000); // n * 10
    expect(parseFloat(stats.sparsity)).toBeGreaterThan(98);
  });
  
  it('should index slots for O(k) lookups', () => {
    const index = new SpaceTimeIndex();
    
    // Add 1000 slots
    for (let i = 0; i < 1000; i++) {
      const slot: NeedSlot = {
        id: `slot-${i}`,
        participantId: `user-${i}@example.com`,
        need_type_id: i % 10 === 0 ? 'tutoring' : 'other',
        quantity: 5,
        name: 'Test',
        time_zone: 'UTC'
      };
      index.addSlot(slot);
    }
    
    // Search for tutoring (should find ~100 matches, not scan all 1000)
    const searchSlot: NeedSlot = {
      id: 'search',
      participantId: 'searcher@example.com',
      need_type_id: 'tutoring',
      quantity: 5,
      name: 'Test',
      time_zone: 'UTC'
    };
    
    const start = performance.now();
    const matches = index.findMatching(searchSlot);
    const duration = performance.now() - start;
    
    // Should be very fast (< 5ms)
    expect(duration).toBeLessThan(5);
    
    // Should find roughly 10% of slots (100 out of 1000)
    expect(matches.size).toBeGreaterThan(90);
    expect(matches.size).toBeLessThan(110);
  });
});

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

describe('Error Handling', () => {
  it('should handle division by zero in MRS computation', () => {
    const computer = new MatrixComputer(2);
    // Don't set any recognition - all zeros
    
    expect(() => {
      const mrs = computer.computeMRS();
      // Should not throw, should return all zeros
      expect(mrs.get(0, 1)).toBe(0);
    }).not.toThrow();
  });
  
  it('should handle empty collectives', () => {
    const computer = new MatrixComputer(3);
    computer.setRecognition(0, 1, 0.5);
    computer.setRecognition(0, 2, 0.5);
    
    const scmrs = computer.computeSCMRS_weighted([]);
    
    expect(scmrs.every(v => v === 0)).toBe(true);
  });
  
  it('should handle no compatible slots gracefully', () => {
    const computer = new MatrixComputer(2);
    const engine = new AllocationEngine(computer);
    
    const needs: NeedSlot[] = [{
      id: 'need-1',
      participantId: 'alice@example.com',
      need_type_id: 'tutoring',
      quantity: 10,
      name: 'Test',
      time_zone: 'UTC'
    }];
    
    const avails: AvailabilitySlot[] = [{
      id: 'avail-1',
      participantId: 'bob@example.com',
      need_type_id: 'DIFFERENT-TYPE', // No match!
      quantity: 10,
      name: 'Test',
      time_zone: 'UTC'
    }];
    
    const result = engine.allocateSlots(needs, avails);
    
    expect(result.allocations).toHaveLength(0);
    expect(result.metrics.satisfactionRate).toBe(0);
  });
});

