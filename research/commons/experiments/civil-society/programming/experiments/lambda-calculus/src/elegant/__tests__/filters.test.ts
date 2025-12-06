/**
 * Tests for Elegant Lambda Calculus: Filters
 */

import { describe, it, expect } from 'vitest';
import type { Entity, RecognitionMatrix } from '../../core/types';
import {
  attr,
  mrdFilter,
  timeFilter,
  idFilter,
  metadataFilter,
  nameFilter,
  topN,
  bottomN,
  percentile,
  andFilter,
  orFilter,
  notFilter,
  threshold,
  range,
  seqFilter,
} from '../filters';

describe('Elegant Filters', () => {
  // Test data
  const alice: Entity = { id: 'alice', name: 'Alice', metadata: { role: 'admin' } };
  const bob: Entity = { id: 'bob', name: 'Bob', metadata: { role: 'user' } };
  const charlie: Entity = { id: 'charlie', name: 'Charlie', metadata: { role: 'user' } };
  const dana: Entity = { id: 'dana', name: 'Dana', metadata: { role: 'moderator' } };
  
  const entities = new Set([alice, bob, charlie, dana]);
  
  const matrix: RecognitionMatrix = {
    matrix: {
      alice: { bob: 0.8, charlie: 0.6, dana: 0.9 },
      bob: { alice: 0.7, charlie: 0.5, dana: 0.4 },
      charlie: { alice: 0.6, bob: 0.5, dana: 0.7 },
      dana: { alice: 0.9, bob: 0.4, charlie: 0.7 },
    },
  };

  describe('Basic Filters', () => {
    it('attr: filters by predicate', () => {
      const adminFilter = attr<Entity>(e => e.metadata?.role === 'admin');
      const result = adminFilter(entities);
      
      expect(result.size).toBe(1);
      expect(result.has(alice)).toBe(true);
    });

    it('idFilter: filters by ID set', () => {
      const ids = new Set(['alice', 'charlie']);
      const filter = idFilter(ids);
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(alice)).toBe(true);
      expect(result.has(charlie)).toBe(true);
    });

    it('metadataFilter: filters by metadata key-value', () => {
      const filter = metadataFilter('role')('user');
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(bob)).toBe(true);
      expect(result.has(charlie)).toBe(true);
    });

    it('nameFilter: filters by name pattern', () => {
      const filter = nameFilter(/^[AB]/);
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(alice)).toBe(true);
      expect(result.has(bob)).toBe(true);
    });

    it('timeFilter: filters by timestamp', () => {
      const withTimestamps = new Set([
        { ...alice, lastActive: 100 },
        { ...bob, lastActive: 50 },
        { ...charlie, lastActive: 150 },
      ]);
      
      const filter = timeFilter(75);
      const result = filter(withTimestamps);
      
      expect(result.size).toBe(2);
    });
  });

  describe('Rank Filters', () => {
    it('topN: returns top N by score', () => {
      const scoreFn = (e: Entity) => 
        e.id === 'alice' ? 10 : e.id === 'bob' ? 8 : e.id === 'charlie' ? 6 : 4;
      
      const filter = topN(2)(scoreFn);
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(alice)).toBe(true);
      expect(result.has(bob)).toBe(true);
    });

    it('bottomN: returns bottom N by score', () => {
      const scoreFn = (e: Entity) => 
        e.id === 'alice' ? 10 : e.id === 'bob' ? 8 : e.id === 'charlie' ? 6 : 4;
      
      const filter = bottomN(2)(scoreFn);
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(dana)).toBe(true);
      expect(result.has(charlie)).toBe(true);
    });

    it('percentile: returns range by percentile', () => {
      const scoreFn = (e: Entity) => 
        e.id === 'alice' ? 10 : e.id === 'bob' ? 8 : e.id === 'charlie' ? 6 : 4;
      
      const filter = percentile(25)(75)(scoreFn);
      const result = filter(entities);
      
      expect(result.size).toBeGreaterThan(0);
    });
  });

  describe('Logical Combinators', () => {
    it('andFilter: intersection of two filters', () => {
      const filter1 = attr<Entity>(e => e.metadata?.role === 'user');
      const filter2 = nameFilter(/^[BC]/);
      
      const combined = andFilter(filter1)(filter2);
      const result = combined(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(bob)).toBe(true);
      expect(result.has(charlie)).toBe(true);
    });

    it('orFilter: union of two filters', () => {
      const filter1 = attr<Entity>(e => e.id === 'alice');
      const filter2 = attr<Entity>(e => e.id === 'dana');
      
      const combined = orFilter(filter1)(filter2);
      const result = combined(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(alice)).toBe(true);
      expect(result.has(dana)).toBe(true);
    });

    it('notFilter: complement of filter', () => {
      const filter = attr<Entity>(e => e.id === 'alice');
      const complement = notFilter(filter);
      const result = complement(entities);
      
      expect(result.size).toBe(3);
      expect(result.has(alice)).toBe(false);
      expect(result.has(bob)).toBe(true);
    });
  });

  describe('Threshold Filters', () => {
    it('threshold: filters by minimum score', () => {
      const scoreFn = (e: Entity) => 
        e.id === 'alice' ? 10 : e.id === 'bob' ? 8 : e.id === 'charlie' ? 6 : 4;
      
      const filter = threshold<Entity>(7)(scoreFn);
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(alice)).toBe(true);
      expect(result.has(bob)).toBe(true);
    });

    it('range: filters by score range', () => {
      const scoreFn = (e: Entity) => 
        e.id === 'alice' ? 10 : e.id === 'bob' ? 8 : e.id === 'charlie' ? 6 : 4;
      
      const filter = range<Entity>(5)(9)(scoreFn);
      const result = filter(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(bob)).toBe(true);
      expect(result.has(charlie)).toBe(true);
    });
  });

  describe('Filter Composition', () => {
    it('seqFilter: composes filters sequentially', () => {
      const filter1 = attr<Entity>(e => e.metadata?.role === 'user');
      const filter2 = nameFilter(/^B/);
      
      const combined = seqFilter(filter1)(filter2);
      const result = combined(entities);
      
      expect(result.size).toBe(1);
      expect(result.has(bob)).toBe(true);
    });

    it('multiple composition: chains multiple filters', () => {
      const adminOrModerator = orFilter(
        attr<Entity>(e => e.metadata?.role === 'admin')
      )(attr<Entity>(e => e.metadata?.role === 'moderator'));
      
      const result = adminOrModerator(entities);
      
      expect(result.size).toBe(2);
      expect(result.has(alice)).toBe(true);
      expect(result.has(dana)).toBe(true);
    });
  });
});

