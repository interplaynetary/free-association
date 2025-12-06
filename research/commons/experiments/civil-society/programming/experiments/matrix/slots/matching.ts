/**
 * Slot Matching System
 * 
 * Multi-dimensional compatibility checking:
 * - Type matching
 * - Timezone-aware time matching
 * - Location matching (Haversine distance)
 * - Compliance filter evaluation
 */

import type { NeedSlot, AvailabilitySlot, TimeRange, AvailabilityWindow, Location } from './schemas.js';

// ═══════════════════════════════════════════════════════════════════
// TIME MATCHING
// ═══════════════════════════════════════════════════════════════════

export class TimeMatching {
  /**
   * Convert HH:MM time from timezone to UTC
   */
  static convertTimeToUTC(
    timeStr: string,
    dateStr: string,
    timezone: string = 'UTC'
  ): string {
    if (timezone === 'UTC' || timezone === 'Etc/UTC') {
      return timeStr;
    }
    
    try {
      const [hours, minutes] = timeStr.split(':').map(Number);
      const [year, month, day] = dateStr.split('-').map(Number);
      
      const refUTC = Date.UTC(year, month - 1, day, 12, 0, 0);
      const refDate = new Date(refUTC);
      
      const formatter = new Intl.DateTimeFormat('en-US', {
        timeZone: timezone,
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        hour12: false
      });
      
      const parts = formatter.formatToParts(refDate);
      const tzHour = parseInt(parts.find(p => p.type === 'hour')!.value);
      const tzDay = parseInt(parts.find(p => p.type === 'day')!.value);
      
      const offsetHours = 12 - tzHour;
      const dayShift = tzDay - day;
      
      let utcHours = hours + offsetHours - (dayShift * 24);
      let utcMinutes = minutes;
      
      while (utcHours < 0) utcHours += 24;
      while (utcHours >= 24) utcHours -= 24;
      
      return `${String(utcHours).padStart(2, '0')}:${String(utcMinutes).padStart(2, '0')}`;
    } catch (error) {
      console.warn('Timezone conversion failed:', error);
      return timeStr;
    }
  }
  
  /**
   * Check if two time ranges overlap (both in UTC)
   */
  static timeRangesOverlap(range1: TimeRange, range2: TimeRange): boolean {
    const start1 = range1.start_time;
    const end1 = range1.end_time;
    const start2 = range2.start_time;
    const end2 = range2.end_time;
    
    if (end1 <= start2 || end2 <= start1) {
      return false;
    }
    
    return true;
  }
  
  /**
   * Check if availability windows overlap
   */
  static availabilityWindowsOverlap(
    window1?: AvailabilityWindow,
    window2?: AvailabilityWindow,
    tz1: string = 'UTC',
    tz2: string = 'UTC',
    referenceDate: string = '2024-01-01'
  ): boolean {
    if (!window1 && !window2) return true;
    if (!window1 || !window2) return false;
    
    // Check time ranges
    if (window1.time_ranges && window2.time_ranges) {
      for (const tr1 of window1.time_ranges) {
        const utc_tr1 = {
          start_time: this.convertTimeToUTC(tr1.start_time, referenceDate, tz1),
          end_time: this.convertTimeToUTC(tr1.end_time, referenceDate, tz1)
        };
        
        for (const tr2 of window2.time_ranges) {
          const utc_tr2 = {
            start_time: this.convertTimeToUTC(tr2.start_time, referenceDate, tz2),
            end_time: this.convertTimeToUTC(tr2.end_time, referenceDate, tz2)
          };
          
          if (this.timeRangesOverlap(utc_tr1, utc_tr2)) {
            return true;
          }
        }
      }
    }
    
    // Check day schedules
    if (window1.day_schedules && window2.day_schedules) {
      for (const ds1 of window1.day_schedules) {
        for (const ds2 of window2.day_schedules) {
          const daysOverlap = ds1.days.some(d => ds2.days.includes(d));
          if (!daysOverlap) continue;
          
          for (const tr1 of ds1.time_ranges) {
            const utc_tr1 = {
              start_time: this.convertTimeToUTC(tr1.start_time, referenceDate, tz1),
              end_time: this.convertTimeToUTC(tr1.end_time, referenceDate, tz1)
            };
            
            for (const tr2 of ds2.time_ranges) {
              const utc_tr2 = {
                start_time: this.convertTimeToUTC(tr2.start_time, referenceDate, tz2),
                end_time: this.convertTimeToUTC(tr2.end_time, referenceDate, tz2)
              };
              
              if (this.timeRangesOverlap(utc_tr1, utc_tr2)) {
                return true;
              }
            }
          }
        }
      }
    }
    
    return false;
  }
}

// ═══════════════════════════════════════════════════════════════════
// LOCATION MATCHING
// ═══════════════════════════════════════════════════════════════════

export class LocationMatching {
  /**
   * Calculate distance between two coordinates (Haversine formula)
   * Returns distance in kilometers
   */
  static calculateDistance(
    lat1: number, lon1: number,
    lat2: number, lon2: number
  ): number {
    const R = 6371; // Earth's radius in km
    const dLat = (lat2 - lat1) * Math.PI / 180;
    const dLon = (lon2 - lon1) * Math.PI / 180;
    const a =
      Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
      Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return R * c;
  }
  
  /**
   * Check if two locations are compatible
   */
  static locationsCompatible(
    loc1?: Location,
    loc2?: Location,
    maxDistanceKm: number = 50
  ): boolean {
    if (!loc1 && !loc2) return true;
    if (!loc1 || !loc2) return false;
    
    // Both online = compatible
    if (loc1.type === 'online' && loc2.type === 'online') return true;
    if (loc1.type === 'online' || loc2.type === 'online') return true;
    
    // Check city/country match
    if (loc1.city && loc2.city && loc1.city !== loc2.city) {
      if (loc1.latitude && loc1.longitude &&
          loc2.latitude && loc2.longitude) {
        const distance = this.calculateDistance(
          loc1.latitude, loc1.longitude,
          loc2.latitude, loc2.longitude
        );
        return distance <= maxDistanceKm;
      }
      return false;
    }
    
    if (loc1.country && loc2.country && loc1.country !== loc2.country) {
      return false;
    }
    
    // Check coordinate distance if available
    if (loc1.latitude && loc1.longitude &&
        loc2.latitude && loc2.longitude) {
      const distance = this.calculateDistance(
        loc1.latitude, loc1.longitude,
        loc2.latitude, loc2.longitude
      );
      return distance <= maxDistanceKm;
    }
    
    return true;
  }
}

// ═══════════════════════════════════════════════════════════════════
// COMPLIANCE FILTERS
// ═══════════════════════════════════════════════════════════════════

export class ComplianceFilters {
  /**
   * Evaluate a JsonLogic filter rule
   * (Simplified - use jsonlogic library in production)
   */
  static evaluate(rule: any, data: any): boolean {
    if (!rule) return true;
    return true; // Placeholder - implement JsonLogic
  }
}

// ═══════════════════════════════════════════════════════════════════
// SLOT COMPATIBILITY
// ═══════════════════════════════════════════════════════════════════

export class SlotMatching {
  /**
   * Check if a need slot and availability slot are compatible
   */
  static slotsCompatible(
    needSlot: NeedSlot,
    availSlot: AvailabilitySlot,
    maxDistanceKm: number = 50
  ): boolean {
    // Type must match
    if (needSlot.need_type_id !== availSlot.need_type_id) {
      return false;
    }
    
    // Check location compatibility
    if (!LocationMatching.locationsCompatible(
      needSlot.location,
      availSlot.location,
      maxDistanceKm
    )) {
      return false;
    }
    
    // Check time compatibility
    const timeCompatible = TimeMatching.availabilityWindowsOverlap(
      needSlot.availability_window,
      availSlot.availability_window,
      needSlot.time_zone,
      availSlot.time_zone,
      needSlot.start_date || '2024-01-01'
    );
    
    if (!timeCompatible) {
      return false;
    }
    
    // Check compliance filter
    if (needSlot.filter_rule) {
      const providerData = { providerId: availSlot.participantId };
      if (!ComplianceFilters.evaluate(needSlot.filter_rule, providerData)) {
        return false;
      }
    }
    
    return true;
  }
  
  /**
   * Get compatible providers for a need slot
   */
  static getCompatibleProviders(
    needSlot: NeedSlot,
    availabilitySlots: AvailabilitySlot[]
  ): AvailabilitySlot[] {
    return availabilitySlots.filter(avail =>
      this.slotsCompatible(needSlot, avail)
    );
  }
}

