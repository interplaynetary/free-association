# Complete Integration Example

This document shows a complete end-to-end integration of the collective recognition scheduler.

## Step 1: Environment Configuration

Create or update `.env.local`:

```bash
# Collective Recognition Scheduler Configuration

# Run membership computation weekly (7 days)
MEMBERSHIP_COMPUTATION_INTERVAL_MS=604800000

# Run allocation computation daily (1 day)
ALLOCATION_COMPUTATION_INTERVAL_MS=86400000

# MRD threshold: participants need 50% of average mutual recognition
MRD_THRESHOLD=0.5

# Minimum mutual recognition: 1% (filter out trivial recognitions)
MINIMUM_MUTUAL_RECOGNITION=1.0

# Enable both features
AUTO_UPDATE_MEMBERSHIP=true
AUTO_COMPUTE_ALLOCATIONS=true

# Wait 30 seconds after server startup before first computation
COMPUTATION_STARTUP_DELAY_MS=30000

# Enable detailed logging for development
COLLECTIVE_VERBOSE_LOGGING=true
```

## Step 2: Implement Callbacks

Create `src/lib/server/collective/callbacks.ts`:

```typescript
import type { ComputationCallbacks } from './scheduler';
import type { RecognitionData, BaseCapacity, BaseNeed } from '$lib/protocol/collective/schemas';
import type { Node } from '$lib/protocol/schemas';
import { user } from '$lib/server/mesh/core';
import { sharesOfGeneralFulfillmentMap } from '$lib/protocol/tree';

/**
 * Real implementation using Mesh and your database
 */
export function createCallbacks(): ComputationCallbacks {
	return {
		/**
		 * Fetch recognition data from all users
		 */
		async fetchRecognitionData(): Promise<RecognitionData[]> {
			const recognitionData: RecognitionData[] = [];
			
			try {
				// Get all users from Mesh
				const usersPath = 'users/';
				const usersList = await user.mesh.list(usersPath);
				
				// For each user, fetch their recognition tree
				for (const userId of usersList) {
					const treePath = `trees/${userId}/recognition_tree`;
					
					try {
						const tree = await user.mesh.get(treePath);
						if (!tree) continue;
						
						// Extract recognition shares from tree
						const shares = sharesOfGeneralFulfillmentMap(tree);
						
						// Convert to RecognitionData format
						for (const [toId, share] of Object.entries(shares)) {
							recognitionData.push({
								fromId: userId,
								toId,
								percentage: share * 100, // Convert 0-1 to 0-100
								timestamp: new Date()
							});
						}
					} catch (err) {
						console.warn(`[CALLBACKS] Could not fetch tree for user ${userId}:`, err);
					}
				}
				
				return recognitionData;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch recognition data:', error);
				return [];
			}
		},
		
		/**
		 * Fetch capacities with auto-update enabled
		 */
		async fetchAutoUpdateCapacities(): Promise<BaseCapacity[]> {
			try {
				// Query your database for capacities
				// This is a placeholder - adapt to your actual database
				const capacitiesPath = 'capacities/';
				const capacitiesList = await user.mesh.list(capacitiesPath);
				
				const autoUpdateCapacities: BaseCapacity[] = [];
				
				for (const capacityId of capacitiesList) {
					const capacity = await user.mesh.get(`capacities/${capacityId}`);
					
					if (capacity && capacity.auto_update_members_by_mrd) {
						autoUpdateCapacities.push(capacity);
					}
				}
				
				return autoUpdateCapacities;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch auto-update capacities:', error);
				return [];
			}
		},
		
		/**
		 * Save updated capacity members
		 */
		async saveCapacityMembers(
			capacityId: string,
			members: string[],
			added: string[],
			removed: string[],
			timestamp: Date
		): Promise<void> {
			try {
				// Update capacity in Mesh
				const capacityPath = `capacities/${capacityId}`;
				const capacity = await user.mesh.get(capacityPath);
				
				if (capacity) {
					// Update members and timestamp
					const updatedCapacity = {
						...capacity,
						members,
						last_membership_update: timestamp.toISOString()
					};
					
					await user.mesh.set(capacityPath, updatedCapacity);
					
					// Optional: Store history
					const historyPath = `capacity_membership_history/${capacityId}/${timestamp.getTime()}`;
					await user.mesh.set(historyPath, {
						capacity_id: capacityId,
						members,
						added,
						removed,
						timestamp: timestamp.toISOString()
					});
					
					console.log(
						`[CALLBACKS] Updated capacity ${capacityId}:\n` +
						`  Added: ${added.join(', ') || 'none'}\n` +
						`  Removed: ${removed.join(', ') || 'none'}`
					);
				}
			} catch (error) {
				console.error('[CALLBACKS] Failed to save capacity members:', error);
				throw error;
			}
		},
		
		/**
		 * Fetch capacities for allocation
		 */
		async fetchCapacitiesForAllocation(): Promise<BaseCapacity[]> {
			try {
				const capacitiesPath = 'capacities/';
				const capacitiesList = await user.mesh.list(capacitiesPath);
				
				const capacities: BaseCapacity[] = [];
				
				for (const capacityId of capacitiesList) {
					const capacity = await user.mesh.get(`capacities/${capacityId}`);
					
					// Only include capacities with slots
					if (capacity && capacity.capacity_slots && capacity.capacity_slots.length > 0) {
						capacities.push(capacity);
					}
				}
				
				return capacities;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch capacities:', error);
				return [];
			}
		},
		
		/**
		 * Fetch needs
		 */
		async fetchNeeds(): Promise<Map<string, BaseNeed>> {
			try {
				const needsPath = 'needs/';
				const needsList = await user.mesh.list(needsPath);
				
				const needsMap = new Map<string, BaseNeed>();
				
				for (const needId of needsList) {
					const need = await user.mesh.get(`needs/${needId}`);
					
					// Only include open or partially-fulfilled needs
					if (need && ['open', 'partially-fulfilled'].includes(need.status)) {
						needsMap.set(need.declarer_id, need);
					}
				}
				
				return needsMap;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch needs:', error);
				return new Map();
			}
		},
		
		/**
		 * Fetch member trees
		 */
		async fetchMemberTrees(memberIds: string[]): Promise<Map<string, Node>> {
			const trees = new Map<string, Node>();
			
			for (const memberId of memberIds) {
				try {
					const treePath = `trees/${memberId}/recognition_tree`;
					const tree = await user.mesh.get(treePath);
					
					if (tree) {
						trees.set(memberId, tree);
					}
				} catch (err) {
					console.warn(`[CALLBACKS] Could not fetch tree for member ${memberId}:`, err);
				}
			}
			
			return trees;
		},
		
		/**
		 * Save allocations
		 */
		async saveAllocations(capacityId: string, allocations: any): Promise<void> {
			try {
				const timestamp = new Date();
				
				// Store computation result
				const resultPath = `allocation_computations/${capacityId}/${timestamp.getTime()}`;
				await user.mesh.set(resultPath, {
					capacity_id: capacityId,
					...allocations,
					timestamp: timestamp.toISOString()
				});
				
				// Update latest allocation pointer
				const latestPath = `allocation_computations/${capacityId}/latest`;
				await user.mesh.set(latestPath, {
					timestamp: timestamp.toISOString(),
					result_path: resultPath
				});
				
				console.log(
					`[CALLBACKS] Saved allocations for ${capacityId}:\n` +
					`  Total: ${allocations.total_allocated}/${allocations.total_capacity}\n` +
					`  Members: ${allocations.member_set?.length || 0}`
				);
			} catch (error) {
				console.error('[CALLBACKS] Failed to save allocations:', error);
				throw error;
			}
		},
		
		/**
		 * Log computation events
		 */
		async logComputation(event: string, data: any): Promise<void> {
			try {
				const timestamp = new Date();
				const logPath = `computation_logs/${event}/${timestamp.getTime()}`;
				
				await user.mesh.set(logPath, {
					event,
					data,
					timestamp: timestamp.toISOString()
				});
			} catch (error) {
				console.error('[CALLBACKS] Failed to log computation:', error);
				// Don't throw - logging failures shouldn't break computations
			}
		}
	};
}
```

## Step 3: Initialize in hooks.server.ts

Update `src/hooks.server.ts`:

```typescript
import { initializeMesh, user } from "$lib/server/mesh/core"
import { initializeMonitoring } from "$lib/server/mesh/monitoring"
import { getRegistry } from "$lib/server/data-relay"
import { env } from "$env/dynamic/private"
import { startScheduler } from "$lib/server/collective"
import { createCallbacks } from "$lib/server/collective/callbacks"

let initialized = false

if (!initialized) {
  initializeMesh()
    .then(() => {
      console.log("Mesh initialized successfully")
      initializeMonitoring()

      // Initialize Data Relay System
      const registry = getRegistry(user)
      const enabledPresets = env.ENABLED_RELAYS
        ? env.ENABLED_RELAYS.split(",").map(s => s.trim())
        : ["rss-feed"]
      
      registry.registerPresets(enabledPresets)
      registry.startCacheCleanup(60000)
      console.log(`Data Relay System initialized with: ${enabledPresets.join(", ")}`)

      // Initialize Collective Recognition Scheduler
      try {
        const callbacks = createCallbacks();
        startScheduler(callbacks);
        console.log("✅ Collective Recognition Scheduler initialized")
      } catch (err) {
        console.error("❌ Failed to initialize Collective Recognition Scheduler:", err)
      }
    })
    .catch(err => {
      console.error("Failed to initialize Mesh:", err)
    })
  
  initialized = true
}
```

## Step 4: Testing

### Check Status

```bash
curl http://localhost:3000/api/collective/status
```

Response:
```json
{
  "success": true,
  "status": {
    "isRunning": true,
    "lastMembershipRun": "2025-11-02T10:30:00.000Z",
    "lastAllocationRun": "2025-11-02T10:30:00.000Z",
    "membershipRunCount": 1,
    "allocationRunCount": 1,
    "config": {
      "membershipComputationInterval": 604800000,
      "allocationComputationInterval": 86400000,
      ...
    }
  }
}
```

### Manual Trigger (for testing)

```bash
# Trigger membership computation
curl -X POST http://localhost:3000/api/collective/trigger-membership

# Trigger allocation computation
curl -X POST http://localhost:3000/api/collective/trigger-allocation
```

## Step 5: Monitoring

Watch server logs for computation events:

```
[COLLECTIVE-SCHEDULER] 🚀 Starting scheduler...
  → Membership computation: every 7.0 days
  → Allocation computation: every 1.0 days
  → Startup delay: 30.0 seconds
  → MRD threshold: 0.5
  → Auto-update membership: true
  → Auto-compute allocations: true

[COLLECTIVE-SCHEDULER] 🔄 Starting membership computation #1...
[COLLECTIVE-SCHEDULER]   → Loaded 42 recognition relationships
[COLLECTIVE-SCHEDULER]   → Found 3 auto-update capacities
[COLLECTIVE-SCHEDULER]   ✓ Updated capacity-123: +2 -0
[COLLECTIVE-SCHEDULER] ✅ Membership computation completed in 234ms
  → Processed 3 capacities
  → Updated 1 capacities
  → Added 2 members, removed 0 members

[COLLECTIVE-SCHEDULER] 🔄 Starting allocation computation #1...
[COLLECTIVE-SCHEDULER]   → Loaded 5 capacities
[COLLECTIVE-SCHEDULER]   → Loaded 12 needs
[COLLECTIVE-SCHEDULER]   ✓ capacity-123: 850.5/1000 allocated
[COLLECTIVE-SCHEDULER] ✅ Allocation computation completed in 456ms
  → Processed 5 capacities
  → Computed 5 allocations
  → Total allocated: 4250.75
```

## Troubleshooting

### No computations running

1. Check environment variables are set
2. Verify `AUTO_UPDATE_MEMBERSHIP` and `AUTO_COMPUTE_ALLOCATIONS` are true
3. Wait for startup delay to pass (default 30 seconds)
4. Check server logs for errors

### Callback errors

1. Ensure Mesh is initialized before scheduler starts
2. Verify data paths exist in Mesh
3. Add error handling to callback implementations
4. Check data format matches expected schemas

### Performance issues

1. Enable `COLLECTIVE_VERBOSE_LOGGING` to see timing
2. Reduce computation frequency (increase interval)
3. Add database indexes if using traditional database
4. Consider batching for large datasets

## Production Recommendations

1. **Add authentication** to manual trigger endpoints
2. **Monitor computation times** and set up alerts for slowdowns
3. **Log to external service** (not just console) for audit trails
4. **Set up error notifications** for failed computations
5. **Backup data** before and after membership changes
6. **Test with production-like data** in staging first
7. **Start with longer intervals** (weekly membership, daily allocations)
8. **Gradually increase frequency** as you verify stability

