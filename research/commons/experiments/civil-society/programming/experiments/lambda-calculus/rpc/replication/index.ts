/**
 * Recognition-Based Replication
 * 
 * Selective replication based on MRS/MRD with CRDT conflict resolution.
 */

export { ReplicationManager, type ReplicationStrategy } from './manager';
export {
  SyncCoordinator,
  ConflictResolver,
  type SyncMode,
  type SyncSchedule
} from './sync-strategy';

