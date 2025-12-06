/**
 * Elegant RPC Features
 * 
 * Advanced features inspired by Cap'n Web:
 * - Promise pipelining
 * - Record-replay for .map()
 * - Elegant API design
 */

export {
  PipelinePromise,
  createPipelinePromise,
  replayInstructions,
  type ReplayInstruction
} from './promise-pipeline';

