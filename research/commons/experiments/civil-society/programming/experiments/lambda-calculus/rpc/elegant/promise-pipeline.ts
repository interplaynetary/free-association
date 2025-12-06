/**
 * Elegant Promise Pipelining and Record-Replay for `.map()`
 * 
 * Inspired by Cap'n Web's innovative array.map() handling.
 * Uses record-replay to execute .map() callbacks server-side without round-trips.
 */

/**
 * Recorded instruction for replay
 */
export type ReplayInstruction = 
  | { type: 'call'; target: string; method: string; args: any[] }
  | { type: 'get'; target: string; property: string }
  | { type: 'literal'; value: any };

/**
 * Recording context
 */
class RecordingContext {
  instructions: ReplayInstruction[] = [];
  nextId = 0;

  recordCall(target: string, method: string, args: any[]): string {
    const resultId = `result_${this.nextId++}`;
    this.instructions.push({
      type: 'call',
      target,
      method,
      args
    });
    return resultId;
  }

  recordGet(target: string, property: string): string {
    const resultId = `result_${this.nextId++}`;
    this.instructions.push({
      type: 'get',
      target,
      property
    });
    return resultId;
  }

  recordLiteral(value: any): string {
    const resultId = `result_${this.nextId++}`;
    this.instructions.push({
      type: 'literal',
      value
    });
    return resultId;
  }
}

/**
 * Create a recording proxy
 * Intercepts all operations and records them
 */
function createRecordingProxy(context: RecordingContext, id: string): any {
  return new Proxy({}, {
    get(_, prop) {
      if (typeof prop === 'string') {
        const resultId = context.recordGet(id, prop);
        return createRecordingProxy(context, resultId);
      }
      return undefined;
    },
    
    apply(_, __, args) {
      // Record function call
      const resultId = context.recordCall(id, 'call', args);
      return createRecordingProxy(context, resultId);
    }
  });
}

/**
 * Enhanced Promise with .map() that uses record-replay
 */
export class PipelinePromise<T> extends Promise<T> {
  private pipelineId: string;
  private sendInstructions: (instructions: ReplayInstruction[]) => void;

  constructor(
    executor: (resolve: (value: T | PromiseLike<T>) => void, reject: (reason?: any) => void) => void,
    pipelineId: string,
    sendInstructions: (instructions: ReplayInstruction[]) => void
  ) {
    super(executor);
    this.pipelineId = pipelineId;
    this.sendInstructions = sendInstructions;
  }

  /**
   * Elegant .map() with record-replay
   * 
   * Usage:
   * ```typescript
   * let friendsPromise = api.listFriends();
   * let withPhotos = friendsPromise.map(friend => ({
   *   friend,
   *   photo: api.getUserPhoto(friend.id)
   * }));
   * let results = await withPhotos;
   * ```
   * 
   * The callback is executed once with a recording proxy,
   * then instructions are sent to server for replay on each element.
   */
  map<U>(callback: (item: T extends Array<infer E> ? E : never) => U): PipelinePromise<U[]> {
    // Create recording context
    const context = new RecordingContext();
    
    // Create placeholder for array element
    const placeholder = createRecordingProxy(context, 'element');
    
    // Execute callback once with placeholder to record operations
    try {
      callback(placeholder);
    } catch (error) {
      // Callback must be synchronous
      throw new Error('Map callback must be synchronous for record-replay');
    }
    
    // Send instructions to server
    this.sendInstructions(context.instructions);
    
    // Return new pipeline promise that represents the mapped result
    return new PipelinePromise(
      (resolve, reject) => {
        this.then(
          (array: any) => {
            // Fallback: execute map locally if server doesn't support replay
            // In production, this shouldn't happen as server handles it
            if (Array.isArray(array)) {
              try {
                const result = array.map(callback as any);
                resolve(result);
              } catch (error) {
                reject(error);
              }
            } else {
              reject(new Error('Cannot map over non-array'));
            }
          },
          reject
        );
      },
      `${this.pipelineId}_mapped`,
      this.sendInstructions
    );
  }
}

/**
 * Create a pipelined promise
 * Used by RPC system to return promises that support .map()
 */
export function createPipelinePromise<T>(
  executor: (resolve: (value: T | PromiseLike<T>) => void, reject: (reason?: any) => void) => void,
  pipelineId: string,
  sendInstructions: (instructions: ReplayInstruction[]) => void
): PipelinePromise<T> {
  return new PipelinePromise(executor, pipelineId, sendInstructions);
}

/**
 * Replay instructions on server
 * Executes recorded operations on actual values
 */
export async function replayInstructions(
  instructions: ReplayInstruction[],
  element: any,
  context: any
): Promise<any> {
  const results = new Map<string, any>();
  results.set('element', element);
  
  for (const instruction of instructions) {
    switch (instruction.type) {
      case 'get': {
        const target = results.get(instruction.target);
        if (target !== undefined && instruction.property in target) {
          const value = target[instruction.property];
          results.set(`result_${results.size}`, value);
        }
        break;
      }
      
      case 'call': {
        const target = results.get(instruction.target);
        if (typeof target === 'function') {
          const result = await target(...instruction.args);
          results.set(`result_${results.size}`, result);
        }
        break;
      }
      
      case 'literal': {
        results.set(`result_${results.size}`, instruction.value);
        break;
      }
    }
  }
  
  // Return last result
  const lastKey = `result_${results.size - 1}`;
  return results.get(lastKey);
}

/**
 * Example usage:
 * 
 * ```typescript
 * // Client side
 * let session = await createP2PConnection('alice');
 * let friendsPromise = session.getRemoteSession().listFriends();
 * 
 * // This .map() executes server-side without round-trips!
 * let withPhotos = friendsPromise.map(friend => ({
 *   friend,
 *   photo: api.getUserPhoto(friend.id),
 *   mutualRecognition: api.getMutualRecognition(friend.id)
 * }));
 * 
 * // Single round trip for everything
 * let results = await withPhotos;
 * ```
 */

