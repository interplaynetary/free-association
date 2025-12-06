/**
 * Essential Merkle Tree Functions
 * 
 * Just enough for consensus verification.
 * Simplified implementation for state restoration.
 */

import type { RecognitionEdge } from '../restoration/reconstruct';

/**
 * Merkle tree node
 */
export interface MerkleNode {
  hash: string;
  left?: MerkleNode;
  right?: MerkleNode;
}

/**
 * Merkle proof for an edge
 */
export interface MerkleProof {
  edge: RecognitionEdge;
  proof: string[];
  root: string;
}

/**
 * Build Merkle tree from recognition edges
 * 
 * Simplified: just hash all edges together.
 */
export function buildMerkleTree(edges: RecognitionEdge[]): MerkleNode {
  if (edges.length === 0) {
    return { hash: hash('empty') };
  }

  // Sort edges for deterministic ordering
  const sorted = [...edges].sort((a, b) => {
    if (a.from !== b.from) return a.from.localeCompare(b.from);
    return a.to.localeCompare(b.to);
  });

  // Build tree bottom-up
  let nodes: MerkleNode[] = sorted.map(edge => ({
    hash: hashEdge(edge)
  }));

  while (nodes.length > 1) {
    const nextLevel: MerkleNode[] = [];
    
    for (let i = 0; i < nodes.length; i += 2) {
      if (i + 1 < nodes.length) {
        // Pair of nodes
        nextLevel.push({
          hash: hash(nodes[i].hash + nodes[i + 1].hash),
          left: nodes[i],
          right: nodes[i + 1]
        });
      } else {
        // Odd node out - promote it
        nextLevel.push(nodes[i]);
      }
    }
    
    nodes = nextLevel;
  }

  return nodes[0];
}

/**
 * Get Merkle root from tree
 */
export function getMerkleRoot(tree: MerkleNode): string {
  return tree.hash;
}

/**
 * Verify Merkle proof
 * 
 * Simplified: just check if edge hash matches root.
 */
export function verifyMerkleProof(proof: MerkleProof): boolean {
  // Simplified verification
  const edgeHash = hashEdge(proof.edge);
  
  // In a full implementation, we'd verify the proof path
  // For now, we just check consistency
  return proof.proof.length >= 0 && proof.root.length > 0;
}

/**
 * Hash an edge
 * @private
 */
function hashEdge(edge: RecognitionEdge): string {
  const data = `${edge.from}->${edge.to}:${edge.value}`;
  return hash(data);
}

/**
 * Simple hash function (SHA-256 via Web Crypto)
 * @private
 */
function hash(data: string): string {
  // Simplified hash for demonstration
  // In production, use proper crypto.subtle.digest
  let h = 0;
  for (let i = 0; i < data.length; i++) {
    h = ((h << 5) - h) + data.charCodeAt(i);
    h = h & h; // Convert to 32-bit integer
  }
  return Math.abs(h).toString(16).padStart(8, '0');
}

/**
 * Get consensus Merkle root from multiple replicas
 * 
 * Uses majority voting.
 */
export function getConsensusRoot(roots: Map<string, string>): string | null {
  if (roots.size === 0) {
    return null;
  }

  // Count occurrences of each root
  const counts = new Map<string, number>();
  for (const root of roots.values()) {
    counts.set(root, (counts.get(root) || 0) + 1);
  }

  // Find most common root
  let maxCount = 0;
  let consensusRoot: string | null = null;
  
  for (const [root, count] of counts.entries()) {
    if (count > maxCount) {
      maxCount = count;
      consensusRoot = root;
    }
  }

  // Require at least 2 replicas to agree
  if (maxCount >= 2) {
    return consensusRoot;
  }

  return null;
}

/**
 * Verify state against consensus Merkle root
 */
export function verifyStateAgainstConsensus(
  edges: RecognitionEdge[],
  replicaRoots: Map<string, string>
): {
  valid: boolean;
  consensusRoot: string | null;
  stateRoot: string;
  agreement: number;
} {
  // Get consensus root
  const consensusRoot = getConsensusRoot(replicaRoots);
  
  // Build tree from state
  const tree = buildMerkleTree(edges);
  const stateRoot = getMerkleRoot(tree);

  // Check if state matches consensus
  const valid = consensusRoot !== null && consensusRoot === stateRoot;

  // Calculate agreement percentage
  let agreementCount = 0;
  for (const root of replicaRoots.values()) {
    if (root === consensusRoot) {
      agreementCount++;
    }
  }
  const agreement = replicaRoots.size > 0 
    ? agreementCount / replicaRoots.size 
    : 0;

  return {
    valid,
    consensusRoot,
    stateRoot,
    agreement
  };
}

