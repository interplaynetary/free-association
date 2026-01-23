
import { verifyGenericVC } from "./vc";
import { type Node, aggregateUp, Reducers, Extractors, Graph } from "../aggregation";

/**
 * VC Aggregation Bridge
 * 
 * Connecting verifiable claims to the generic aggregation logic.
 */


/**
 * Process a list of Verifiable Credentials into Graph Nodes.
 * 
 * @param vcs List of signed Verifiable Credentials
 * @returns List of verified Nodes populated with attributes/potentials
 */
export async function processVCsToNodes(vcs: any[]): Promise<Node[]> {
    // Phase 1: Ingestion & Verification
    const verifiedVCs: any[] = [];
    for (const vc of vcs) {
        // In real app: verifyGenericVC(vc)
        // For tests: bypass signature if mocking, or assume verifyGenericVC handles it
        const result = await verifyGenericVC(vc);
        if (result.verified) {
            verifiedVCs.push(vc);
        } else {
            console.warn(`Skipping invalid credential from ${vc.issuer}:`, result.error);
        }
    }

    // Phase 2: Resolution (Per-Observer LWW)
    // Map<SubjectID, Map<IssuerID, LatestVC>>
    const resolutionMap = new Map<string, Map<string, any>>();

    for (const vc of verifiedVCs) {
        const subjectId = vc.credentialSubject.id || vc.issuer;
        const issuerId = vc.issuer;
        const issueDate = new Date(vc.issuanceDate).getTime();

        if (!resolutionMap.has(subjectId)) {
            resolutionMap.set(subjectId, new Map());
        }

        const issuerMap = resolutionMap.get(subjectId)!;
        const existingVC = issuerMap.get(issuerId);

        if (!existingVC) {
            issuerMap.set(issuerId, vc);
        } else {
            // LWW per Observer
            const existingDate = new Date(existingVC.issuanceDate).getTime();
            if (issueDate > existingDate) {
                issuerMap.set(issuerId, vc);
            }
        }
    }

    // Phase 3: Materialization (Merge)
    const validNodes: Node[] = [];

    for (const [subjectId, issuerMap] of resolutionMap) {
        // Merge attributes from all resolved VCs for this subject
        // Strategy: Union for Arrays (Set), LWW-Global for Scalars (Arbitrary for now, or accumulation)

        let mergedNode: Node = { id: subjectId };
        const combinedMemberOf = new Set<string>();

        // Sort issuers deterministicly or by some priority if needed? 
        // For now, order is arbitrary (iteration order).
        for (const vc of issuerMap.values()) {
            const subject = vc.credentialSubject;

            // 1. Merge Scalars (Spread updates)
            mergedNode = { ...mergedNode, ...subject };

            // 2. Accumulate Topology (Union)
            if (Array.isArray(subject.memberOf)) {
                subject.memberOf.forEach((m: any) => combinedMemberOf.add(String(m)));
            }

            // Restore: Handle specialized mappings
            if (subject.alumniOf) {
                combinedMemberOf.add('AlumniGroup');
            }
            if (subject.potential && (subject.potential as any).memberOf) {
                (subject.potential as any).memberOf.forEach((m: any) => combinedMemberOf.add(String(m)));
            }
        }

        // Apply accumulated topology
        mergedNode.memberOf = combinedMemberOf;

        // Ensure ID determines Node identity
        mergedNode.id = subjectId;

        validNodes.push(mergedNode);
    }

    return validNodes;
}

/**
 * Example: Aggregating Capacity from VCs
 * Demonstrates how to use the generic system for specific "Economic" logic.
 */
export async function aggregateCapacityFromVCs(
    vcs: any[],
    targetType: string,
    targetNodeId: string
): Promise<number> {
    const nodes = await processVCsToNodes(vcs);
    const graph = new Graph(nodes);

    return graph.from(targetNodeId).aggregateUp(
        "memberOf",
        Extractors.potentialsByType(targetType),
        (acc: number, magnitudes: number[]) => acc + (magnitudes ? magnitudes.reduce((a, b) => a + b, 0) : 0),
        0
    );
}

