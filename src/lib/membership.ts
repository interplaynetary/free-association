import { writable } from 'svelte/store';
import { mutualFulfillment } from '$lib/protocol';
import type { Node } from '$lib/schema';

const players = writable<any[]>([]);

const currentMembers = writable<Node[]>([]);

// Membership qualification function
export function qualifiesForMembership(
	candidateNode: Node,
	existingMembers: Node[],
	nodesMap: Record<string, Node>,
	minimumMutualRecognitionThreshold: number = 0.05,
	minimumRelationshipCount: number = 3
): boolean {
	let validRelationships = 0;

	for (const member of existingMembers) {
		const mutualRecognition = mutualFulfillment(candidateNode, member, nodesMap);

		if (mutualRecognition >= minimumMutualRecognitionThreshold) {
			validRelationships++;
		}
	}

	return validRelationships >= minimumRelationshipCount;
}

// Add member to collective if they qualify
export function addMemberIfQualified(
	candidateNode: Node,
	existingMembers: Node[],
	nodesMap: Record<string, Node>,
	minimumMutualRecognitionThreshold?: number,
	minimumRelationshipCount?: number
): { added: boolean; reason: string } {
	const qualifies = qualifiesForMembership(
		candidateNode,
		existingMembers,
		nodesMap,
		minimumMutualRecognitionThreshold,
		minimumRelationshipCount
	);

	if (qualifies) {
		const newMembers = [...existingMembers, candidateNode];
		currentMembers.set(newMembers);
		return {
			added: true,
			reason: `Added ${candidateNode.name} to collective`
		};
	}

	return {
		added: false,
		reason: `${candidateNode.name} does not meet minimum relationship threshold`
	};
}

export { players, currentMembers };
