/**
 * Contributor Utilities
 * 
 * Pure utility functions for working with contributors.
 * Separated from tree-specific logic for reusability.
 * 
 * V5: Added organization support - contributors can be org_ids that expand to members
 */

import type { Contributor } from '../schemas';

/**
 * Resolve contributor ID to public key if resolver provided
 */
export function resolveContributorId(
	contributorId: string,
	resolveToPublicKey?: (id: string) => string | undefined
): string {
	return resolveToPublicKey ? resolveToPublicKey(contributorId) || contributorId : contributorId;
}

/**
 * Get unique resolved contributor IDs from a list
 * Works with weighted Contributor objects
 */
export function getUniqueResolvedIds(
	contributors: Contributor[],
	resolveToPublicKey?: (id: string) => string | undefined
): string[] {
	const resolvedIds = contributors.map((c) => resolveContributorId(c.id, resolveToPublicKey));
	return [...new Set(resolvedIds)];
}

/**
 * Calculate total points for a set of contributors
 */
export function totalContributorPoints(contributors: Contributor[]): number {
	return contributors.reduce((sum, c) => sum + c.points, 0);
}

/**
 * Find a contributor in a list and return their entry
 */
export function findContributor(
	targetContributorId: string,
	contributors: Contributor[],
	resolveToPublicKey?: (id: string) => string | undefined
): Contributor | undefined {
	return contributors.find(
		(c) => resolveContributorId(c.id, resolveToPublicKey) === targetContributorId
	);
}

/**
 * Get the points assigned to a specific contributor
 */
export function getContributorPoints(
	contributors: Contributor[],
	contributorId: string,
	resolveToPublicKey?: (id: string) => string | undefined
): number {
	const contributor = findContributor(contributorId, contributors, resolveToPublicKey);
	return contributor?.points || 0;
}

/**
 * Update a contributor's points in a list
 * Returns new array (immutable)
 */
export function updateContributorPoints(
	contributors: Contributor[],
	contributorId: string,
	newPoints: number,
	resolveToPublicKey?: (id: string) => string | undefined
): Contributor[] {
	return contributors.map((c) => {
		const resolvedId = resolveContributorId(c.id, resolveToPublicKey);
		if (resolvedId === contributorId) {
			return { ...c, points: newPoints };
		}
		return c;
	});
}

// ═══════════════════════════════════════════════════════════════════
// ORGANIZATION SUPPORT (V5)
// ═══════════════════════════════════════════════════════════════════

/**
 * Resolve organization ID to array of member public keys (recursive!)
 * 
 * This is a pure utility version that takes a membership resolver function.
 * For the integrated version with stores, use resolveOrganizationMembers from users.svelte.ts
 * 
 * @param org_id - Organization identifier
 * @param getMembership - Function to get membership list for an org_id
 * @param visited - Set of already visited org_ids (prevents infinite loops)
 * @returns Array of resolved public keys (deduplicated)
 */
export function resolveOrganizationMembers(
	org_id: string,
	getMembership: (id: string) => string[] | undefined,
	visited: Set<string> = new Set()
): string[] {
	// Prevent infinite loops (circular organization references)
	if (visited.has(org_id)) {
		return [];
	}
	visited.add(org_id);

	// Get membership list
	const members = getMembership(org_id);
	if (!members) {
		return [];
	}

	const resolved: string[] = [];

	// Process each member
	for (const member of members) {
		if (member.startsWith('org_')) {
			// Nested organization - resolve recursively
			const nestedMembers = resolveOrganizationMembers(member, getMembership, visited);
			resolved.push(...nestedMembers);
		} else {
			// Direct member (pubkey) - add as-is
			resolved.push(member);
		}
	}

	// Deduplicate
	return [...new Set(resolved)];
}

/**
 * Resolve contributor ID with organization support
 * 
 * Handles three types of identifiers:
 * 1. org_id (starting with "org_") → resolves to array of pubkeys
 * 2. contact_id or other → resolves via resolveToPublicKey function
 * 3. pubkey (raw) → returns as-is
 * 
 * @param contributorId - The identifier to resolve
 * @param resolveToPublicKey - Function to resolve contact_id to pubkey
 * @param getMembership - Function to get membership list for org_id
 * @returns Array of public keys (may be empty if unresolvable)
 */
export function resolveContributorWithOrgs(
	contributorId: string,
	resolveToPublicKey?: (id: string) => string | undefined,
	getMembership?: (org_id: string) => string[] | undefined
): string[] {
	// Case 1: Organization - expand to all members
	if (contributorId.startsWith('org_')) {
		if (!getMembership) {
			// No membership resolver provided - treat as unresolvable
			return [];
		}
		return resolveOrganizationMembers(contributorId, getMembership);
	}

	// Case 2: Try resolving via resolveToPublicKey (handles contact_id, etc.)
	if (resolveToPublicKey) {
		const resolved = resolveToPublicKey(contributorId);
		if (resolved && resolved !== contributorId) {
			return [resolved];
		}
	}

	// Case 3: Already a pubkey or unresolvable - return as array
	return [contributorId];
}

/**
 * Get unique resolved contributor IDs with organization support
 * 
 * Expands organizations and resolves contacts to public keys.
 * Returns deduplicated array of all resolved public keys.
 * 
 * @param contributors - Array of Contributor objects
 * @param resolveToPublicKey - Function to resolve contact_id to pubkey
 * @param getMembership - Function to get membership list for org_id
 * @returns Array of unique resolved public keys
 */
export function getUniqueResolvedIdsWithOrgs(
	contributors: Contributor[],
	resolveToPublicKey?: (id: string) => string | undefined,
	getMembership?: (org_id: string) => string[] | undefined
): string[] {
	const allResolved: string[] = [];

	for (const c of contributors) {
		const resolved = resolveContributorWithOrgs(c.id, resolveToPublicKey, getMembership);
		allResolved.push(...resolved);
	}

	// Deduplicate
	return [...new Set(allResolved)];
}

/**
 * Calculate total points for contributors with organization expansion
 * 
 * When a contributor is an org, its points are distributed equally among all members.
 * 
 * Example:
 * - Org "org_abc" has 100 points and 5 members
 * - Each member gets 20 points from this contribution
 * 
 * @param contributors - Array of Contributor objects
 * @param getMembership - Function to get membership list for org_id
 * @returns Map of pubkey -> total points from all contributions
 */
export function calculateDistributedPoints(
	contributors: Contributor[],
	getMembership?: (org_id: string) => string[] | undefined
): Map<string, number> {
	const pointsMap = new Map<string, number>();

	for (const c of contributors) {
		if (c.id.startsWith('org_') && getMembership) {
			// Organization - distribute points among members
			const members = resolveOrganizationMembers(c.id, getMembership);
			if (members.length > 0) {
				const pointsPerMember = c.points / members.length;
				for (const member of members) {
					const current = pointsMap.get(member) || 0;
					pointsMap.set(member, current + pointsPerMember);
				}
			}
		} else {
			// Individual contributor - add points directly
			const current = pointsMap.get(c.id) || 0;
			pointsMap.set(c.id, current + c.points);
		}
	}

	return pointsMap;
}

