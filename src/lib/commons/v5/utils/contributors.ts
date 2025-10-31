/**
 * Contributor Utilities
 * 
 * Pure utility functions for working with contributors.
 * Separated from tree-specific logic for reusability.
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

