import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import {
    mutualContributors,
    providerShares
} from '../core.svelte';

// OBJECT ATTRIBUTES SYSTEM
// Enables semantic interoperability and distributed consensus on object definitions

// User's object attribute definitions - Gun: ~ourId/objectName/objectAttributes
// Structure: objectName → attribute key-value pairs
// Example: "homemade-pizza" → {type: "food", cuisine: "italian", prep_time: 45, difficulty: "intermediate"}
export const userObjectAttributes: Writable<Record<string, Record<string, any>>> = writable({});

// Network object attributes from mutual contributors - Gun: ~contributorId/objectName/objectAttributes
// Structure: contributorId → objectName → attribute key-value pairs
// Example: "alice" → "pizza" → {type: "food", cuisine: "italian", prep_time: 30}
export const networkObjectAttributes: Writable<
	Record<string, Record<string, Record<string, any>>>
> = writable({});

// Derived store that analyzes semantic overlaps and consensus across the network
export const commonObjectAttributes = derived(
	[userObjectAttributes, networkObjectAttributes, mutualContributors, providerShares],
	([$userObjectAttributes, $networkObjectAttributes, $mutualContributors, $providerShares]) => {
		console.log(
			'[COMMON-OBJECT-ATTRIBUTES] Calculating provider-share-weighted semantic consensus and compatibility...'
		);

		if (!$userObjectAttributes || Object.keys($userObjectAttributes).length === 0) {
			console.log('[COMMON-OBJECT-ATTRIBUTES] No user object attributes available');
			return {};
		}

		if (!$providerShares || Object.keys($providerShares).length === 0) {
			console.log('[COMMON-OBJECT-ATTRIBUTES] No provider shares available, using equal weighting');
		}

		const analysis: Record<
			string,
			{
				// Basic consensus tracking
				contributors: string[];
				total_variants: number;

				// Attribute analysis
				consensus_attributes: Record<
					string,
					{
						values: any[];
						agreement_ratio: number;
						consensus_value: any;
					}
				>;

				// Semantic compatibility scoring between contributors
				compatibility_matrix: Record<string, number>;

				// Network learning
				emerging_attributes: string[];
				stable_attributes: string[];
			}
		> = {};

		// Step 1: Collect all object variants across the network
		const objectVariants: Record<
			string,
			Array<{
				contributorId: string;
				attributes: Record<string, any>;
				providerShare: number;
			}>
		> = {};

		// Add user's objects (user always has provider share of 1.0 - they fully recognize themselves)
		Object.entries($userObjectAttributes).forEach(([objectName, attributes]) => {
			if (!objectVariants[objectName]) objectVariants[objectName] = [];
			objectVariants[objectName].push({
				contributorId: 'user',
				attributes,
				providerShare: 1.0
			});
		});

		// Add network objects from mutual contributors only
		$mutualContributors.forEach((contributorId) => {
			const contributorObjects = $networkObjectAttributes[contributorId];
			if (!contributorObjects) return;

			// Get this contributor's provider share for weighting
			const providerShare = $providerShares[contributorId] || 0;

			Object.entries(contributorObjects).forEach(([objectName, attributes]) => {
				if (!objectVariants[objectName]) objectVariants[objectName] = [];
				objectVariants[objectName].push({
					contributorId,
					attributes,
					providerShare
				});
			});
		});

		// Step 2: Analyze each object's consensus and compatibility
		Object.entries(objectVariants).forEach(([objectName, variants]) => {
			if (variants.length < 2) return; // Need at least 2 variants for consensus analysis

			console.log(
				`[COMMON-OBJECT-ATTRIBUTES] Analyzing ${variants.length} variants of "${objectName}"`
			);

			const contributors = variants.map((v) => v.contributorId);
			const totalVariantWeight = variants.reduce((sum, variant) => sum + variant.providerShare, 0);

			// Collect all unique attribute keys across variants
			const allAttributeKeys = new Set<string>();
			variants.forEach((variant) => {
				Object.keys(variant.attributes).forEach((key) => allAttributeKeys.add(key));
			});

			// Analyze consensus for each attribute
			const consensusAttributes: Record<
				string,
				{
					values: any[];
					agreement_ratio: number;
					consensus_value: any;
				}
			> = {};

			const emergingAttributes: string[] = [];
			const stableAttributes: string[] = [];

			allAttributeKeys.forEach((attributeKey) => {
				const values: any[] = [];
				const weightedValues: Array<{ value: any; weight: number }> = [];
				let totalWeightPresent = 0;
				let totalWeightAll = 0;

				variants.forEach((variant) => {
					totalWeightAll += variant.providerShare;

					if (variant.attributes[attributeKey] !== undefined) {
						values.push(variant.attributes[attributeKey]);
						weightedValues.push({
							value: variant.attributes[attributeKey],
							weight: variant.providerShare
						});
						totalWeightPresent += variant.providerShare;
					}
				});

				// Calculate provider-share-weighted agreement ratio
				const agreementRatio = totalWeightAll > 0 ? totalWeightPresent / totalWeightAll : 0;

				// Determine consensus value using provider-share weighting
				let consensusValue: any = null;
				if (weightedValues.length > 0) {
					const firstValue = weightedValues[0].value;

					// For numbers, calculate weighted average if agreement is high
					if (typeof firstValue === 'number' && agreementRatio > 0.7) {
						const weightedSum = weightedValues.reduce(
							(sum, item) => sum + item.value * item.weight,
							0
						);
						const totalWeight = weightedValues.reduce((sum, item) => sum + item.weight, 0);
						consensusValue = totalWeight > 0 ? weightedSum / totalWeight : firstValue;
					} else {
						// For strings and other values, use weighted mode (most common by provider share)
						const valueWeights: Record<string, number> = {};
						weightedValues.forEach((item) => {
							const key = String(item.value);
							valueWeights[key] = (valueWeights[key] || 0) + item.weight;
						});

						const mostWeighted = Object.entries(valueWeights).sort(([, a], [, b]) => b - a)[0];
						consensusValue = mostWeighted ? mostWeighted[0] : String(firstValue);

						// Convert back to original type if it was a number
						if (typeof firstValue === 'number' && !isNaN(Number(consensusValue))) {
							consensusValue = Number(consensusValue);
						}
					}
				}

				consensusAttributes[attributeKey] = {
					values,
					agreement_ratio: agreementRatio,
					consensus_value: consensusValue
				};

				// Classify attribute stability
				if (agreementRatio >= 0.8) {
					stableAttributes.push(attributeKey);
				} else if (agreementRatio >= 0.3) {
					emergingAttributes.push(attributeKey);
				}
			});

			// Step 3: Calculate compatibility matrix between contributors
			const compatibilityMatrix: Record<string, number> = {};

			for (let i = 0; i < variants.length; i++) {
				for (let j = i + 1; j < variants.length; j++) {
					const variant1 = variants[i];
					const variant2 = variants[j];

					const sharedAttributes: string[] = [];
					const conflictingAttributes: string[] = [];
					const totalAttributes = new Set([
						...Object.keys(variant1.attributes),
						...Object.keys(variant2.attributes)
					]);

					totalAttributes.forEach((attr) => {
						const val1 = variant1.attributes[attr];
						const val2 = variant2.attributes[attr];

						if (val1 !== undefined && val2 !== undefined) {
							if (
								val1 === val2 ||
								(typeof val1 === 'number' &&
									typeof val2 === 'number' &&
									Math.abs(val1 - val2) < Math.max(val1, val2) * 0.2)
							) {
								sharedAttributes.push(attr);
							} else {
								conflictingAttributes.push(attr);
							}
						}
					});

					// Calculate compatibility score (0-1)
					const compatibility =
						totalAttributes.size > 0
							? sharedAttributes.length /
								(sharedAttributes.length + conflictingAttributes.length * 2)
							: 0;

					const key1 = `${variant1.contributorId}->${variant2.contributorId}`;
					const key2 = `${variant2.contributorId}->${variant1.contributorId}`;
					compatibilityMatrix[key1] = compatibility;
					compatibilityMatrix[key2] = compatibility;
				}
			}

			analysis[objectName] = {
				contributors,
				total_variants: variants.length,
				consensus_attributes: consensusAttributes,
				compatibility_matrix: compatibilityMatrix,
				emerging_attributes: emergingAttributes,
				stable_attributes: stableAttributes
			};

			console.log(
				`[COMMON-OBJECT-ATTRIBUTES] "${objectName}" (${variants.length} variants, total weight: ${totalVariantWeight.toFixed(3)}): ${stableAttributes.length} stable, ${emergingAttributes.length} emerging attributes`
			);
		});

		console.log(
			`[COMMON-OBJECT-ATTRIBUTES] Analyzed ${Object.keys(analysis).length} objects with semantic consensus`
		);
		return analysis;
	}
);

// Derived store for semantic object discovery - helps find objects by attribute similarity
export const semanticObjectIndex = derived(
	[commonObjectAttributes],
	([$commonObjectAttributes]) => {
		console.log('[SEMANTIC-OBJECT-INDEX] Building semantic search index...');

		// Create reverse index: attribute → objects that have it
		const attributeIndex: Record<string, string[]> = {};

		// Create value index: attribute=value → objects
		const valueIndex: Record<string, string[]> = {};

		Object.entries($commonObjectAttributes).forEach(([objectName, analysis]) => {
			Object.entries(analysis.consensus_attributes).forEach(([attributeKey, attributeData]) => {
				// Add to attribute index
				if (!attributeIndex[attributeKey]) attributeIndex[attributeKey] = [];
				attributeIndex[attributeKey].push(objectName);

				// Add to value index
				if (attributeData.consensus_value !== null) {
					const valueKey = `${attributeKey}=${attributeData.consensus_value}`;
					if (!valueIndex[valueKey]) valueIndex[valueKey] = [];
					valueIndex[valueKey].push(objectName);
				}
			});
		});

		console.log(
			`[SEMANTIC-OBJECT-INDEX] Indexed ${Object.keys(attributeIndex).length} attributes across ${Object.keys($commonObjectAttributes).length} objects`
		);

		return {
			byAttribute: attributeIndex,
			byValue: valueIndex,

			// Helper function to find semantically similar objects
			findSimilar: (targetObject: string, minCompatibility: number = 0.7) => {
				const analysis = $commonObjectAttributes[targetObject];
				if (!analysis) return [];

				return Object.entries($commonObjectAttributes)
					.filter(([objName, _]) => objName !== targetObject)
					.map(([objName, objAnalysis]) => {
						// Calculate semantic similarity based on shared stable attributes
						const targetStable = new Set(analysis.stable_attributes);
						const objStable = new Set(objAnalysis.stable_attributes);
						const intersection = new Set([...targetStable].filter((x) => objStable.has(x)));
						const union = new Set([...targetStable, ...objStable]);

						const similarity = union.size > 0 ? intersection.size / union.size : 0;

						return { objectName: objName, similarity };
					})
					.filter(({ similarity }) => similarity >= minCompatibility)
					.sort((a, b) => b.similarity - a.similarity);
			}
		};
	}
);

// Loading state for object attributes
export const isLoadingObjectAttributes = writable(false);

// ...