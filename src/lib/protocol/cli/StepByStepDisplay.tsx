/**
 * Step-by-Step Iteration Display
 * 
 * Interactive iteration display - press Enter to advance
 */

import React, { useState, useEffect } from 'react';
import { Box, Text, useInput } from 'ink';
import type { AllocationResult, SystemStateSnapshot } from '../free';
import type { Commitment } from '../schemas';
import {
	buildSystemState,
	computeAllocations,
	computeMutualRecognition,
	type GlobalRecognitionWeights
} from '../free';

interface StepByStepDisplayProps {
	commitments?: Record<string, Commitment>;
	myPubKey?: string;
	onComplete?: () => void;
	onBack?: () => void;
}

export const StepByStepDisplay: React.FC<StepByStepDisplayProps> = ({
	commitments: providedCommitments,
	myPubKey: providedPubKey,
	onComplete,
	onBack
}) => {
	// Load from storage if not provided
	const [commitments, setCommitments] = useState<Record<string, Commitment> | null>(
		providedCommitments || null
	);
	const [myPubKey, setMyPubKey] = useState<string>(providedPubKey || '');
	
	useEffect(() => {
		if (!providedCommitments) {
			// Load from storage
			const storage = require('./data/storage');
			const allCommitments = storage.loadCommitments();
			setCommitments(allCommitments || {});
			const config = storage.loadConfig();
			if (config) {
				setMyPubKey(config.myPubKey);
			}
		}
	}, [providedCommitments]);
	const [iteration, setIteration] = useState(0);
	const [previousState, setPreviousState] = useState<SystemStateSnapshot | null>(null);
	const [allAllocations, setAllAllocations] = useState<Record<string, Record<string, number>>>({});
	const [converged, setConverged] = useState(false);
	const [results, setResults] = useState<Array<{ provider: string; result: AllocationResult }>>([]);
	const [needMagnitude, setNeedMagnitude] = useState(0);
	const [peopleNames] = useState<Record<string, string>>({
		'alice-pub-key': 'Alice',
		'bob-pub-key': 'Bob',
		'carol-pub-key': 'Carol'
	});

	// Initialize allocations tracking
	useEffect(() => {
		if (!commitments) return;
		const initial: Record<string, Record<string, number>> = {};
		for (const pubKey of Object.keys(commitments)) {
			initial[pubKey] = {};
		}
		setAllAllocations(initial);
	}, [commitments]);

	// Compute current iteration
	useEffect(() => {
		if (converged || !commitments) return;

		// Update commitments with reduced needs
		const updatedCommitments: Record<string, Commitment> = {};
		for (const [pubKey, commitment] of Object.entries(commitments)) {
			const updatedNeedSlots = (commitment.need_slots || []).map(slot => ({
				...slot,
				quantity: Math.max(0, slot.quantity - (allAllocations[pubKey]?.[slot.need_type_id] || 0))
			}));
			updatedCommitments[pubKey] = {
				...commitment,
				need_slots: updatedNeedSlots
			};
		}

		const systemState = buildSystemState(updatedCommitments, previousState || undefined);

		// Compute allocations from all providers
		const allResults: Array<{ provider: string; result: AllocationResult }> = [];
		
		for (const [providerPubKey, providerCommitment] of Object.entries(updatedCommitments)) {
			const providerCapacity = providerCommitment.capacity_slots || [];
			if (providerCapacity.length === 0) continue;

			const myRecognition = providerCommitment.global_recognition_weights || {};
			const othersRecognition: Record<string, GlobalRecognitionWeights> = {};
			for (const [otherPubKey, otherCommitment] of Object.entries(updatedCommitments)) {
				if (otherPubKey !== providerPubKey) {
					othersRecognition[otherPubKey] = otherCommitment.global_recognition_weights || {};
				}
			}
			const mutualRecognition = computeMutualRecognition(myRecognition, othersRecognition, providerPubKey);

			const result = computeAllocations(
				providerPubKey,
				providerCapacity,
				myRecognition,
				mutualRecognition,
				updatedCommitments,
				systemState,
				previousState
			);

			allResults.push({ provider: providerPubKey, result });
		}

		setResults(allResults);
		
		if (allResults.length > 0) {
			setNeedMagnitude(allResults[0].result.convergence.totalNeedMagnitude);
			
			if (allResults[0].result.convergence.universalSatisfaction || 
			    allResults[0].result.convergence.isConverged) {
				setConverged(true);
			}
		}

		setPreviousState(systemState);
	}, [iteration, converged]);

	useInput((input, key) => {
		if (converged) {
			if (onBack) onBack();
			else if (onComplete) onComplete();
			return;
		}

		if (key.return) {
			// Update allocations for next iteration
			const newAllocations = { ...allAllocations };
			for (const { result } of results) {
				for (const allocation of result.allocations) {
					const recipientId = allocation.recipient_pubkey;
					const typeId = allocation.need_type_id;
					if (!newAllocations[recipientId]) {
						newAllocations[recipientId] = {};
					}
					newAllocations[recipientId][typeId] = 
						(newAllocations[recipientId][typeId] || 0) + allocation.quantity;
				}
			}
			setAllAllocations(newAllocations);
			setIteration(iteration + 1);
		} else if (key.escape) {
			if (onBack) onBack();
			else if (onComplete) onComplete();
		}
	});
	
	if (!commitments || Object.keys(commitments).length === 0) {
		return (
			<Box flexDirection="column" padding={2}>
				<Text color="yellow">⚠️  No commitments found</Text>
				<Text dimColor>Add some capacities and needs first, then try again.</Text>
				<Box marginTop={1}>
					<Text>Press Esc to go back</Text>
				</Box>
			</Box>
		);
	}

	const totalAllocations = results.flatMap(r => r.result.allocations);
	const convergence = results.length > 0 ? results[0].result.convergence : null;

	return (
		<Box flexDirection="column">
			<Box borderStyle="double" borderColor="cyan" padding={1} marginBottom={1}>
				<Text bold color="cyan">
					🔄 ITERATION {iteration} - Step-by-Step Mode
				</Text>
			</Box>

			{/* Current Needs */}
			<Box flexDirection="column" borderStyle="round" borderColor="yellow" padding={1} marginBottom={1}>
				<Text bold color="yellow">📊 Current Needs:</Text>
				{results.length > 0 && results[0].result && (
					<Box flexDirection="column" marginTop={1}>
						{Object.entries(buildSystemState(commitments).needsByPersonAndType).map(([person, needsByType]) => {
							const totalNeed = Math.sqrt(
								Object.values(needsByType).reduce((sum, need) => sum + (need as number) ** 2, 0)
							);
							const personName = peopleNames[person] || person.substring(0, 8);
							return (
								<Box key={person} flexDirection="column" marginLeft={1}>
									<Text>{personName}: <Text color="magenta">{totalNeed.toFixed(2)}</Text> (magnitude)</Text>
									{Object.entries(needsByType).map(([type, amount]) => (
										(amount as number) > 0.01 ? (
											<Text key={`${person}-${type}`} dimColor marginLeft={2}>
												- {type}: {(amount as number).toFixed(2)}
											</Text>
										) : null
									))}
								</Box>
							);
						})}
					</Box>
				)}
			</Box>

			{/* Allocations Made */}
			<Box flexDirection="column" borderStyle="round" borderColor="green" padding={1} marginBottom={1}>
				<Text bold color="green">💰 Allocations Made:</Text>
				{totalAllocations.length === 0 ? (
					<Text dimColor marginLeft={1}>  (none this iteration)</Text>
				) : (
					<Box flexDirection="column" marginTop={1}>
						{results.map(({ provider, result }) => (
							result.allocations.length > 0 && (
								<Box key={provider} flexDirection="column" marginLeft={1}>
									<Text color="cyan">From {peopleNames[provider] || provider.substring(0, 8)}:</Text>
									{result.allocations.map((alloc, idx) => (
										<Text key={idx} marginLeft={2}>
											→ {peopleNames[alloc.recipient_pubkey] || alloc.recipient_pubkey.substring(0, 8)}: 
											<Text color="green"> {alloc.quantity.toFixed(2)} </Text>
											{alloc.need_type_id} ({alloc.tier})
										</Text>
									))}
								</Box>
							)
						))}
					</Box>
				)}
			</Box>

			{/* Convergence Status */}
			{convergence && (
				<Box flexDirection="column" borderStyle="round" borderColor="blue" padding={1} marginBottom={1}>
					<Text bold color="blue">📈 Convergence Status:</Text>
					<Box flexDirection="column" marginTop={1} marginLeft={1}>
						<Text>Need Magnitude: <Text color="magenta">{convergence.totalNeedMagnitude.toFixed(2)}</Text></Text>
						{iteration > 0 && convergence.percentNeedReduction !== undefined && (
							<Text>Reduction: <Text color="cyan">{convergence.percentNeedReduction.toFixed(1)}%</Text></Text>
						)}
						<Text>People Satisfied: <Text color="cyan">{convergence.percentNeedsMet.toFixed(1)}%</Text></Text>
					</Box>
				</Box>
			)}

			{/* Instructions */}
			{converged ? (
				<Box borderStyle="round" borderColor="green" padding={1} justifyContent="center">
					<Text color="green" bold>
						🎉 CONVERGENCE ACHIEVED! Press any key to return...
					</Text>
				</Box>
			) : (
				<Box borderStyle="round" borderColor="yellow" padding={1} justifyContent="center">
					<Text color="yellow">
						👉 Press <Text bold>ENTER</Text> for next iteration | <Text bold>ESC</Text> to quit
					</Text>
				</Box>
			)}
		</Box>
	);
};

