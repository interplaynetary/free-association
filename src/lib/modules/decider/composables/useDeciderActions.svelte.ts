/**
 * @module useDeciderActions
 * Composable for handling all decider user actions
 * Extracts action handler logic from DeciderWidget
 */

import type { ReactiveP2PDecider, ProposedConfigChanges, SupportExpression } from '../decider.svelte';

export interface DeciderActions {
	submitProposal: (content: string) => Promise<void>;
	submitChallenge: (proposalPub: string, content: string) => Promise<void>;
	submitComment: (proposalPub: string, content: string) => Promise<void>;
	submitModification: (proposalPub: string, content: string) => Promise<void>;
	submitSupport: (proposalPub: string, allocation: SupportExpression) => Promise<void>;
	submitConfigProposal: (description: string, changes: ProposedConfigChanges) => Promise<void>;
}

export interface ActionOptions {
	onSuccess?: (message: string) => void;
	onError?: (message: string) => void;
	validateContent?: (content: string, maxLength?: number) => boolean;
}

export function useDeciderActions(
	decider: ReactiveP2PDecider | null,
	options: ActionOptions = {}
): DeciderActions {
	const { onSuccess, onError, validateContent } = options;
	
	async function submitProposal(content: string): Promise<void> {
		if (!decider) {
			onError?.('Decider not initialized');
			return;
		}
		
		if (validateContent && !validateContent(content)) {
			return;
		}
		
		try {
			await decider.writeMyProposal(content);
			onSuccess?.('Proposal submitted successfully');
		} catch (e) {
			const message = e instanceof Error ? e.message : 'Failed to submit proposal';
			onError?.(message);
		}
	}
	
	async function submitChallenge(proposalPub: string, content: string): Promise<void> {
		if (!decider) {
			onError?.('Decider not initialized');
			return;
		}
		
		if (validateContent && !validateContent(content)) {
			return;
		}
		
		try {
			await decider.writeMyChallengeToProposal(proposalPub, content);
			onSuccess?.('Challenge submitted successfully');
		} catch (e) {
			const message = e instanceof Error ? e.message : 'Failed to submit challenge';
			onError?.(message);
		}
	}
	
	async function submitComment(proposalPub: string, content: string): Promise<void> {
		if (!decider) {
			onError?.('Decider not initialized');
			return;
		}
		
		if (validateContent && !validateContent(content)) {
			return;
		}
		
		try {
			await decider.writeMyCommentOnProposal(proposalPub, content);
			onSuccess?.('Comment submitted successfully');
		} catch (e) {
			const message = e instanceof Error ? e.message : 'Failed to submit comment';
			onError?.(message);
		}
	}
	
	async function submitModification(proposalPub: string, content: string): Promise<void> {
		if (!decider) {
			onError?.('Decider not initialized');
			return;
		}
		
		if (validateContent && !validateContent(content)) {
			return;
		}
		
		try {
			await decider.writeMyModificationToProposal(proposalPub, content);
			onSuccess?.('Modification submitted successfully');
		} catch (e) {
			const message = e instanceof Error ? e.message : 'Failed to submit modification';
			onError?.(message);
		}
	}
	
	async function submitSupport(proposalPub: string, allocation: SupportExpression): Promise<void> {
		if (!decider) {
			onError?.('Decider not initialized');
			return;
		}
		
		try {
			await decider.writeMySupportForProposal(proposalPub, allocation);
			onSuccess?.('Support submitted successfully');
		} catch (e) {
			const message = e instanceof Error ? e.message : 'Failed to submit support';
			onError?.(message);
		}
	}
	
	async function submitConfigProposal(description: string, changes: ProposedConfigChanges): Promise<void> {
		if (!decider) {
			onError?.('Decider not initialized');
			return;
		}
		
		if (validateContent && !validateContent(description, 1000)) {
			return;
		}
		
		try {
			await decider.writeMyConfigProposal(description, changes);
			onSuccess?.('Configuration proposal submitted successfully');
		} catch (e) {
			const message = e instanceof Error ? e.message : 'Failed to submit config proposal';
			onError?.(message);
		}
	}
	
	return {
		submitProposal,
		submitChallenge,
		submitComment,
		submitModification,
		submitSupport,
		submitConfigProposal
	};
}











