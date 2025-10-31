import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

/**
 * Generic Tree Zipper for CLI Navigation
 * 
 * A tree zipper is a functional data structure that maintains:
 * 1. Current focus (where we are in the tree)
 * 2. Path history (how we got here)
 * 3. Ability to navigate up/down/left/right
 * 
 * This eliminates closure issues by making navigation pure and explicit.
 */

// ═══════════════════════════════════════════════════════════════════
// TREE NODE TYPES
// ═══════════════════════════════════════════════════════════════════

export type TreeNodeType = 
	| 'input'        // Single value input (text, number, etc)
	| 'select'       // Single choice from options
	| 'multi-select' // Multiple choices from options
	| 'custom'       // Custom component
	| 'branch';      // Decision point (dynamic children)

export interface TreeNode<TState = any> {
	id: string;
	type: TreeNodeType;
	label?: string;
	
	// Component configuration
	component?: React.ComponentType<any>;
	componentProps?: (state: TState, path: string[]) => any;
	
	// Input-specific
	inputType?: 'text' | 'number' | 'time';
	placeholder?: string;
	defaultValue?: (state: TState) => string | undefined;
	validate?: (value: string, state: TState) => boolean | string;
	
	// Select-specific
	options?: Array<{ value: string; label: string }> | ((state: TState) => Array<{ value: string; label: string }>);
	
	// Multi-select specific
	allowEmpty?: boolean;
	
	// Children (can be static or dynamic based on state)
	children?: TreeNode<TState>[] | ((state: TState, path: string[]) => TreeNode<TState>[]);
	
	// Lifecycle hooks
	onEnter?: (state: TState) => TState;  // Called when entering this node
	onExit?: (state: TState, value: any) => TState;  // Called when leaving (with value)
	
	// Navigation hints
	skipIfEmpty?: boolean;  // Auto-skip if no value needed
	autoAdvance?: boolean;  // Auto-advance after value submission
}

// ═══════════════════════════════════════════════════════════════════
// ZIPPER STATE
// ═══════════════════════════════════════════════════════════════════

export interface ZipperState<TState = any> {
	// Current position
	path: string[];  // Array of node IDs leading to current node
	
	// Collected data
	data: TState;
	
	// Navigation state
	cursor: number;  // For multi-select/option lists
}

// ═══════════════════════════════════════════════════════════════════
// ZIPPER NAVIGATOR
// ═══════════════════════════════════════════════════════════════════

export interface TreeZipperProps<TState = any> {
	tree: TreeNode<TState>;
	initialState: TState;
	onComplete: (state: TState) => void;
	onCancel: () => void;
}

export function TreeZipper<TState = any>({
	tree,
	initialState,
	onComplete,
	onCancel
}: TreeZipperProps<TState>) {
	const [zipperState, setZipperState] = useState<ZipperState<TState>>({
		path: [tree.id],
		data: initialState,
		cursor: 0
	});

	// Get current node
	const getCurrentNode = (): TreeNode<TState> | null => {
		let node: TreeNode<TState> | null = tree;
		
		for (let i = 1; i < zipperState.path.length; i++) {
			const nodeId = zipperState.path[i];
			if (!node) return null;
			
			const children = typeof node.children === 'function'
				? node.children(zipperState.data, zipperState.path)
				: node.children;
			
			if (!children) return null;
			node = children.find(c => c.id === nodeId) || null;
		}
		
		return node;
	};

	const currentNode = getCurrentNode();

	// Navigation: Move to next node
	const goForward = () => {
		if (!currentNode) return;
		
		// Check if current node has children
		const children = typeof currentNode.children === 'function'
			? currentNode.children(zipperState.data, zipperState.path)
			: currentNode.children;
		
		if (children && children.length > 0) {
			// Move into first child
			const firstChild = children[0];
			
			// Call onEnter if defined
			const newData = firstChild.onEnter ? firstChild.onEnter(zipperState.data) : zipperState.data;
			
			setZipperState(prev => ({
				...prev,
				path: [...prev.path, firstChild.id],
				cursor: 0,
				data: newData
			}));
			
			// If first child is a branch, auto-advance through it
			if (firstChild.type === 'branch') {
				// Use setTimeout to avoid state update during render
				setTimeout(() => goForward(), 0);
			}
		} else {
			// No children (leaf node or empty branch) - go to next sibling
			goToNextSibling();
		}
	};

	// Navigation: Move to previous node
	const goBack = () => {
		if (zipperState.path.length <= 1) {
			// At root - cancel
			onCancel();
			return;
		}
		
		// Pop current node from path
		setZipperState(prev => ({
			...prev,
			path: prev.path.slice(0, -1),
			cursor: 0
		}));
	};

	// Navigation: Move to next sibling
	const goToNextSibling = () => {
		if (zipperState.path.length <= 1) {
			// At root - complete
			onComplete(zipperState.data);
			return;
		}
		
		// Get parent node
		const parentPath = zipperState.path.slice(0, -1);
		let parentNode: TreeNode<TState> | null = tree;
		
		for (let i = 1; i < parentPath.length; i++) {
			const nodeId = parentPath[i];
			if (!parentNode) return;
			
			const children = typeof parentNode.children === 'function'
				? parentNode.children(zipperState.data, parentPath.slice(0, i + 1))
				: parentNode.children;
			
			if (!children) return;
			parentNode = children.find(c => c.id === nodeId) || null;
		}
		
		if (!parentNode) return;
		
		const siblings = typeof parentNode.children === 'function'
			? parentNode.children(zipperState.data, parentPath)
			: parentNode.children;
		
		if (!siblings) return;
		
		const currentId = zipperState.path[zipperState.path.length - 1];
		const currentIndex = siblings.findIndex(s => s.id === currentId);
		
		if (currentIndex < siblings.length - 1) {
			// Move to next sibling
			const nextSibling = siblings[currentIndex + 1];
			setZipperState(prev => ({
				...prev,
				path: [...parentPath, nextSibling.id],
				cursor: 0
			}));
			
			if (nextSibling.onEnter) {
				setZipperState(prev => ({
					...prev,
					data: nextSibling.onEnter!(prev.data)
				}));
			}
		} else {
			// No more siblings - go up to parent's sibling
			setZipperState(prev => ({
				...prev,
				path: parentPath,
				cursor: 0
			}));
			goToNextSibling();
		}
	};

	// Handle value submission
	const handleSubmit = (value: any) => {
		if (!currentNode) return;
		
		// Update state with value
		let newData = zipperState.data;
		if (currentNode.onExit) {
			newData = currentNode.onExit(zipperState.data, value);
		}
		
		setZipperState(prev => ({
			...prev,
			data: newData
		}));
		
		// Auto-advance if configured
		if (currentNode.autoAdvance !== false) {
			goForward();
		}
	};

	// Global input handler
	useInput((input, key) => {
		if (key.leftArrow) {
			goBack();
			return;
		} else if (key.rightArrow) {
			goForward();
			return;
		} else if (key.escape) {
			onCancel();
			return;
		}
	});

	// Render current node
	if (!currentNode) {
		return (
			<Box>
				<Text color="red">Error: Node not found</Text>
			</Box>
		);
	}

	// Breadcrumb
	const breadcrumb = zipperState.path.join(' → ');

	return (
		<Box flexDirection="column">
			<Box marginBottom={1}>
				<Text dimColor>{breadcrumb}</Text>
			</Box>
			<NodeRenderer
				node={currentNode}
				state={zipperState.data}
				path={zipperState.path}
				cursor={zipperState.cursor}
				onSubmit={handleSubmit}
				onCancel={onCancel}
				onCursorChange={(newCursor) => setZipperState(prev => ({ ...prev, cursor: newCursor }))}
			/>
			<Box marginTop={1}>
				<Text dimColor>← Back | → Forward | Esc Cancel</Text>
			</Box>
		</Box>
	);
}

// ═══════════════════════════════════════════════════════════════════
// NODE RENDERER
// ═══════════════════════════════════════════════════════════════════

interface NodeRendererProps<TState = any> {
	node: TreeNode<TState>;
	state: TState;
	path: string[];
	cursor: number;
	onSubmit: (value: any) => void;
	onCancel: () => void;
	onCursorChange: (cursor: number) => void;
}

function NodeRenderer<TState = any>({
	node,
	state,
	path,
	cursor,
	onSubmit,
	onCancel,
	onCursorChange
}: NodeRendererProps<TState>) {
	// Branch nodes don't render - they just organize structure
	if (node.type === 'branch') {
		return (
			<Box>
				<Text dimColor>Navigating...</Text>
			</Box>
		);
	}
	
	// Custom component
	if (node.type === 'custom' && node.component) {
		const Component = node.component;
		const props = node.componentProps ? node.componentProps(state, path) : {};
		return <Component {...props} onSubmit={onSubmit} onCancel={onCancel} />;
	}

	// Input node
	if (node.type === 'input') {
		return (
			<InputNode
				node={node}
				state={state}
				onSubmit={onSubmit}
				onCancel={onCancel}
			/>
		);
	}

	// Select node
	if (node.type === 'select') {
		return (
			<SelectNode
				node={node}
				state={state}
				cursor={cursor}
				onSubmit={onSubmit}
				onCursorChange={onCursorChange}
			/>
		);
	}

	// Multi-select node
	if (node.type === 'multi-select') {
		return (
			<MultiSelectNode
				node={node}
				state={state}
				cursor={cursor}
				onSubmit={onSubmit}
				onCursorChange={onCursorChange}
			/>
		);
	}

	return <Text>Unknown node type: {node.type}</Text>;
}

// ═══════════════════════════════════════════════════════════════════
// NODE TYPE COMPONENTS (separate to avoid Hooks violations)
// ═══════════════════════════════════════════════════════════════════

interface InputNodeProps<TState = any> {
	node: TreeNode<TState>;
	state: TState;
	onSubmit: (value: string) => void;
	onCancel: () => void;
}

function InputNode<TState = any>({ node, state, onSubmit, onCancel }: InputNodeProps<TState>) {
	const defaultVal = node.defaultValue ? node.defaultValue(state) : '';
	const [value, setValue] = useState(defaultVal || '');
	const [error, setError] = useState<string | null>(null);
	
	useInput((input, key) => {
		if (key.return) {
			if (node.validate) {
				const result = node.validate(value, state);
				if (result !== true) {
					setError(typeof result === 'string' ? result : 'Invalid input');
					return;
				}
			}
			setError(null);
			onSubmit(value);
		} else if (key.escape) {
			onCancel();
		} else if (key.backspace || key.delete) {
			setValue(value.slice(0, -1));
			setError(null);
		} else if (!key.ctrl && !key.meta && input) {
			setValue(value + input);
			setError(null);
		}
	});
	
	return (
		<Box flexDirection="column">
			{node.label && <Text bold>{node.label}</Text>}
			<Box marginTop={1}>
				<Text>
					{value || <Text dimColor>{node.placeholder}</Text>}
					<Text dimColor>█</Text>
				</Text>
			</Box>
			{error && (
				<Box marginTop={1}>
					<Text color="red">⚠ {error}</Text>
				</Box>
			)}
		</Box>
	);
}

interface SelectNodeProps<TState = any> {
	node: TreeNode<TState>;
	state: TState;
	cursor: number;
	onSubmit: (value: string) => void;
	onCursorChange: (cursor: number) => void;
}

function SelectNode<TState = any>({ node, state, cursor, onSubmit, onCursorChange }: SelectNodeProps<TState>) {
	const opts = typeof node.options === 'function' ? node.options(state) : node.options || [];
	
	useInput((input, key) => {
		if (key.upArrow) {
			onCursorChange(Math.max(0, cursor - 1));
		} else if (key.downArrow) {
			onCursorChange(Math.min(opts.length - 1, cursor + 1));
		} else if (key.return) {
			onSubmit(opts[cursor].value);
		}
	});
	
	return (
		<Box flexDirection="column">
			{node.label && <Text bold>{node.label}</Text>}
			<Box flexDirection="column" marginTop={1}>
				{opts.map((opt, idx) => (
					<Text key={opt.value} color={idx === cursor ? 'cyan' : undefined}>
						{idx === cursor ? '▶ ' : '  '}
						{opt.label}
					</Text>
				))}
			</Box>
		</Box>
	);
}

interface MultiSelectNodeProps<TState = any> {
	node: TreeNode<TState>;
	state: TState;
	cursor: number;
	onSubmit: (value: string[]) => void;
	onCursorChange: (cursor: number) => void;
}

function MultiSelectNode<TState = any>({ node, state, cursor, onSubmit, onCursorChange }: MultiSelectNodeProps<TState>) {
	const opts = typeof node.options === 'function' ? node.options(state) : node.options || [];
	const [selected, setSelected] = useState<string[]>([]);
	
	useInput((input, key) => {
		if (key.upArrow) {
			onCursorChange(Math.max(0, cursor - 1));
		} else if (key.downArrow) {
			onCursorChange(Math.min(opts.length - 1, cursor + 1));
		} else if (input === ' ') {
			const val = opts[cursor].value;
			setSelected(prev => 
				prev.includes(val) ? prev.filter(v => v !== val) : [...prev, val]
			);
		} else if (key.return) {
			onSubmit(selected);
		}
	});
	
	return (
		<Box flexDirection="column">
			{node.label && <Text bold>{node.label}</Text>}
			<Box flexDirection="column" marginTop={1}>
				{opts.map((opt, idx) => {
					const isSelected = selected.includes(opt.value);
					return (
						<Text key={opt.value} color={idx === cursor ? 'cyan' : undefined}>
							{idx === cursor ? '▶ ' : '  '}
							{isSelected ? '[✓] ' : '[ ] '}
							{opt.label}
						</Text>
					);
				})}
			</Box>
			<Box marginTop={1}>
				<Text dimColor>Selected: {selected.length > 0 ? selected.join(', ') : 'none'}</Text>
			</Box>
		</Box>
	);
}

