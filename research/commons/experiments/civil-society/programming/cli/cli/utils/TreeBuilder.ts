import type { TreeNode } from './TreeZipper';

/**
 * Elegant Tree Builder - Fluent API for building wizard trees
 * 
 * Examples:
 * 
 * // Simple field with auto-state-update
 * input('quantity').path('quantity').label('How many?')
 * 
 * // Select with auto-state-update
 * select('type').path('type').options([...])
 * 
 * // Conditional branch
 * when(s => s.recurrence).then([...nodes])
 * 
 * // From Zod schema
 * fromSchema(CapacitySchema)
 */

// ═══════════════════════════════════════════════════════════════════
// HELPER: Path-based state updates (no manual spreading!)
// ═══════════════════════════════════════════════════════════════════

function setPath<T>(obj: T, path: string, value: any): T {
	const keys = path.split('.');
	if (keys.length === 1) {
		return { ...obj, [path]: value };
	}
	
	const [first, ...rest] = keys;
	const objRecord = obj as Record<string, any>;
	return {
		...obj,
		[first]: setPath(objRecord[first] || {}, rest.join('.'), value)
	};
}

function getPath<T>(obj: T, path: string): any {
	const keys = path.split('.');
	return keys.reduce((acc, key) => acc?.[key], obj as Record<string, any>);
}

// ═══════════════════════════════════════════════════════════════════
// FLUENT BUILDERS
// ═══════════════════════════════════════════════════════════════════

class NodeBuilder<TState = any> {
	protected node: Partial<TreeNode<TState>>;
	
	constructor(id: string, type: TreeNode<TState>['type']) {
		this.node = { id, type };
	}
	
	label(text: string) {
		this.node.label = text;
		return this;
	}
	
	/** Auto-update state at path (e.g., 'quantity' or 'availability.startTime') */
	path(statePath: string) {
		this.node.onExit = (state, value) => setPath(state, statePath, value);
		return this;
	}
	
	/** Custom state update function */
	update(fn: (state: TState, value: any) => TState) {
		this.node.onExit = fn;
		return this;
	}
	
	/** Enter hook */
	onEnter(fn: (state: TState) => TState) {
		this.node.onEnter = fn;
		return this;
	}
	
	/** Skip this node based on condition */
	when(condition: (state: TState) => boolean) {
		const originalNode = this.node;
		// Wrap in a branch that conditionally includes this node
		return branch(`${this.node.id}-conditional` as string)
			.children(state => condition(state) ? [originalNode as TreeNode<TState>] : []);
	}
	
	build(): TreeNode<TState> {
		return this.node as TreeNode<TState>;
	}
}

class InputBuilder<TState = any> extends NodeBuilder<TState> {
	constructor(id: string) {
		super(id, 'input');
	}
	
	type(inputType: 'text' | 'number' | 'time') {
		this.node.inputType = inputType;
		return this;
	}
	
	placeholder(text: string) {
		this.node.placeholder = text;
		return this;
	}
	
	default(value: string | ((state: TState) => string | undefined)) {
		this.node.defaultValue = typeof value === 'function' ? value : () => value;
		return this;
	}
	
	validate(fn: (value: string, state: TState) => boolean | string) {
		this.node.validate = fn;
		return this;
	}
	
	required() {
		this.node.validate = (value) => value.length > 0 || 'This field is required';
		return this;
	}
	
	number(min?: number, max?: number) {
		this.node.inputType = 'number';
		this.node.validate = (value) => {
			const num = parseFloat(value);
			if (isNaN(num)) return 'Please enter a valid number';
			if (min !== undefined && num < min) return `Must be at least ${min}`;
			if (max !== undefined && num > max) return `Must be at most ${max}`;
			return true;
		};
		return this;
	}
}

class SelectBuilder<TState = any> extends NodeBuilder<TState> {
	constructor(id: string) {
		super(id, 'select');
	}
	
	options(opts: Array<{ value: string; label: string }> | ((state: TState) => Array<{ value: string; label: string }>)) {
		this.node.options = opts;
		return this;
	}
}

class MultiSelectBuilder<TState = any> extends NodeBuilder<TState> {
	constructor(id: string) {
		super(id, 'multi-select');
	}
	
	options(opts: Array<{ value: string; label: string }> | ((state: TState) => Array<{ value: string; label: string }>)) {
		this.node.options = opts;
		return this;
	}
	
	allowEmpty() {
		this.node.allowEmpty = true;
		return this;
	}
}

class BranchBuilder<TState = any> extends NodeBuilder<TState> {
	constructor(id: string) {
		super(id, 'branch');
	}
	
	children(nodes: TreeNode<TState>[] | ((state: TState) => TreeNode<TState>[])) {
		this.node.children = nodes;
		return this;
	}
}

class CustomBuilder<TState = any> extends NodeBuilder<TState> {
	constructor(id: string) {
		super(id, 'custom');
	}
	
	component(Component: React.ComponentType<any>) {
		this.node.component = Component;
		return this;
	}
	
	props(fn: (state: TState, path: string[]) => any) {
		this.node.componentProps = fn;
		return this;
	}
}

// ═══════════════════════════════════════════════════════════════════
// FACTORY FUNCTIONS (elegant syntax)
// ═══════════════════════════════════════════════════════════════════

export function input<TState = any>(id: string) {
	return new InputBuilder<TState>(id);
}

export function select<TState = any>(id: string) {
	return new SelectBuilder<TState>(id);
}

export function multiSelect<TState = any>(id: string) {
	return new MultiSelectBuilder<TState>(id);
}

export function branch<TState = any>(id: string) {
	return new BranchBuilder<TState>(id);
}

export function custom<TState = any>(id: string) {
	return new CustomBuilder<TState>(id);
}

// ═══════════════════════════════════════════════════════════════════
// CONDITIONAL HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Conditional branch - only include children if condition is true
 * 
 * Example:
 *   when(s => s.recurrence !== null).then([
 *     custom('schedule').component(TimeScheduleBuilder)
 *   ])
 */
export function when<TState = any>(condition: (state: TState) => boolean) {
	return {
		then(nodes: TreeNode<TState>[]) {
			return branch<TState>(`when-${Math.random().toString(36).substring(7)}`)
				.children(state => condition(state) ? nodes : [])
				.build();
		}
	};
}

// ═══════════════════════════════════════════════════════════════════
// COMPOSITION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a wizard from a list of nodes
 */
export function wizard<TState = any>(id: string, nodes: TreeNode<TState>[]) {
	return branch<TState>(id).children(nodes).build();
}

/**
 * Repeat a pattern for each item in an array
 * 
 * Example:
 *   forEach(s => s.selectedDays).map(day => [
 *     input(`${day}-start`).path(`times.${day}.start`),
 *     input(`${day}-end`).path(`times.${day}.end`)
 *   ])
 */
export function forEach<TState = any, TItem = any>(
	getArray: (state: TState) => TItem[]
) {
	return {
		map(fn: (item: TItem, index: number) => TreeNode<TState>[]) {
			return branch<TState>(`forEach-${Math.random().toString(36).substring(7)}`)
				.children(state => {
					const items = getArray(state);
					return items.flatMap((item, index) => fn(item, index));
				})
				.build();
		}
	};
}

// ═══════════════════════════════════════════════════════════════════
// SCHEMA-DRIVEN GENERATION (Future: auto-generate from Zod)
// ═══════════════════════════════════════════════════════════════════

/**
 * Generate tree nodes from Zod schema (future enhancement)
 * 
 * Example:
 *   fromSchema(CapacitySchema).fields(['quantity', 'unit', 'recurrence'])
 */
export function fromSchema<TState = any>(schema: any) {
	// TODO: Introspect Zod schema and auto-generate nodes
	return {
		fields(fieldNames: string[]): TreeNode<TState>[] {
			// Future: auto-generate based on schema types
			return [];
		}
	};
}


