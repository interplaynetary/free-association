import { 
  createCollectionStore, 
  aggregateCollection,
  filterCollectionStore,
  type CollectionStore,
  type CollectionItem 
} from '../utils/svelte/reactiveStores';

// Todo item interface
export interface Todo extends CollectionItem {
  title: string;
  done: boolean;
  created: number;
}

// Extended interface for the Todo collection store
export interface TodoStore extends CollectionStore<Todo> {
  toggle: (id: string, currentState: boolean) => void;
}

/**
 * Create enhanced todo store using reactive store pattern
 */
export const todos = createCollectionStore<Todo>(
  'todos',
  // Sort by creation date (newest first)
  (a, b) => (b[1].created || 0) - (a[1].created || 0)
) as TodoStore;

// Add the toggle method
todos.toggle = (id: string, currentState: boolean) => {
  todos.update(id, { done: !currentState });
};

/**
 * Store that tracks only completed todos
 */
export const completedTodos = filterCollectionStore(
  todos,
  (todo) => todo.done === true
);

/**
 * Store that tracks only active todos
 */
export const activeTodos = filterCollectionStore(
  todos,
  (todo) => todo.done === false
);

/**
 * Store that counts completed todos
 */
export const completedCount = aggregateCollection(
  todos,
  (items) => items.filter(([_, todo]) => todo.done).length
);

/**
 * Store that counts active todos
 */
export const activeCount = aggregateCollection(
  todos,
  (items) => items.filter(([_, todo]) => !todo.done).length
);

/**
 * Store that provides todo stats
 */
export const todoStats = aggregateCollection(
  todos,
  (items) => {
    const total = items.length;
    const completed = items.filter(([_, todo]) => todo.done).length;
    const active = total - completed;
    const percentage = total > 0 ? Math.round((completed / total) * 100) : 0;
    
    return {
      total,
      completed,
      active,
      percentage
    };
  }
); 