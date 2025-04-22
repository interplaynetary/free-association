<script lang="ts">
  import {
    todos,
    completedCount,
    activeCount,
    todoStats,
  } from "../../stores/todo";

  let newTodo = "";

  function addTodo() {
    if (newTodo.trim()) {
      todos.add({
        title: newTodo,
        done: false,
        created: Date.now(),
      });
      newTodo = "";
    }
  }
</script>

<div class="todo-app">
  <header>
    <h1>GUN Todo App</h1>
    <p class="subtitle">With Reactive Store Pattern</p>
  </header>

  <div class="stats">
    <div class="stat-card">
      <span class="stat-value">{$todoStats?.total || 0}</span>
      <span class="stat-label">Total</span>
    </div>
    <div class="stat-card">
      <span class="stat-value">{$activeCount || 0}</span>
      <span class="stat-label">Active</span>
    </div>
    <div class="stat-card">
      <span class="stat-value">{$completedCount || 0}</span>
      <span class="stat-label">Completed</span>
    </div>
    <div class="stat-card">
      <span class="stat-value">{$todoStats?.percentage || 0}%</span>
      <span class="stat-label">Progress</span>
    </div>
  </div>

  <form on:submit|preventDefault={addTodo}>
    <input
      bind:value={newTodo}
      placeholder="What needs to be done?"
      autocomplete="off"
    />
    <button type="submit">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="16"
        height="16"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        stroke-width="2"
        stroke-linecap="round"
        stroke-linejoin="round"
      >
        <line x1="12" y1="5" x2="12" y2="19"></line>
        <line x1="5" y1="12" x2="19" y2="12"></line>
      </svg>
      Add
    </button>
  </form>

  {#if $todos?.length === 0}
    <div class="empty-state">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="48"
        height="48"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        stroke-width="1"
        stroke-linecap="round"
        stroke-linejoin="round"
      >
        <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
        ></path>
        <polyline points="14 2 14 8 20 8"></polyline>
        <line x1="16" y1="13" x2="8" y2="13"></line>
        <line x1="16" y1="17" x2="8" y2="17"></line>
        <polyline points="10 9 9 9 8 9"></polyline>
      </svg>
      <p>No todos yet. Add one above!</p>
    </div>
  {:else}
    <ul class="todo-list">
      {#each $todos as [id, todo] (id)}
        <li class:completed={todo.done}>
          <div class="todo-checkbox">
            <input
              type="checkbox"
              id={`todo-${id}`}
              checked={todo.done}
              on:change={() => todos.toggle(id, todo.done)}
            />
            <label for={`todo-${id}`}></label>
          </div>

          <span class="todo-text">{todo.title}</span>

          <div class="todo-actions">
            <button
              class="action-btn delete-btn"
              onclick={() => todos.remove(id)}
              aria-label="Delete todo"
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                width="16"
                height="16"
                viewBox="0 0 24 24"
                fill="none"
                stroke="currentColor"
                stroke-width="2"
                stroke-linecap="round"
                stroke-linejoin="round"
              >
                <polyline points="3 6 5 6 21 6"></polyline>
                <path
                  d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"
                ></path>
              </svg>
            </button>
          </div>
        </li>
      {/each}
    </ul>
  {/if}

  {#if $completedCount > 0}
    <div class="clear-completed">
      <button
        onclick={() => {
          $todos.forEach(([id, todo]) => {
            if (todo.done) todos.remove(id);
          });
        }}
      >
        Clear completed todos
      </button>
    </div>
  {/if}
</div>

<style>
  .todo-app {
    max-width: 550px;
    margin: 2rem auto;
    padding: 2rem;
    font-family:
      system-ui,
      -apple-system,
      BlinkMacSystemFont,
      "Segoe UI",
      Roboto,
      sans-serif;
    box-shadow: 0 4px 24px rgba(0, 0, 0, 0.1);
    border-radius: 12px;
    background: white;
    overflow: hidden;
  }

  header {
    margin-bottom: 1.5rem;
    text-align: center;
  }

  h1 {
    font-size: 1.8rem;
    color: #4a89dc;
    margin: 0;
    font-weight: 700;
  }

  .subtitle {
    color: #6c7a93;
    font-size: 0.9rem;
    margin: 0.3rem 0 0 0;
  }

  .stats {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 0.75rem;
    margin-bottom: 1.5rem;
  }

  .stat-card {
    background: #f8faff;
    padding: 0.75rem;
    border-radius: 8px;
    text-align: center;
    border: 1px solid #e5ebf7;
  }

  .stat-value {
    display: block;
    font-size: 1.25rem;
    font-weight: 700;
    color: #4a89dc;
  }

  .stat-label {
    display: block;
    font-size: 0.75rem;
    color: #6c7a93;
    margin-top: 0.25rem;
  }

  form {
    display: flex;
    margin-bottom: 1.5rem;
    position: relative;
  }

  input {
    flex-grow: 1;
    padding: 0.9rem 1rem;
    font-size: 1rem;
    border: 2px solid #e5ebf7;
    border-radius: 8px;
    transition: all 0.2s ease;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.04);
  }

  input:focus {
    border-color: #4a89dc;
    outline: none;
    box-shadow: 0 0 0 3px rgba(74, 137, 220, 0.2);
  }

  button {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    background: #4a89dc;
    color: white;
    border: none;
    border-radius: 8px;
    padding: 0 1.25rem;
    margin-left: 0.75rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s ease;
    white-space: nowrap;
  }

  button:hover {
    background: #3a79cc;
    transform: translateY(-1px);
  }

  button:active {
    transform: translateY(0);
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 3rem 0;
    color: #6c7a93;
    background: #f8faff;
    border-radius: 8px;
    border: 1px dashed #e5ebf7;
  }

  .empty-state svg {
    color: #c4cfe0;
    margin-bottom: 1rem;
  }

  .empty-state p {
    margin: 0;
    font-size: 0.95rem;
  }

  .todo-list {
    list-style: none;
    padding: 0;
    margin: 0;
    border-radius: 8px;
    overflow: hidden;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06);
    border: 1px solid #e5ebf7;
  }

  li {
    display: flex;
    align-items: center;
    padding: 1rem;
    transition: all 0.2s ease;
    background: white;
    position: relative;
    border-bottom: 1px solid #e5ebf7;
  }

  li:last-child {
    border-bottom: none;
  }

  li:hover {
    background: #f8faff;
  }

  .todo-checkbox {
    position: relative;
    width: 22px;
    height: 22px;
    flex-shrink: 0;
  }

  .todo-checkbox input {
    position: absolute;
    opacity: 0;
    cursor: pointer;
    height: 0;
    width: 0;
  }

  .todo-checkbox label {
    position: absolute;
    top: 0;
    left: 0;
    height: 22px;
    width: 22px;
    background-color: #fff;
    border: 2px solid #dbe3f0;
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .todo-checkbox label:after {
    content: "";
    position: absolute;
    display: none;
    left: 7px;
    top: 2px;
    width: 5px;
    height: 10px;
    border: solid white;
    border-width: 0 2px 2px 0;
    transform: rotate(45deg);
  }

  .todo-checkbox input:checked ~ label {
    background-color: #4a89dc;
    border-color: #4a89dc;
  }

  .todo-checkbox input:checked ~ label:after {
    display: block;
  }

  .todo-text {
    margin-left: 1rem;
    flex-grow: 1;
    color: #3a4a66;
    font-size: 1rem;
    word-break: break-word;
    transition: all 0.2s ease;
  }

  li.completed .todo-text {
    text-decoration: line-through;
    color: #9cabc2;
  }

  .todo-actions {
    display: flex;
    gap: 0.5rem;
    opacity: 0;
    transition: opacity 0.2s ease;
  }

  li:hover .todo-actions {
    opacity: 1;
  }

  .action-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
    padding: 0;
    margin: 0;
    border-radius: 6px;
    background: transparent;
    color: #9cabc2;
  }

  .action-btn:hover {
    background: rgba(74, 137, 220, 0.1);
    color: #4a89dc;
    transform: none;
  }

  .delete-btn:hover {
    background: rgba(255, 90, 95, 0.1);
    color: #ff5a5f;
  }

  .clear-completed {
    margin-top: 1rem;
    text-align: center;
  }

  .clear-completed button {
    background: transparent;
    color: #9cabc2;
    font-size: 0.85rem;
    padding: 0.5rem 1rem;
  }

  .clear-completed button:hover {
    color: #ff5a5f;
    background: rgba(255, 90, 95, 0.1);
  }

  @media (max-width: 600px) {
    .todo-app {
      margin: 1rem;
      padding: 1.5rem;
      border-radius: 8px;
    }

    .stats {
      grid-template-columns: repeat(2, 1fr);
    }

    .todo-actions {
      opacity: 1;
    }
  }
</style>
