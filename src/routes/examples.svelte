<script>
  import { onMount } from 'svelte';
  import { writable } from 'svelte/store';
  import { gun, recallUser } from '../utils/gun/gunSetup';
  
  // List of available examples
  const examples = [
    { id: 'counter', name: 'Counter Example', description: 'Simple counter with GUN' },
    { id: 'todo', name: 'Todo App', description: 'Todo list with GUN persistence' },
    { id: 'chat', name: 'Chat Room', description: 'Real-time chat between users' },
    { id: 'graph', name: 'Graph Visualization', description: 'Visualize GUN data as a graph' },
    { id: 'transient', name: 'Transient Storage', description: 'Compare transient vs persistent storage' },
  ];
  
  onMount(async () => {
    await recallUser();
  });
  
  let selectedExample = writable(null);
  
  function selectExample(id) {
    selectedExample.set(id);
  }
</script>

<div class="examples-page">
  <h1>Gun Examples</h1>
  
  <div class="examples-container">
    <div class="examples-list">
      <h2>Available Examples</h2>
      <ul>
        {#each examples as example}
          <li>
            <a 
              href="/examples/{example.id}" 
              class="example-link"
            >
              <h3>{example.name}</h3>
              <p>{example.description}</p>
            </a>
          </li>
        {/each}
      </ul>
    </div>
  </div>
</div>

<style>
  .examples-page {
    max-width: 1200px;
    margin: 0 auto;
    padding: 2rem;
  }
  
  h1 {
    text-align: center;
    margin-bottom: 2rem;
  }
  
  .examples-container {
    display: flex;
    gap: 2rem;
  }
  
  .examples-list {
    flex: 1;
  }
  
  ul {
    list-style: none;
    padding: 0;
  }
  
  li {
    margin-bottom: 1rem;
  }
  
  .example-link {
    display: block;
    padding: 1rem;
    background-color: #f5f5f5;
    border-radius: 0.5rem;
    text-decoration: none;
    color: inherit;
    transition: transform 0.2s, box-shadow 0.2s;
  }
  
  .example-link:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    background-color: #e0e0e0;
  }
  
  h3 {
    margin-top: 0;
    margin-bottom: 0.5rem;
  }
  
  p {
    margin: 0;
    color: #666;
  }
</style> 