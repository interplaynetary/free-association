<script lang="ts">
    import svelteLogo from './assets/svelte.svg'
    import viteLogo from '/vite.svg'
    import ToDo from './ToDo.svelte'
    import LogIn from '../LogIn.svelte'
    import Recognition from '../Recognition.svelte'
    // import ExampleForm from './lib/examples/ExampleForm.svelte'
  </script>
  
  <main>
    <div>
      <a href="https://vite.dev" target="_blank" rel="noreferrer">
        <img src={viteLogo} class="logo" alt="Vite Logo" />
      </a>
      <a href="https://svelte.dev" target="_blank" rel="noreferrer">
        <img src={svelteLogo} class="logo svelte" alt="Svelte Logo" />
      </a>
    </div>
    <h1>Free-Association</h1>
  
    <div class="fullscreen-container">
      <Recognition path={['recognition']} height="100vh" />
    </div>
  
    <div class="card">
      <LogIn />
    </div>
  
    <div class="card">
      <ToDo />
    </div>
  
    <p>
      Built with GUN - a decentralized graph database that synchronizes in real-time.
    </p>
  
    <p class="read-the-docs">
      Click on the Vite and Svelte logos to learn more
    </p>
  </main>
  
  <style>
    .logo {
      height: 6em;
      padding: 1.5em;
      will-change: filter;
      transition: filter 300ms;
    }
    .logo:hover {
      filter: drop-shadow(0 0 2em #646cffaa);
    }
    .logo.svelte:hover {
      filter: drop-shadow(0 0 2em #ff3e00aa);
    }
    .read-the-docs {
      color: #888;
    }
    
    .card {
      padding: 2em;
      width: 100%;
      max-width: 640px;
      margin: 0 auto;
    }
  
    .fullscreen-container {
      width: 100%;
      height: 100vh;
      padding: 0;
      margin: 0;
      overflow: hidden;
    }
  </style>
  