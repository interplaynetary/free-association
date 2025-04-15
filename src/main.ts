// Import global styles
import './global.css';

import './app.css'
import App from './App.svelte'
import { mount } from 'svelte'
//import { initConnectionManager } from './utils/gun/connectionManager';

// Initialize Gun connection manager at the application level
//initConnectionManager();

// Use mount() for Svelte 5 instead of new App()
const app = mount(App, {
  target: document.getElementById('app')!,
})

export default app
