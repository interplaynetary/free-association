import './app.css';
import App from './lib/App.svelte';
import { mount } from 'svelte';

// Use mount() for Svelte 5 instead of new App()
const app = mount(App, {
	target: document.getElementById('app')!
});

export default app;
