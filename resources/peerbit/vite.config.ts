import { sveltekit } from "@sveltejs/kit/vite";
import { defineConfig } from "vite";
import peerbit from "@peerbit/vite";
export default defineConfig({
    plugins: [sveltekit(), peerbit()],

    optimizeDeps: {
        esbuildOptions: {
            target: "esnext", // allow top-level await
        },
    },
    build: {
        target: "esnext", // allow top-level await
    },

    // add static assets to Vite serving allow list
    server: {
        fs: {
            allow: ["static"],
        },
    },
});
