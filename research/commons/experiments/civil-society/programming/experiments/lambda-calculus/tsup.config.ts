import { defineConfig } from 'tsup';

export default defineConfig({
  entry: {
    index: 'index.ts',
    'elegant/index': 'src/elegant/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: true,
  splitting: false,
  sourcemap: true,
  clean: true,
  treeshake: true,
  minify: false, // Set to true for production builds
  target: 'es2020',
  outDir: 'dist',
  external: ['zod'],
});

