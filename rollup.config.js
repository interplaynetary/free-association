export default {
	plugins: [
		resolve({
			browser: true,
			dedupe: (importee) => importee === 'svelte' || importee.startsWith('svelte/'),
			preferBuiltins: false
		})
	]
};
