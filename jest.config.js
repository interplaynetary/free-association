/** @type {import('ts-jest').JestConfigWithTsJest} */
export default {
	preset: 'ts-jest/presets/default-esm',
	testEnvironment: 'node',
	extensionsToTreatAsEsm: ['.ts', '.svelte.ts'],
	moduleNameMapper: {
		'^\\$lib/(.*)$': '<rootDir>/src/lib/$1',
		'^\\$app/(.*)$': '<rootDir>/.svelte-kit/runtime/app/$1'
	},
	transform: {
		'^.+\\.ts$': [
			'ts-jest',
			{
				useESM: true,
				tsconfig: {
					module: 'ESNext',
					moduleResolution: 'bundler',
					allowSyntheticDefaultImports: true,
					esModuleInterop: true
				}
			}
		]
	},
	testMatch: ['**/*.test.ts'],
	collectCoverageFrom: [
		'src/**/*.{ts,js,svelte.ts}',
		'!src/**/*.test.ts',
		'!src/**/*.spec.ts'
	],
	coverageDirectory: 'coverage',
	verbose: true
};

