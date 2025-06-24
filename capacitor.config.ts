import type { CapacitorConfig } from '@capacitor/cli';

const config: CapacitorConfig = {
	appId: 'com.playnet.freeassociation',
	appName: 'Free Association',
	webDir: 'build',
	server: {
		androidScheme: 'https'
	},
	plugins: {
		// Configure for Gun.js networking
		CapacitorHttp: {
			enabled: true
		}
	}
};

export default config;
