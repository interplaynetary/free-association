// Polyfill window object for Gun in service worker context
if (typeof window === 'undefined') {
	// @ts-ignore
	globalThis.window = {
		crypto: self.crypto,
		TextEncoder: self.TextEncoder,
		TextDecoder: self.TextDecoder,
		WebSocket: self.WebSocket,
		location: self.location
	};
} 