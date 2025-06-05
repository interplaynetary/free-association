const relay = require('@gun-vue/relay').default;

relay.init({
	host: 'localhost', // Domain or IP address where your relay is hosted (no 'http://')
	port: 8765, // The port your relay will listen on (e.g., 8080)
	store: true, // Use 'true' to enable persistent storage on disk
	path: 'public', // Folder to serve static files (e.g., your front-end build)
	showQr: false // Render a QR Code Link to the server
});

/*
relay.init({
	host: "localhost", // Domain or IP address where your relay is hosted (no 'http://')
	port: 8765, // The port your relay will listen on (e.g., 8080)
	store: false, // Use 'true' to enable persistent storage on disk
	path: "public", // Folder to serve static files (e.g., your front-end build)
	showQr: "false", // Render a QR Code Link to the server
});
*/
