import relay from "@gun-vue/relay";

// Initialize with the default options
relay.init({
  host: "localhost",
  port: 8765,
  store: true, // Enable persistent storage
  path: "public",
  showQR: true
});

console.log("Gun relay started. Press Ctrl+C to stop.");

// Keep the process running
process.on('SIGINT', () => {
  console.log('Gun relay shutting down...');
  process.exit(0);
}); 