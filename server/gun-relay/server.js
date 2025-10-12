import relay from '@gun-vue/relay';

// Configuration from environment variables with defaults
const config = {
  host: process.env.GUN_RELAY_HOST || '0.0.0.0',
  port: parseInt(process.env.GUN_RELAY_PORT) || 8765,
  store: process.env.GUN_RELAY_STORE === 'true' || true,
  path: process.env.GUN_RELAY_PATH || 'public',
  showQr: process.env.GUN_RELAY_SHOW_QR === 'true' || false
};

console.log('Starting Gun Relay Server with configuration:');
console.log(JSON.stringify(config, null, 2));

relay.init(config).then(() => {
  console.log('Gun Relay Server started successfully');
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Storage: ${config.store ? 'enabled at ' + config.path : 'disabled'}`);
}).catch((err) => {
  console.error('Failed to start Gun Relay Server');
  console.error('Error details:', {
    message: err.message,
    code: err.code,
    stack: err.stack,
    config: config
  });
  process.exit(1);
});
