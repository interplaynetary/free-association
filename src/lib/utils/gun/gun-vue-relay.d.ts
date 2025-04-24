declare module '@gun-vue/relay' {
  interface RelayOptions {
    host?: string;
    port?: number;
    store?: boolean;
    path?: string;
    showQr?: boolean;
  }

  interface Relay {
    init(options?: RelayOptions): void;
    getGun(): any;
  }

  const relay: Relay;
  export default relay;
} 