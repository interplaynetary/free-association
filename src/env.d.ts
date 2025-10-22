/// <reference types="vite/client" />

declare module '$env/static/private' {
  export const JWT_SECRET: string;
  export const JWT_EXPIRY: string;
  export const MASTER_API_KEY: string;
  export const OPENROUTER_KEYS: string;
  export const OPENROUTER_BASE_URL: string;
  export const APP_URL: string;
  export const NODE_ENV: string;
}

