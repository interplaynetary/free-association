<script lang="ts">
  import { onMount } from 'svelte';
  import { browser } from '$app/environment';
  import { evmAddress, evmChainId, isWalletConnected, connectEvmWallet, disconnectEvmWallet } from '$lib/state/wallet.svelte';

  let addr = $state('');
  let chain = $state<number | null>(null);
  let connected = $state(false);

  $effect(() => { addr = $evmAddress; });
  $effect(() => { chain = $evmChainId; });
  $effect(() => { connected = $isWalletConnected; });

  function shortAddress(a: string) {
    if (!a) return '';
    return a.slice(0, 6) + '…' + a.slice(-4);
  }

  let error = $state<string>('');

  async function handleConnect() {
    error = '';
    try { await connectEvmWallet(); } catch (e: any) { error = e?.message || 'Failed to connect'; }
  }
  async function handleDisconnect() {
    error = '';
    try { await disconnectEvmWallet(); } catch (e: any) { error = e?.message || 'Failed to disconnect'; }
  }
  onMount(() => {
    if (!browser) return;
  });
</script>

<div class="wallet-connect">
  {#if connected}
    <div class="status">
      <span class="dot online"></span>
      <span>{shortAddress(addr)}{chain !== null ? ` · chain ${chain}` : ''}</span>
    </div>
    <button class="btn" onclick={handleDisconnect}>Disconnect</button>
  {:else}
    <button class="btn primary" onclick={handleConnect}>Connect Wallet</button>
  {/if}
  {#if error}
    <div class="error">{error}</div>
  {/if}
</div>

<style>
  .wallet-connect { display: flex; align-items: center; gap: .5rem; }
  .status { display: flex; align-items: center; gap: .35rem; font-size: .9rem; }
  .dot { width: .5rem; height: .5rem; border-radius: 50%; display: inline-block; }
  .dot.online { background: #10b981; }
  .btn { padding: .35rem .6rem; border-radius: .4rem; border: 1px solid #e5e7eb; cursor: pointer; }
  .btn.primary { background: #111827; color: white; border-color: #111827; }
  .error { color: #ef4444; font-size: .85rem; }
</style>
