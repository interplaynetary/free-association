<script lang="ts">
  import WalletConnect from '$lib/components/WalletConnect.svelte';
</script>

<section class="page">
  <h2>Wallet</h2>
  <p>Connect an EVM wallet (e.g., MetaMask) to link your address to your profile.</p>
  <WalletConnect />
</section>

<style>
  .page { padding: 1rem; display: grid; gap: .75rem; }
  h2 { font-size: 1.25rem; font-weight: 600; }
</style>

