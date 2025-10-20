<script lang="ts">
  import { onMount } from 'svelte';
  import { verifiedList, notVerifiedList, startKycDirectory, stopKycDirectory } from '$lib/state/kyc-directory.svelte';

  let verified = $derived($verifiedList);
  let unverified = $derived($notVerifiedList);

  onMount(() => {
    startKycDirectory();
    return () => stopKycDirectory();
  });

  function short(a: string) { return a ? a.slice(0,6)+'…'+a.slice(-4) : ''; }
</script>

<section class="page">
  <h2>Community</h2>
  <div class="section">
    <h3>Verified</h3>
    {#if verified.length === 0}
      <div class="empty">No verified users yet.</div>
    {:else}
      <div class="grid">
        {#each verified as u}
          <div class="card verified">
            <div class="name">{u.name}</div>
            <div class="pub">{short(u.pubkey)}</div>
            {#if u.wallet?.address}
              <div class="wallet">{u.wallet.address}</div>
            {:else}
              <div class="wallet none">No wallet on profile</div>
            {/if}
          </div>
        {/each}
      </div>
    {/if}
  </div>

  <div class="section">
    <h3>Not Verified</h3>
    {#if unverified.length === 0}
      <div class="empty">No unverified users found.</div>
    {:else}
      <div class="grid">
        {#each unverified as u}
          <div class="card unverified">
            <div class="name">{u.name}</div>
            <div class="pub">{short(u.pubkey)}</div>
            {#if u.wallet?.address}
              <div class="wallet">{u.wallet.address}</div>
            {:else}
              <div class="wallet none">No wallet on profile</div>
            {/if}
          </div>
        {/each}
      </div>
    {/if}
  </div>
</section>

<style>
  .page { padding: 1rem; display: grid; gap: 1rem; }
  .section { display: grid; gap: .5rem; }
  h2 { font-size: 1.25rem; font-weight: 600; }
  h3 { font-size: 1.05rem; font-weight: 600; margin: 0; }
  .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: .75rem; }
  .card { border: 1px solid #e5e7eb; border-radius: .6rem; padding: .75rem; display: grid; gap: .25rem; }
  .card.verified { border-color: #10b98133; background: #ecfdf5; }
  .card.unverified { border-color: #f59e0b33; background: #fffbeb; }
  .name { font-weight: 600; }
  .pub { color: #6b7280; font-size: .9rem; }
  .wallet { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace; font-size: .9rem; word-break: break-all; overflow-wrap: anywhere; }
  .wallet.none { color: #9ca3af; font-style: italic; }
  .empty { color: #6b7280; }
</style>
