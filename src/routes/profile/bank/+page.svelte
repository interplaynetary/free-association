<script lang="ts">
  import { browser } from '$app/environment';
  import { onMount } from 'svelte';
  import { USE_HOLSTER_AUTH } from '$lib/config';
  import {
    ownBankDetails,
    isSavingBankDetails,
    bankError,
    incomingBankRequests,
    saveOwnBankDetails,
    approveBankAccess,
    rejectBankAccess
  } from '$lib/state/bank.svelte';
  import type { BankDetails } from '$lib/schema';

  let details: BankDetails | null = null;
  let saving = false;
  let error = '';
  let holderName = '';
  let iban = '';
  let bic = '';
  let bankName = '';
  let country = '';

  $effect(() => { details = $ownBankDetails; });
  $effect(() => { saving = $isSavingBankDetails; });
  $effect(() => { error = $bankError; });

  onMount(() => {
    if (!browser) return;
    if (details) {
      holderName = details.holderName;
      iban = details.iban;
      bic = details.bic || '';
      bankName = details.bankName || '';
      country = details.country || '';
    }
  });

  async function save() {
    try {
      await saveOwnBankDetails({ holderName, iban, bic: bic || undefined, bankName: bankName || undefined, country: country || undefined });
    } catch {}
  }

  async function approve(reqPub: string, reqEpub?: string) {
    try { await approveBankAccess(reqPub, reqEpub); } catch (e) { console.error(e); }
  }
  async function reject(reqPub: string) {
    try { await rejectBankAccess(reqPub); } catch (e) { console.error(e); }
  }
</script>

<section class="page">
  <h2>Bank Details</h2>
  {#if !USE_HOLSTER_AUTH}
    <p class="note">Enable Holster auth to store encrypted bank details.</p>
    <button class="btn primary" onclick={() => { if (browser) { localStorage.setItem('USE_HOLSTER_AUTH','true'); location.reload(); } }}>Enable Holster Auth</button>
  {:else}
    <div class="form">
      <div class="row">
        <label for="holderName">Account holder</label>
        <input id="holderName" bind:value={holderName} placeholder="Full name" />
      </div>
      <div class="row">
        <label for="iban">IBAN</label>
        <input id="iban" bind:value={iban} placeholder="DE89 3704 0044 0532 0130 00" />
      </div>
      <div class="row">
        <label for="bic">BIC/SWIFT (optional)</label>
        <input id="bic" bind:value={bic} placeholder="DEUTDEFF" />
      </div>
      <div class="row">
        <label for="bankName">Bank name (optional)</label>
        <input id="bankName" bind:value={bankName} placeholder="Your Bank" />
      </div>
      <div class="row">
        <label for="country">Country (2-letter, optional)</label>
        <input id="country" bind:value={country} placeholder="DE" />
      </div>

      <button class="btn primary" onclick={save} disabled={saving}>
        {saving ? 'Saving…' : 'Save Encrypted'}
      </button>
      {#if error}
        <div class="error">{error}</div>
      {/if}

      {#if details}
        <div class="saved">Saved {new Date(details.updatedAt).toLocaleString()}</div>
      {/if}
    </div>

    <h3>Incoming Access Requests</h3>
    {#if Object.keys($incomingBankRequests).length === 0}
      <div class="note">No incoming requests.</div>
    {:else}
      <div class="requests">
        {#each Object.entries($incomingBankRequests) as [pub, req]}
          <div class="request">
            <div class="meta">
              <div class="who">{req.requesterAlias || pub.slice(0,8)+'…'}</div>
              <div class="status {req.status}">{req.status}</div>
            </div>
            {#if req.message}<div class="msg">{req.message}</div>{/if}
            <div class="actions">
              <button class="btn" onclick={() => approve(pub, req.requesterEpub)} disabled={req.status !== 'pending'}>Approve</button>
              <button class="btn" onclick={() => reject(pub)} disabled={req.status !== 'pending'}>Reject</button>
            </div>
          </div>
        {/each}
      </div>
    {/if}
  {/if}
</section>

<style>
  .page { padding: 1rem; display: grid; gap: 1rem; }
  h2 { font-size: 1.25rem; font-weight: 600; }
  .form { display: grid; gap: .6rem; max-width: 520px; }
  .row { display: grid; gap: .35rem; }
  label { font-size: .9rem; color: #374151; }
  input { border: 1px solid #e5e7eb; border-radius: .4rem; padding: .5rem .6rem; }
  .btn { padding: .4rem .7rem; border-radius: .4rem; border: 1px solid #e5e7eb; cursor: pointer; }
  .btn.primary { background: #111827; color: white; border-color: #111827; }
  .error { color: #ef4444; }
  .saved { color: #10b981; font-size: .9rem; }
  .note { color: #6b7280; }
  .requests { display: grid; gap: .5rem; }
  .request { border: 1px solid #e5e7eb; border-radius: .6rem; padding: .6rem; display: grid; gap: .35rem; }
  .request .meta { display: flex; align-items: center; justify-content: space-between; }
  .request .status { font-size: .85rem; text-transform: capitalize; }
  .request .status.pending { color: #f59e0b; }
  .request .status.approved { color: #10b981; }
  .request .status.rejected { color: #ef4444; }
  .actions { display: flex; gap: .5rem; }
</style>
