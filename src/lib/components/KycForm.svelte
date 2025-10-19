<script lang="ts">
  import { kycStatus } from '$lib/state/kyc.svelte';
  import { browser } from '$app/environment';
  import { userPub as unifiedUserPub, userAlias as unifiedUserAlias } from '$lib/state/auth.svelte';

  let diditLoading = $state(false);
  let diditError = $state('');

  async function startDidit() {
    diditError = '';
    diditLoading = true;
    try {
      const externalUserId = $unifiedUserPub || $unifiedUserAlias || 'anonymous';
      const r = await fetch('/api/kyc', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ externalUserId })
      });
      const j = await r.json();
      if (!r.ok) throw new Error(j?.error || 'Failed');
      if (browser) window.location.href = j.url;
    } catch (e: any) {
      diditError = e?.message || 'Start failed';
    } finally {
      diditLoading = false;
    }
  }

  const status = $derived($kycStatus);

</script>

<div class="kyc-form">
  <h3>KYC Verification</h3>
  <p class="status">Status: <strong class:verified={status==='verified'} class:pending={status==='pending'} class:rejected={status==='rejected'}>{status}</strong></p>

  <div class="providers">
    <div class="provider">
      <button class="btn primary" onclick={startDidit} disabled={diditLoading}>
        {diditLoading ? 'Starting…' : 'Verify with Didit'}
      </button>
      {#if diditError}<div class="error">{diditError}</div>{/if}
    </div>
  </div>

  {#if status === 'verified'}
    <div class="success">You are verified. Thank you!</div>
  {/if}
</div>

<style>
  .kyc-form { max-width: 480px; display: grid; gap: .75rem; }
  .providers { display: grid; gap: .5rem; }
  .provider { padding: .35rem 0; }
  .btn.primary { background: #111827; color: white; border: 1px solid #111827; padding: .5rem .9rem; border-radius: .45rem; }
  .error { color: #ef4444; }
  .row { display: grid; gap: .35rem; }
  .row.check { display: flex; align-items: center; }
  label { font-size: .9rem; color: #374151; }
  input { border: 1px solid #e5e7eb; border-radius: .4rem; padding: .5rem .6rem; }
  .btn.primary { background: #111827; color: white; border: 1px solid #111827; padding: .5rem .9rem; border-radius: .45rem; }
  .error { color: #ef4444; }
  .success { color: #10b981; }
  .status strong.verified { color: #10b981; }
  .status strong.pending { color: #f59e0b; }
  .status strong.rejected { color: #ef4444; }
</style>
