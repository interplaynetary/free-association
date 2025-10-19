<script lang="ts">
  import KycForm from '$lib/components/KycForm.svelte';
</script>

<section class="page">
  <h2>KYC</h2>
  <p>Complete a simple self-attestation. Admins or providers can later connect third-party KYC.</p>
  <KycForm />
</section>

<style>
  .page { padding: 1rem; display: grid; gap: .75rem; }
  h2 { font-size: 1.25rem; font-weight: 600; }
</style>

