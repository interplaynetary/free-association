<script lang="ts">
  import { connectionStatus, reconnectToPeers } from '../utils/gun';
</script>

<div class="connection-status" class:connected={$connectionStatus.connected}>
  {$connectionStatus.statusText}
  {#if !$connectionStatus.connected}
    <button class="reconnect-btn" on:click={reconnectToPeers}>
      Reconnect
    </button>
  {/if}
</div>

<style>
  .connection-status {
    font-size: 0.8rem;
    color: #e74c3c;
    padding: 0.4rem 0.8rem;
    margin-top: 0.5rem;
    display: flex;
    align-items: center;
    gap: 0.5rem;
    border-radius: 4px;
    background: #fef1f0;
    border: 1px solid #fadbd8;
    transition: all 0.2s ease;
  }
  
  .connection-status.connected {
    color: #27ae60;
    background: #eafaf1;
    border: 1px solid #d5f5e3;
  }
  
  .reconnect-btn {
    font-size: 0.7rem;
    padding: 0.2rem 0.5rem;
    margin: 0;
    background: #e74c3c;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
  }
  
  .reconnect-btn:hover {
    background: #c0392b;
    transform: none;
  }
</style> 