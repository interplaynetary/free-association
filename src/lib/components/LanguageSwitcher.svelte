<script lang="ts">
  import { browser } from '$app/environment';

  let currentLang = browser ? (localStorage.getItem('lang') || 'en') : 'en';
  let isOpen = false;

  const languages = [
    { code: 'en', abbr: 'EN', name: 'English' },
    { code: 'pt', abbr: 'PT', name: 'Português' },
  ];

  function toggleDropdown() {
    isOpen = !isOpen;
  }

  function selectLanguage(langCode: string) {
    if (!browser) return;
    console.log('Changing language to:', langCode);
    localStorage.setItem('lang', langCode);
    currentLang = langCode;
    isOpen = false;
    window.location.reload();
  }

  function handleClickOutside(event: MouseEvent) {
    const target = event.target as HTMLElement;
    if (!target.closest('.lang-switcher')) {
      isOpen = false;
    }
  }

  $: currentLangObj = languages.find(l => l.code === currentLang) || languages[0];
</script>

<svelte:window on:click={handleClickOutside} />

<div class="lang-switcher">
  <button class="lang-button" on:click|stopPropagation={toggleDropdown} type="button">
    {currentLangObj.abbr}
    <svg width="12" height="12" viewBox="0 0 12 12" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M3 4.5L6 7.5L9 4.5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
    </svg>
  </button>
  
  {#if isOpen}
    <div class="lang-dropdown">
      {#each languages as lang}
        <button
          class="lang-option"
          class:active={lang.code === currentLang}
          on:click={() => selectLanguage(lang.code)}
          type="button"
        >
          <span class="lang-abbr">{lang.abbr}</span>
          <span class="lang-name">{lang.name}</span>
        </button>
      {/each}
    </div>
  {/if}
</div>

<style>
  .lang-switcher {
    position: relative;
    display: inline-block;
  }

  .lang-button {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 6px 12px;
    border: 1px solid rgba(0, 0, 0, 0.2);
    border-radius: 6px;
    background: white;
    color: #333;
    font-size: 14px;
    font-weight: 500;
    cursor: pointer;
    outline: none;
    transition: all 0.2s ease;
  }

  .lang-button:hover {
    border-color: rgba(0, 0, 0, 0.3);
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
  }

  .lang-button:focus {
    border-color: #2563eb;
    box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.1);
  }

  .lang-dropdown {
    position: absolute;
    top: calc(100% + 4px);
    right: 0;
    min-width: 160px;
    background: white;
    border: 1px solid rgba(0, 0, 0, 0.15);
    border-radius: 6px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
    overflow: hidden;
    z-index: 1000;
  }

  .lang-option {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    padding: 10px 12px;
    border: none;
    background: transparent;
    color: #333;
    font-size: 14px;
    cursor: pointer;
    transition: background 0.15s ease;
    text-align: left;
  }

  .lang-option:hover {
    background: rgba(0, 0, 0, 0.05);
  }

  .lang-option.active {
    background: rgba(37, 99, 235, 0.1);
    color: #2563eb;
  }

  .lang-abbr {
    font-weight: 600;
    min-width: 24px;
  }

  .lang-name {
    font-weight: 400;
  }

  @media (prefers-color-scheme: dark) {
    .lang-button {
      background: rgba(255, 255, 255, 0.1);
      border-color: rgba(255, 255, 255, 0.2);
    }

    .lang-button:hover {
      border-color: rgba(255, 255, 255, 0.3);
      background: rgba(255, 255, 255, 0.15);
    }

    .lang-button:focus {
      border-color: #60a5fa;
      box-shadow: 0 0 0 3px rgba(96, 165, 250, 0.2);
    }

    .lang-dropdown {
      background: #1f2937;
      border-color: rgba(255, 255, 255, 0.2);
    }

    .lang-option {
      color: rgba(255, 255, 255, 0.9);
    }

    .lang-option:hover {
      background: rgba(255, 255, 255, 0.1);
    }

    .lang-option.active {
      background: rgba(96, 165, 250, 0.2);
      color: #60a5fa;
    }
  }
</style>

