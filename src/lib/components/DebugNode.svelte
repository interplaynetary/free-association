<script lang="ts">
  import { createRec, type RecognitionStore } from "../../stores/rec.svelte";
  // Use $props() instead of export let in runes mode
  let { store: initialStore } = $props<{
    store?: RecognitionStore;
  }>();

  let store = $state<RecognitionStore>(initialStore);
  let name = store.nameStore;
  let points = store.pointsStore;
  let children = store.childrenStore;
  let contributors = store.contributorsStore;
</script>

<div class="border">
  <h1>{$name} {store.path.join("/")}</h1>
  <div>
    <div>
      <h2>Points</h2>
      <p>{$points}</p>
    </div>
    <div>
      <h2>Children</h2>
      <ul class="children-list">
        {#each $children as child}
          <li>
            <svelte:self store={store.getChild(child[0])} />
          </li>
        {/each}
      </ul>
    </div>
    <div>
      <h2>Contributors</h2>
      <ul class="contributors-list">
        {#each $contributors as contributor}
          <li>
            <svelte:self store={store.getContributor(contributor[0])} />
          </li>
        {/each}
      </ul>
    </div>
  </div>
  <!-- <pre>{JSON.stringify(store, null, 2)}</pre> -->
</div>

<style>
  .border {
    border: 1px solid #9e0000;
    border-radius: 4px;
    padding: 16px;
  }
  .children-list li {
    list-style-type: none;
    padding: 0;
    margin: 0;
    margin-left: 100px;
  }
</style>
