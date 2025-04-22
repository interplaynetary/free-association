<script lang="ts">
  import { createRec, type RecognitionStore } from "../stores/rec";
  import RawNode from "./DebugNode.svelte";

  let store = $state<RecognitionStore>(createRec());
</script>

<div>
  <h1>Page</h1>
  <RawNode {store} />
</div>
