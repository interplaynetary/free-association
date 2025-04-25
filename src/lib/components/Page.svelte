<script lang="ts">
  import { createRec } from "../../stores/rec.svelte";
  import type { RecognitionStore } from "../../types/types";
  import RawNode from "./DebugNode.svelte";

  let store = $state<RecognitionStore>(createRec());
</script>

<div>
  <h1>Page</h1>
  <RawNode {store} />
</div>
