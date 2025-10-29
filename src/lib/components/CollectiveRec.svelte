<script lang="ts">
  import * as d3 from 'd3';
  import { onMount } from 'svelte';

  // Membership logic aligned with $lib/examples/membership.ts semantics:
  // qualify if mutual recognition with existing members exceeds a threshold
  // for a minimum number of relationships.
  const MIN_MUTUAL_THRESHOLD = 5; // percent (membership.ts default 0.05 mapped to 0-100 scale)
  const MIN_RELATIONSHIP_COUNT = 3;

  type Recognition = { from: string; to: string; value: number };

  function buildRecognitionHelpers(recognitionData: Recognition[]) {
    const recognitionMatrix = new Map<string, Map<string, number>>();
    const participants = new Set<string>();

    for (const rec of recognitionData) {
      if (!recognitionMatrix.has(rec.from)) {
        recognitionMatrix.set(rec.from, new Map());
      }
      recognitionMatrix.get(rec.from)!.set(rec.to, rec.value);
      participants.add(rec.from);
      participants.add(rec.to);
    }

    const getRecognition = (from: string, to: string) => recognitionMatrix.get(from)?.get(to) || 0;
    const getMutual = (a: string, b: string) => (a === b ? 0 : Math.min(getRecognition(a, b), getRecognition(b, a)));

    return { recognitionMatrix, participants, getRecognition, getMutual };
  }

  // Demo scenarios
  const scenarios: Array<{
    name: string;
    week: number;
    recognition: Recognition[];
    seeds: string[];
    needs: Record<string, number>;
  }> = [
    {
      name: 'Bootstrap',
      week: 0,
      recognition: [
        { from: 'Alice', to: 'Bob', value: 25 },
        { from: 'Bob', to: 'Alice', value: 20 },
        { from: 'Alice', to: 'Charlie', value: 15 },
        { from: 'Charlie', to: 'Alice', value: 18 },
        { from: 'Bob', to: 'Charlie', value: 22 },
        { from: 'Charlie', to: 'Bob', value: 20 }
      ],
      seeds: ['Alice', 'Bob', 'Charlie'],
      needs: { Alice: 1000, Bob: 500, Charlie: 200 }
    },
    {
      name: 'Dave Building',
      week: 4,
      recognition: [
        { from: 'Alice', to: 'Bob', value: 25 },
        { from: 'Bob', to: 'Alice', value: 20 },
        { from: 'Alice', to: 'Charlie', value: 15 },
        { from: 'Charlie', to: 'Alice', value: 18 },
        { from: 'Bob', to: 'Charlie', value: 22 },
        { from: 'Charlie', to: 'Bob', value: 20 },
        { from: 'Alice', to: 'Dave', value: 12 },
        { from: 'Dave', to: 'Alice', value: 10 },
        { from: 'Bob', to: 'Dave', value: 8 },
        { from: 'Dave', to: 'Bob', value: 6 },
        { from: 'Charlie', to: 'Dave', value: 0 },
        { from: 'Dave', to: 'Charlie', value: 5 }
      ],
      seeds: ['Alice', 'Bob', 'Charlie'],
      needs: { Alice: 1000, Bob: 500, Charlie: 200, Dave: 800 }
    },
    {
      name: 'Dave Joins',
      week: 8,
      recognition: [
        { from: 'Alice', to: 'Bob', value: 25 },
        { from: 'Bob', to: 'Alice', value: 20 },
        { from: 'Alice', to: 'Charlie', value: 15 },
        { from: 'Charlie', to: 'Alice', value: 18 },
        { from: 'Bob', to: 'Charlie', value: 22 },
        { from: 'Charlie', to: 'Bob', value: 20 },
        { from: 'Alice', to: 'Dave', value: 15 },
        { from: 'Dave', to: 'Alice', value: 14 },
        { from: 'Bob', to: 'Dave', value: 10 },
        { from: 'Dave', to: 'Bob', value: 9 },
        { from: 'Charlie', to: 'Dave', value: 8 },
        { from: 'Dave', to: 'Charlie', value: 7 }
      ],
      seeds: ['Alice', 'Bob', 'Charlie'],
      needs: { Alice: 1000, Bob: 500, Charlie: 200, Dave: 800 }
    }
  ];

  let week = $state(0);
  const currentScenario = $derived(scenarios[week]);

  // Recognition helpers derived from current scenario
  $: recognitionHelpers = buildRecognitionHelpers(currentScenario.recognition);
  $: participants = Array.from(recognitionHelpers.participants);

  // Membership qualification consistent with membership.ts approach
  function qualifiesForMembershipByMatrix(candidate: string, existingMembers: string[]): boolean {
    let validRelationships = 0;
    for (const member of existingMembers) {
      const mutual = recognitionHelpers.getMutual(candidate, member);
      if (mutual >= MIN_MUTUAL_THRESHOLD) {
        validRelationships++;
      }
    }
    return validRelationships >= MIN_RELATIONSHIP_COUNT;
  }

  // Emerge membership iteratively starting from seeds
  $: members = (() => {
    const seedMembers = new Set(currentScenario.seeds);
    let current = [...seedMembers];
    let changed = true;
    let iterations = 0;
    while (changed && iterations < 5) {
      iterations++;
      changed = false;
      for (const p of participants) {
        if (!current.includes(p)) {
          if (qualifiesForMembershipByMatrix(p, current)) {
            current = [...current, p];
            changed = true;
          }
        }
      }
    }
    return current;
  })();

  // Simple metrics for UI display
  $: mrsByPerson = (() => {
    const map: Record<string, number> = {};
    for (const p of participants) {
      let score = 0;
      for (const m of members) {
        if (p !== m) score += recognitionHelpers.getMutual(p, m);
      }
      map[p] = score;
    }
    return map;
  })();
  $: networkAvgMrs = members.length
    ? members.reduce((sum, m) => sum + (mrsByPerson[m] || 0), 0) / members.length
    : 0;

  // Collective recognition allocations based on members only
  function computeCollectiveRecognitionForMembers(membersIn: string[]) {
    const memberSet = new Set(membersIn);
    const mutualFulfillment: Record<string, number> = {};
    membersIn.forEach((m) => (mutualFulfillment[m] = 0));

    for (const rec of currentScenario.recognition) {
      if (memberSet.has(rec.from) && memberSet.has(rec.to) && rec.from !== rec.to) {
        const mutual = recognitionHelpers.getMutual(rec.from, rec.to);
        if (mutual > 0) {
          mutualFulfillment[rec.from] += mutual;
        }
      }
    }

    const totalMutual = Object.values(mutualFulfillment).reduce((a, b) => a + b, 0);
    const shares: Record<string, number> = {};
    membersIn.forEach((m) => (shares[m] = totalMutual > 0 ? mutualFulfillment[m] / totalMutual : 0));

    const allocations: Record<string, { need: number; share: number; priority: number }> = {};
    membersIn.forEach((m) => {
      const need = currentScenario.needs[m] || 0;
      const share = shares[m] || 0;
      allocations[m] = { need, share, priority: share * 100 };
    });

    return { shares, allocations, mutualFulfillment, totalMutual };
  }

  $: crResult = computeCollectiveRecognitionForMembers(members);

  // Treemap rendering
  let svgEl: SVGSVGElement;
  $: treemapData = members.map((m) => ({
    name: m,
    value: crResult.allocations[m].priority,
    priority: crResult.allocations[m].priority,
    need: crResult.allocations[m].need
  }));

  function drawTreemap(width: number, height: number) {
    if (!svgEl || !treemapData || treemapData.length === 0) {
      if (svgEl) d3.select(svgEl).selectAll('*').remove();
      return;
    }

    const svg = d3.select(svgEl);
    svg.selectAll('*').remove();

    const root = d3
      .hierarchy({ children: treemapData as any })
      .sum((d: any) => d.value)
      .sort((a: any, b: any) => b.value - a.value);

    d3.treemap().size([width, height]).padding(2).round(true)(root as any);

    const colors = d3
      .scaleOrdinal<string>()
      .domain(treemapData.map((d) => d.name))
      .range(['#3b82f6', '#8b5cf6', '#ec4899', '#f59e0b']);

    const cells = svg
      .selectAll('g')
      .data((root as any).leaves())
      .join('g')
      .attr('transform', (d: any) => `translate(${d.x0},${d.y0})`);

    cells
      .append('rect')
      .attr('width', (d: any) => d.x1 - d.x0)
      .attr('height', (d: any) => d.y1 - d.y0)
      .attr('fill', (d: any) => colors(d.data.name) as string)
      .attr('opacity', 0.9)
      .attr('rx', 8);

    cells
      .append('text')
      .attr('x', (d: any) => (d.x1 - d.x0) / 2)
      .attr('y', (d: any) => (d.y1 - d.y0) / 2 - 10)
      .attr('text-anchor', 'middle')
      .attr('fill', 'white')
      .attr('font-size', '16px')
      .attr('font-weight', '600')
      .text((d: any) => d.data.name);

    cells
      .append('text')
      .attr('x', (d: any) => (d.x1 - d.x0) / 2)
      .attr('y', (d: any) => (d.y1 - d.y0) / 2 + 10)
      .attr('text-anchor', 'middle')
      .attr('fill', 'white')
      .attr('font-size', '14px')
      .attr('opacity', 0.9)
      .text((d: any) => `${(d.data.priority as number).toFixed(1)}%`);

    cells
      .append('text')
      .attr('x', (d: any) => (d.x1 - d.x0) / 2)
      .attr('y', (d: any) => (d.y1 - d.y0) / 2 + 28)
      .attr('text-anchor', 'middle')
      .attr('fill', 'white')
      .attr('font-size', '11px')
      .attr('opacity', 0.7)
      .text((d: any) => `$${d.data.need}`);
  }

  // Redraw when data changes
  $effect(() => {
    drawTreemap(520, 300);
  });
</script>

<div class="w-full min-h-screen bg-gradient-to-br from-violet-50 via-blue-50 to-cyan-50">
  <div class="max-w-7xl mx-auto px-8 py-12">
    <div class="text-center mb-12">
      <h1 class="text-5xl font-light tracking-tight text-slate-800 mb-3">Mutual Recognition Network</h1>
      <p class="text-slate-500 text-lg">Membership emerges from relationship depth · Resources flow through mutual recognition</p>
    </div>

    <div class="flex items-center justify-center gap-4 mb-16">
      {#each scenarios as s, i}
        <div class="contents">
          <button
            on:click={() => (week = i)}
            class={`group px-8 py-5 rounded-2xl transition-all duration-300 ${
              week === i
                ? 'bg-gradient-to-br from-blue-500 to-violet-600 text-white shadow-xl scale-105'
                : 'bg-white/80 text-slate-600 hover:bg-white hover:shadow-lg backdrop-blur'
            }`}
          >
            <div class="text-xs font-medium opacity-70 mb-1">Week {s.week}</div>
            <div class="text-sm font-medium">{s.name}</div>
          </button>
          {#if i < scenarios.length - 1}
            <div class="w-12 h-px bg-gradient-to-r from-slate-300 to-slate-200" />
          {/if}
        </div>
      {/each}
    </div>

    <div class="grid grid-cols-1 lg:grid-cols-5 gap-8">
      <div class="lg:col-span-2 space-y-6">
        <div class="bg-white/80 backdrop-blur rounded-3xl p-8 shadow-lg">
          <div class="mb-6">
            <h2 class="text-2xl font-light text-slate-800 mb-2">Membership</h2>
            <p class="text-slate-500 text-sm">Who is in the network?</p>
          </div>

          <div class="grid grid-cols-2 gap-4 mb-6">
            <div class="bg-gradient-to-br from-purple-50 to-pink-50 rounded-xl p-4">
              <div class="text-slate-600 text-xs font-medium mb-1">Network Avg</div>
              <div class="text-2xl font-light text-slate-800">{networkAvgMrs.toFixed(1)}%</div>
            </div>
            <div class="bg-gradient-to-br from-blue-50 to-cyan-50 rounded-xl p-4">
              <div class="text-slate-600 text-xs font-medium mb-1">Threshold</div>
              <div class="text-sm text-slate-700">≥ {MIN_RELATIONSHIP_COUNT} relationships at ≥ {MIN_MUTUAL_THRESHOLD}%</div>
            </div>
          </div>

          <div class="space-y-3">
            {#each participants as p}
              {#if members.includes(p)}
                {#let isMember = true}
              {:else}
                {#let isMember = false}
              {/if}
              {#let mrs = mrsByPerson[p] || 0}
              <div class={`rounded-2xl p-5 transition-all duration-500 ${
                isMember
                  ? 'bg-gradient-to-br from-emerald-100 to-teal-100 border-2 border-emerald-300'
                  : 'bg-gradient-to-br from-slate-100 to-slate-50 border-2 border-slate-200'
              }`}>
                <div class="flex items-center justify-between mb-3">
                  <span class="text-lg font-medium text-slate-800">{p}</span>
                  <span class={`text-xs px-3 py-1 rounded-full font-medium ${
                    isMember ? 'bg-emerald-500 text-white' : 'bg-slate-400 text-white'
                  }`}>{isMember ? 'Member' : 'Building'}</span>
                </div>

                <div class="grid grid-cols-2 gap-3 mb-3 text-sm">
                  <div>
                    <div class="text-slate-600 text-xs mb-1">Mutual Recognition (to members)</div>
                    <div class="font-mono text-base text-slate-800">{mrs.toFixed(1)}%</div>
                  </div>
                  <div>
                    <div class="text-slate-600 text-xs mb-1">Relationships ≥ {MIN_MUTUAL_THRESHOLD}%</div>
                    <div class="font-mono text-base text-slate-800">{
                      (() => {
                        let c = 0;
                        for (const m of members) if (m !== p && recognitionHelpers.getMutual(p, m) >= MIN_MUTUAL_THRESHOLD) c++;
                        return c;
                      })()
                    }</div>
                  </div>
                </div>

                <div class="h-2 bg-white/60 rounded-full overflow-hidden">
                  <div class={`h-full transition-all duration-700 rounded-full ${
                    isMember ? 'bg-gradient-to-r from-emerald-500 to-teal-500' : 'bg-slate-400'
                  }`} style={`width: ${Math.min(100, (mrs / (networkAvgMrs || 1)) * 50)}%`} />
                </div>
              </div>
            {/each}
          </div>
        </div>
      </div>

      <div class="lg:col-span-3 space-y-6">
        <div class="bg-white/80 backdrop-blur rounded-3xl p-8 shadow-lg">
          <div class="mb-6">
            <h2 class="text-2xl font-light text-slate-800 mb-2">Collective Prioritization</h2>
            <p class="text-slate-500 text-sm">Resource allocation by mutual recognition</p>
          </div>

          {#if members.length > 0}
            <>
              <div class="bg-gradient-to-br from-indigo-50 to-purple-50 rounded-2xl p-6 mb-6">
                <div class="text-slate-600 text-sm font-medium mb-2">Total Mutual Fulfillment Among {members.length} Members</div>
                <div class="text-3xl font-light text-slate-800">{crResult.totalMutual.toFixed(1)}%</div>
              </div>

              <div class="bg-gradient-to-br from-slate-50 to-slate-100 rounded-2xl p-6 mb-6">
                <svg bind:this={svgEl} width={520} height={300} />
              </div>

              <div class="space-y-3">
                {#each participants as p}
                  {#if !members.includes(p)}
                    <div class="rounded-2xl p-4 bg-slate-100/50 border-2 border-slate-200">
                      <div class="flex items-center justify-between opacity-50">
                        <span class="text-base font-medium text-slate-700">{p}</span>
                        <span class="text-xs text-slate-500">Not receiving</span>
                      </div>
                    </div>
                  {:else}
                    <div class="rounded-2xl p-4 bg-gradient-to-br from-blue-100 to-indigo-100 border-2 border-blue-300">
                      <div class="flex items-center justify-between mb-2">
                        <span class="text-base font-medium text-slate-800">{p}</span>
                        <span class="text-lg font-semibold text-blue-600">{crResult.allocations[p].priority.toFixed(1)}%</span>
                      </div>
                      <div class="flex gap-4 text-xs text-slate-600">
                        <div>Need: <span class="font-mono">${crResult.allocations[p].need}</span></div>
                        <div>Fulfillment: <span class="font-mono">{(crResult.mutualFulfillment[p] || 0).toFixed(1)}%</span></div>
                      </div>
                    </div>
                  {/if}
                {/each}
              </div>
            </>
          {:else}
            <div class="text-center py-12 text-slate-500">No members yet</div>
          {/if}
        </div>
      </div>
    </div>

    <div class="mt-12 text-center">
      <div class="inline-block bg-white/80 backdrop-blur rounded-2xl px-8 py-4 shadow-lg">
        <p class="text-slate-600 text-sm max-w-3xl">
          {#if week === 0}
            ✨ Seed participants establish mutual recognition → All become initial members → Resources flow proportionally through treemap
          {:else if week === 1}
            🔨 Dave builds relationships but does not meet membership threshold → Not yet a member → No space in resource treemap
          {:else if week === 2}
            🎉 Dave's mutual recognition deepens → Meets membership threshold → Becomes member → Receives proportional space in treemap
          {/if}
        </p>
      </div>
    </div>
  </div>
</div>