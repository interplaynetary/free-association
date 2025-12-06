<script lang="ts">
  import { ArrowRight, TrendingUp, Users, Target } from 'lucide-svelte';
  import * as d3 from 'd3';

  let beneficialAllocation = $state(60);
  let pieRef = $state<HTMLDivElement | null>(null);
  let derivativeRef = $state<HTMLDivElement | null>(null);
  let chartRef = $state<HTMLDivElement | null>(null);
  
  const nonBeneficialAllocation = $derived(100 - beneficialAllocation);
  
  // Goal achievement probability function: sigmoid-like growth
  const calculateGoalProbability = (tB: number): number => {
    const normalized = tB / 100;
    return 0.1 + 0.85 * (1 / (1 + Math.exp(-8 * (normalized - 0.5))));
  };
  
  const currentProbability = $derived(calculateGoalProbability(beneficialAllocation));
  
  // Calculate derivative numerically
  const calculateDerivative = (tB: number): number => {
    const h = 0.1;
    return (calculateGoalProbability(tB + h) - calculateGoalProbability(tB - h)) / (2 * h);
  };
  
  const currentDerivative = $derived(calculateDerivative(beneficialAllocation));
  
  // D3 Pie Chart
  $effect(() => {
    if (!pieRef) return;
    
    const width = 300;
    const height = 300;
    const radius = Math.min(width, height) / 2;
    
    d3.select(pieRef).selectAll("*").remove();
    
    const svg = d3.select(pieRef)
      .append("svg")
      .attr("width", width)
      .attr("height", height)
      .append("g")
      .attr("transform", `translate(${width/2},${height/2})`);
    
    type PieData = { label: string; value: number; color: string };
    
    const data: PieData[] = [
      { label: "Beneficial (B)", value: beneficialAllocation, color: "#10b981" },
      { label: "Non-Beneficial (N)", value: nonBeneficialAllocation, color: "#ef4444" }
    ];
    
    const pie = d3.pie<PieData>()
      .value((d: PieData) => d.value)
      .sort(null);
    
    const arc = d3.arc<d3.PieArcDatum<PieData>>()
      .innerRadius(0)
      .outerRadius(radius - 10);
    
    const labelArc = d3.arc<d3.PieArcDatum<PieData>>()
      .innerRadius(radius - 60)
      .outerRadius(radius - 60);
    
    const arcs = svg.selectAll("arc")
      .data(pie(data))
      .enter()
      .append("g")
      .attr("class", "arc");
    
    arcs.append("path")
      .attr("d", (d: any) => arc(d) as string)
      .attr("fill", (d: any) => d.data.color)
      .attr("stroke", "white")
      .attr("stroke-width", 2)
      .style("opacity", 0.9)
      .on("mouseover", function() {
        d3.select(this).style("opacity", 1);
      })
      .on("mouseout", function() {
        d3.select(this).style("opacity", 0.9);
      });
    
    arcs.append("text")
      .attr("transform", (d: any) => {
        const centroid = labelArc.centroid(d);
        return `translate(${centroid})`;
      })
      .attr("text-anchor", "middle")
      .attr("font-size", "14px")
      .attr("font-weight", "bold")
      .attr("fill", "white")
      .text((d: any) => `${d.data.value}%`);
    
    // Center text showing P(G)
    svg.append("text")
      .attr("text-anchor", "middle")
      .attr("y", -10)
      .attr("font-size", "12px")
      .attr("fill", "#666")
      .text("P(G) =");
    
    svg.append("text")
      .attr("text-anchor", "middle")
      .attr("y", 10)
      .attr("font-size", "20px")
      .attr("font-weight", "bold")
      .attr("fill", "#4F46E5")
      .text(`${(currentProbability * 100).toFixed(1)}%`);
    
  });
  
  // D3 Derivative Visualization
  $effect(() => {
    if (!derivativeRef) return;
    
    const width = 300;
    const height = 250;
    const margin = { top: 20, right: 20, bottom: 40, left: 50 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    
    d3.select(derivativeRef).selectAll("*").remove();
    
    const svg = d3.select(derivativeRef)
      .append("svg")
      .attr("width", width)
      .attr("height", height);
    
    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);
    
    // Generate derivative data
    type DerivData = { x: number; y: number };
    const derivData: DerivData[] = [];
    for (let t = 0; t <= 100; t += 1) {
      derivData.push({
        x: t,
        y: calculateDerivative(t)
      });
    }
    
    const xScale = d3.scaleLinear()
      .domain([0, 100])
      .range([0, innerWidth]);
    
    const yScale = d3.scaleLinear()
      .domain([0, (d3.max(derivData, d => d.y) || 0) * 1.1])
      .range([innerHeight, 0]);
    
    // Add axes
    g.append("g")
      .attr("transform", `translate(0,${innerHeight})`)
      .call(d3.axisBottom(xScale).ticks(5))
      .append("text")
      .attr("x", innerWidth / 2)
      .attr("y", 35)
      .attr("fill", "black")
      .attr("font-size", "11px")
      .text("T(a,B) %");
    
    g.append("g")
      .call(d3.axisLeft(yScale).ticks(5).tickFormat(d => Number(d).toFixed(3)))
      .append("text")
      .attr("transform", "rotate(-90)")
      .attr("x", -innerHeight / 2)
      .attr("y", -35)
      .attr("fill", "black")
      .attr("font-size", "11px")
      .attr("text-anchor", "middle")
      .text("dP(G)/dT(a,B)");
    
    // Add gradient area
    const area = d3.area<DerivData>()
      .x(d => xScale(d.x))
      .y0(innerHeight)
      .y1(d => yScale(d.y))
      .curve(d3.curveMonotoneX);
    
    g.append("path")
      .datum(derivData)
      .attr("fill", "url(#gradient)")
      .attr("d", area as any)
      .style("opacity", 0.6);
    
    // Add gradient definition
    const gradient = svg.append("defs")
      .append("linearGradient")
      .attr("id", "gradient")
      .attr("x1", "0%")
      .attr("y1", "0%")
      .attr("x2", "0%")
      .attr("y2", "100%");
    
    gradient.append("stop")
      .attr("offset", "0%")
      .attr("stop-color", "#8b5cf6")
      .attr("stop-opacity", 1);
    
    gradient.append("stop")
      .attr("offset", "100%")
      .attr("stop-color", "#8b5cf6")
      .attr("stop-opacity", 0.3);
    
    // Add line
    const line = d3.line<DerivData>()
      .x(d => xScale(d.x))
      .y(d => yScale(d.y))
      .curve(d3.curveMonotoneX);
    
    g.append("path")
      .datum(derivData)
      .attr("fill", "none")
      .attr("stroke", "#8b5cf6")
      .attr("stroke-width", 2)
      .attr("d", line as any);
    
    // Add current position marker
    g.append("circle")
      .attr("cx", xScale(beneficialAllocation))
      .attr("cy", yScale(currentDerivative))
      .attr("r", 5)
      .attr("fill", "#dc2626")
      .attr("stroke", "white")
      .attr("stroke-width", 2);
    
    // Add value label
    g.append("text")
      .attr("x", xScale(beneficialAllocation))
      .attr("y", yScale(currentDerivative) - 10)
      .attr("text-anchor", "middle")
      .attr("font-size", "10px")
      .attr("font-weight", "bold")
      .attr("fill", "#dc2626")
      .text(currentDerivative.toFixed(4));
    
  });
  
  // D3 Main Chart
  $effect(() => {
    if (!chartRef) return;
    
    const width = 800;
    const height = 300;
    const margin = { top: 20, right: 20, bottom: 60, left: 60 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    
    d3.select(chartRef).selectAll("*").remove();
    
    const svg = d3.select(chartRef)
      .append("svg")
      .attr("width", "100%")
      .attr("height", height)
      .attr("viewBox", `0 0 ${width} ${height}`);
    
    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);
    
    const xScale = d3.scaleLinear()
      .domain([0, 100])
      .range([0, innerWidth]);
    
    const yScale = d3.scaleLinear()
      .domain([0, 1])
      .range([innerHeight, 0]);
    
    // Grid
    g.append("g")
      .attr("class", "grid")
      .attr("opacity", 0.1)
      .call(d3.axisLeft(yScale).ticks(5).tickSize(-innerWidth).tickFormat(() => ""));
    
    g.append("g")
      .attr("class", "grid")
      .attr("opacity", 0.1)
      .attr("transform", `translate(0,${innerHeight})`)
      .call(d3.axisBottom(xScale).ticks(10).tickSize(-innerHeight).tickFormat(() => ""));
    
    // Area
    const area = d3.area<{tB: number, probability: number}>()
      .x(d => xScale(d.tB))
      .y0(innerHeight)
      .y1(d => yScale(d.probability))
      .curve(d3.curveMonotoneX);
    
    g.append("path")
      .datum(chartData)
      .attr("fill", "#818CF8")
      .attr("opacity", 0.6)
      .attr("d", area);
    
    // Line
    const line = d3.line<{tB: number, probability: number}>()
      .x(d => xScale(d.tB))
      .y(d => yScale(d.probability))
      .curve(d3.curveMonotoneX);
    
    g.append("path")
      .datum(chartData)
      .attr("fill", "none")
      .attr("stroke", "#4F46E5")
      .attr("stroke-width", 3)
      .attr("d", line);
    
    // Axes
    g.append("g")
      .attr("transform", `translate(0,${innerHeight})`)
      .call(d3.axisBottom(xScale).ticks(10))
      .append("text")
      .attr("x", innerWidth / 2)
      .attr("y", 45)
      .attr("fill", "black")
      .attr("font-size", "12px")
      .attr("text-anchor", "middle")
      .text("T(a,B) - Recognition to Beneficial Partners (%)");
    
    g.append("g")
      .call(d3.axisLeft(yScale).ticks(5).tickFormat(d => `${(Number(d) * 100).toFixed(0)}%`))
      .append("text")
      .attr("transform", "rotate(-90)")
      .attr("x", -innerHeight / 2)
      .attr("y", -45)
      .attr("fill", "black")
      .attr("font-size", "12px")
      .attr("text-anchor", "middle")
      .text("P(G) - Goal Achievement Probability");
    
    // Current position marker
    g.append("circle")
      .attr("cx", xScale(beneficialAllocation))
      .attr("cy", yScale(currentProbability))
      .attr("r", 6)
      .attr("fill", "#dc2626")
      .attr("stroke", "white")
      .attr("stroke-width", 2);
  });
  
  const chartData = $derived.by(() => {
    const data = [];
    for (let tb = 0; tb <= 100; tb += 2) {
      data.push({
        tB: tb,
        probability: calculateGoalProbability(tb)
      });
    }
    return data;
  });
  
  const rer = $derived(nonBeneficialAllocation > 0 ? 
    (beneficialAllocation / nonBeneficialAllocation).toFixed(2) : '∞');
  
  const marginalBenefit = $derived(beneficialAllocation < 100 ?
    ((calculateGoalProbability(Math.min(100, beneficialAllocation + 10)) - currentProbability) * 100).toFixed(1) : 0);
</script>

<div class="w-full max-w-7xl mx-auto p-6 bg-gradient-to-br from-blue-50 to-indigo-50">
  <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
    <h1 class="text-3xl font-bold text-gray-800 mb-2 flex items-center gap-2">
      <Target class="text-indigo-600" />
      Anti-Gaming Theorem: Recognition Budget Allocation
    </h1>
    <p class="text-gray-600 mb-4">
      Visualizing how ∂P(G)/∂T(a,B) {'>'} 0 drives optimal recognition allocation
    </p>
    
    <div class="bg-indigo-50 border-l-4 border-indigo-500 p-4 mb-4">
      <p class="text-sm text-gray-700">
            <strong>Budget Constraint:</strong> T(a,B) + T(a,N) = 1 (100%)
            <br/>
            <strong>Theorem:</strong> dP(G)/dT(a,B) {'>'} 0 ⟹ Goal achievement strictly increases with beneficial recognition
          </p>
        </div>

        <div class="bg-gray-50 border border-gray-200 rounded-lg p-4">
          <h3 class="font-semibold text-gray-800 mb-3 text-sm">Variable Definitions</h3>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-2 text-xs">
            <div><span class="font-mono font-semibold text-indigo-600">a</span> = You, the participant with a goal</div>
            <div><span class="font-mono font-semibold text-indigo-600">G</span> = Your goal (what you're trying to achieve)</div>
            <div><span class="font-mono font-semibold text-indigo-600">P</span> = Set of all participants in the network</div>
            <div><span class="font-mono font-semibold text-green-600">B ⊆ P</span> = Beneficial partners (help achieve G)</div>
            <div><span class="font-mono font-semibold text-red-600">N = P \ B</span> = Non-beneficial partners (don't help G)</div>
            <div><span class="font-mono font-semibold text-blue-600">P(G)</span> = Probability of achieving goal G</div>
            <div><span class="font-mono font-semibold text-green-600">T(a,B)</span> = Total recognition allocated to B</div>
            <div><span class="font-mono font-semibold text-red-600">T(a,N)</span> = Total recognition allocated to N</div>
            <div><span class="font-mono font-semibold text-purple-600">R(a,b)</span> = Recognition from a to partner b</div>
            <div><span class="font-mono font-semibold text-orange-600">RER</span> = Recognition Efficiency Ratio = T(B)/T(N)</div>
          </div>
        </div>
      </div>

  <!-- Interactive Controls -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
          <Users class="text-green-600" />
          Adjust Recognition Budget
        </h2>
        
        <div class="mb-6">
          <label for="beneficial-allocation" class="block text-sm font-medium text-gray-700 mb-2">
            Recognition to Beneficial Partners: T(a,B) = {beneficialAllocation}%
          </label>
      <input
        id="beneficial-allocation"
        type="range"
        min="0"
        max="100"
        bind:value={beneficialAllocation}
        class="w-full h-3 bg-gradient-to-r from-red-300 via-yellow-300 to-green-500 rounded-lg appearance-none cursor-pointer"
      />
          <div class="flex justify-between text-xs text-gray-500 mt-1">
            <span>0% (All wasted)</span>
            <span>50% (Half wasted)</span>
            <span>100% (Optimal)</span>
          </div>
        </div>
      </div>

  <!-- Pie Chart and Derivative Side by Side -->
  <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
    <!-- Pie Chart -->
        <div class="bg-white rounded-lg shadow-lg p-6">
          <h2 class="text-lg font-semibold text-gray-800 mb-3">
            Recognition Budget Allocation
          </h2>
          <p class="text-sm text-gray-600 mb-4">
            Constraint: T(a,B) + T(a,N) = 1
          </p>
      <div bind:this={pieRef} class="flex justify-center"></div>
          <div class="mt-4 space-y-2">
            <div class="flex items-center gap-2">
              <div class="w-4 h-4 bg-green-500 rounded"></div>
              <span class="text-sm"><span class="font-mono font-semibold">T(a,B)</span> = {beneficialAllocation}% → Beneficial partners who help achieve your goal</span>
            </div>
            <div class="flex items-center gap-2">
              <div class="w-4 h-4 bg-red-500 rounded"></div>
              <span class="text-sm"><span class="font-mono font-semibold">T(a,N)</span> = {nonBeneficialAllocation}% → Non-beneficial partners (wasted recognition)</span>
            </div>
          </div>
        </div>

    <!-- Derivative Chart -->
        <div class="bg-white rounded-lg shadow-lg p-6">
          <h2 class="text-lg font-semibold text-gray-800 mb-3">
            Marginal Impact: <span class="font-mono">dP(G)/dT(a,B)</span>
          </h2>
          <p class="text-sm text-gray-600 mb-4">
            Rate of change in goal achievement probability per unit change in beneficial recognition
          </p>
      <div bind:this={derivativeRef} class="flex justify-center"></div>
          <div class="mt-4 bg-purple-50 p-3 rounded">
            <p class="text-sm text-gray-700">
              <strong>Current derivative:</strong> <span class="font-mono">{currentDerivative.toFixed(4)}</span>
              <br/>
              <span class="text-xs">
                {currentDerivative > 0 ? 
                  "✓ Positive: Increasing T(a,B) by 1% improves P(G)" : 
                  "⚠ At maximum"}
              </span>
            </p>
          </div>
        </div>
      </div>

  <!-- Key Metrics -->
      <div class="grid grid-cols-1 md:grid-cols-4 gap-4 mb-6">
        <div class="bg-white rounded-lg shadow-lg p-5">
          <div class="flex items-center justify-between mb-2">
            <span class="text-sm font-medium text-gray-600">Goal Achievement</span>
            <Target class="text-blue-600 w-5 h-5" />
          </div>
          <div class="text-3xl font-bold text-blue-700">
            {(currentProbability * 100).toFixed(1)}%
          </div>
          <div class="text-xs text-gray-500 mt-1 font-mono">P(G) = Probability of achieving goal G</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-5">
          <div class="flex items-center justify-between mb-2">
            <span class="text-sm font-medium text-gray-600">Derivative</span>
            <TrendingUp class="text-purple-600 w-5 h-5" />
          </div>
          <div class="text-3xl font-bold text-purple-700">
            {currentDerivative.toFixed(3)}
          </div>
          <div class="text-xs text-gray-500 mt-1 font-mono">dP/dT(B) = Rate of change</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-5">
          <div class="flex items-center justify-between mb-2">
            <span class="text-sm font-medium text-gray-600">Efficiency Ratio</span>
            <TrendingUp class="text-orange-600 w-5 h-5" />
          </div>
          <div class="text-3xl font-bold text-orange-700">
            {rer}
          </div>
          <div class="text-xs text-gray-500 mt-1 font-mono">RER = T(B)/T(N)</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-5">
          <div class="flex items-center justify-between mb-2">
            <span class="text-sm font-medium text-gray-600">+10% Benefit</span>
            <ArrowRight class="text-green-600 w-5 h-5" />
          </div>
          <div class="text-3xl font-bold text-green-700">
            +{marginalBenefit}%
          </div>
          <div class="text-xs text-gray-500 mt-1 font-mono">Δ P(G) from +10% to T(B)</div>
        </div>
      </div>

  <!-- Main Chart -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold text-gray-800 mb-4">
          <span class="font-mono">P(G)</span> as Function of <span class="font-mono">T(a,B)</span>
        </h2>
        <p class="text-sm text-gray-600 mb-4">
          How your goal achievement probability changes with recognition allocation to beneficial partners
        </p>
    <div bind:this={chartRef} class="w-full"></div>
        <div class="mt-2 text-sm text-gray-600 text-center">
          Current allocation: T(B) = {beneficialAllocation}%, P(G) = {(currentProbability * 100).toFixed(1)}%
        </div>
      </div>

  <!-- Mathematical Insights -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold text-gray-800 mb-4">Mathematical Interpretation</h2>
        
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="p-4 bg-blue-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2">The Budget Constraint</h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p class="font-mono">T(a,B) + T(a,N) = 1</p>
              <p class="text-xs"><span class="font-mono">T(a,B)</span> = Total recognition you give to beneficial partners (who help your goal)</p>
              <p class="text-xs"><span class="font-mono">T(a,N)</span> = Total recognition you give to non-beneficial partners (who don't help)</p>
              <p class="text-xs">Total recognition is conserved. Every % to N is a % not given to B.</p>
            </div>
          </div>

          <div class="p-4 bg-purple-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2">The Derivative</h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p class="font-mono">dP(G)/dT(a,B) {'>'} 0</p>
              <p class="text-xs"><span class="font-mono">dP(G)/dT(a,B)</span> = Rate at which your goal achievement probability changes when you increase recognition to beneficial partners</p>
              <p class="text-xs">Always positive: more beneficial recognition always helps.</p>
              <p class="text-xs">Decreasing slope: diminishing returns as T(B) → 100%.</p>
            </div>
          </div>

          <div class="p-4 bg-green-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2">Opportunity Cost</h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p class="font-mono">dP(G)/dT(a,N) {'<'} 0</p>
              <p class="text-xs"><span class="font-mono">dP(G)/dT(a,N)</span> = Rate at which your goal achievement changes when you increase recognition to non-beneficial partners</p>
              <p class="text-xs">Recognition to N reduces P(G) by taking from B.</p>
              <p class="text-xs">Cost = derivative × amount wasted.</p>
            </div>
          </div>

          <div class="p-4 bg-orange-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2">Optimal Point</h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p class="font-mono">T*(a,B) = 1, T*(a,N) = 0</p>
              <p class="text-xs"><span class="font-mono">T*</span> = Optimal allocation that maximizes <span class="font-mono">P(G)</span></p>
              <p class="text-xs">Maximum P(G) when 100% goes to beneficial partners.</p>
              <p class="text-xs">Any T(N) {'>'} 0 is suboptimal.</p>
            </div>
          </div>
        </div>
      </div>

  <!-- Example Scenarios -->
      <div class="bg-white rounded-lg shadow-lg p-6">
        <h2 class="text-xl font-semibold text-gray-800 mb-4">Test Scenarios</h2>
        
        <div class="space-y-3">
      <button
        onclick={() => beneficialAllocation = 20}
        class="w-full text-left p-4 bg-red-50 hover:bg-red-100 rounded-lg border border-red-200 transition-colors"
      >
            <div class="font-semibold text-red-800">High Waste (20% to B, 80% to N)</div>
            <div class="text-sm text-gray-600 mt-1">
              Severe underallocation - large positive derivative means huge improvement potential
            </div>
          </button>

      <button
        onclick={() => beneficialAllocation = 50}
        class="w-full text-left p-4 bg-yellow-50 hover:bg-yellow-100 rounded-lg border border-yellow-200 transition-colors"
      >
            <div class="font-semibold text-yellow-800">Half Wasted (50% to B, 50% to N)</div>
            <div class="text-sm text-gray-600 mt-1">
              Moderate allocation - still strong derivative, clear room to optimize
            </div>
          </button>

      <button
        onclick={() => beneficialAllocation = 90}
        class="w-full text-left p-4 bg-green-50 hover:bg-green-100 rounded-lg border border-green-200 transition-colors"
      >
            <div class="font-semibold text-green-800">Near-Optimal (90% to B, 10% to N)</div>
            <div class="text-sm text-gray-600 mt-1">
              Excellent allocation - small positive derivative, diminishing returns
            </div>
      </button>
    </div>
  </div>
</div>