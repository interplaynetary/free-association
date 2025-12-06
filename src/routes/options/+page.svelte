<script lang="ts">
  import { ArrowRight, TrendingUp, Users, Target, Zap } from 'lucide-svelte';
  import * as d3 from 'd3';

  let beneficialAllocation = $state(60);
  let timeHorizon = $state(1); // Time variable
  let pieRef = $state<HTMLDivElement | null>(null);
  let derivativeRef = $state<HTMLDivElement | null>(null);
  let greeksRef = $state<HTMLDivElement | null>(null);
  
  const nonBeneficialAllocation = $derived(100 - beneficialAllocation);
  
  // Goal achievement probability function with time dependency
  const calculateGoalProbability = (tB: number, t: number = timeHorizon): number => {
    const normalized = tB / 100;
    // Add time decay/acceleration factor
    const timeFactor = 1 - Math.exp(-0.5 * t); // Approaches 1 as time increases
    return timeFactor * (0.1 + 0.85 * (1 / (1 + Math.exp(-8 * (normalized - 0.5)))));
  };
  
  const currentProbability = $derived(calculateGoalProbability(beneficialAllocation, timeHorizon));
  
  // === THE GREEKS ===
  
  // Delta: ∂P/∂T(a,B) - First derivative w.r.t. beneficial allocation
  const calculateDelta = (tB: number, t: number = timeHorizon): number => {
    const h = 0.1;
    return (calculateGoalProbability(tB + h, t) - calculateGoalProbability(tB - h, t)) / (2 * h);
  };
  
  // Gamma: ∂²P/∂T(a,B)² - Second derivative (convexity/acceleration)
  const calculateGamma = (tB: number, t: number = timeHorizon): number => {
    const h = 0.5;
    const fPlus = calculateGoalProbability(tB + h, t);
    const f = calculateGoalProbability(tB, t);
    const fMinus = calculateGoalProbability(tB - h, t);
    return (fPlus - 2*f + fMinus) / (h * h);
  };
  
  // Theta: ∂P/∂t - Time decay/acceleration of goal achievement
  const calculateTheta = (tB: number, t: number = timeHorizon): number => {
    const h = 0.01;
    return (calculateGoalProbability(tB, t + h) - calculateGoalProbability(tB, t - h)) / (2 * h);
  };
  
  // Vega: ∂P/∂σ - Sensitivity to "volatility" (network instability)
  // In options: sensitivity to volatility. Here: sensitivity to recognition uncertainty
  const calculateVega = (tB: number): number => {
    // Simulating volatility effect: uncertainty in whether partners remain beneficial
    // Higher vega at mid-allocations (50%) where uncertainty matters most
    const normalized = tB / 100;
    return 0.5 * Math.exp(-8 * Math.pow(normalized - 0.5, 2));
  };
  
  // Rho: ∂P/∂r - Sensitivity to "interest rate" (opportunity cost rate)
  // In recognition: sensitivity to alternative uses of recognition
  const calculateRho = (tB: number): number => {
    // Higher rho means higher opportunity cost of current allocation
    const normalized = tB / 100;
    return -0.3 * (1 - normalized); // Negative: cost decreases as T(B) increases
  };
  
  const currentDelta = $derived(calculateDelta(beneficialAllocation, timeHorizon));
  const currentGamma = $derived(calculateGamma(beneficialAllocation, timeHorizon));
  const currentTheta = $derived(calculateTheta(beneficialAllocation, timeHorizon));
  const currentVega = $derived(calculateVega(beneficialAllocation));
  const currentRho = $derived(calculateRho(beneficialAllocation));
  
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
      .style("opacity", 0.9);
    
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
  
  // D3 Greeks Visualization
  $effect(() => {
    if (!greeksRef) return;
    
    const width = 600;
    const height = 250;
    const margin = { top: 20, right: 20, bottom: 40, left: 60 };
    const innerWidth = width - margin.left - margin.right;
    const innerHeight = height - margin.top - margin.bottom;
    
    d3.select(greeksRef).selectAll("*").remove();
    
    const svg = d3.select(greeksRef)
      .append("svg")
      .attr("width", width)
      .attr("height", height);
    
    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);
    
    // Generate Greeks data
    type GreeksData = { x: number; delta: number; gamma: number; vega: number };
    const greeksData: GreeksData[] = [];
    for (let t = 0; t <= 100; t += 1) {
      greeksData.push({
        x: t,
        delta: calculateDelta(t, timeHorizon),
        gamma: calculateGamma(t, timeHorizon) * 10, // Scale for visibility
        vega: calculateVega(t)
      });
    }
    
    const xScale = d3.scaleLinear()
      .domain([0, 100])
      .range([0, innerWidth]);
    
    const yScale = d3.scaleLinear()
      .domain([
        (d3.min(greeksData, d => Math.min(d.delta, d.gamma, d.vega)) || 0) * 1.2,
        (d3.max(greeksData, d => Math.max(d.delta, d.gamma, d.vega)) || 0) * 1.2
      ])
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
      .attr("y", -45)
      .attr("fill", "black")
      .attr("font-size", "11px")
      .attr("text-anchor", "middle")
      .text("Greek Values");
    
    // Lines for each Greek
    const lineDelta = d3.line<GreeksData>()
      .x(d => xScale(d.x))
      .y(d => yScale(d.delta))
      .curve(d3.curveMonotoneX);
    
    const lineGamma = d3.line<GreeksData>()
      .x(d => xScale(d.x))
      .y(d => yScale(d.gamma))
      .curve(d3.curveMonotoneX);
    
    const lineVega = d3.line<GreeksData>()
      .x(d => xScale(d.x))
      .y(d => yScale(d.vega))
      .curve(d3.curveMonotoneX);
    
    // Delta line
    g.append("path")
      .datum(greeksData)
      .attr("fill", "none")
      .attr("stroke", "#8b5cf6")
      .attr("stroke-width", 2)
      .attr("d", lineDelta as any);
    
    // Gamma line
    g.append("path")
      .datum(greeksData)
      .attr("fill", "none")
      .attr("stroke", "#ef4444")
      .attr("stroke-width", 2)
      .attr("stroke-dasharray", "5,5")
      .attr("d", lineGamma as any);
    
    // Vega line
    g.append("path")
      .datum(greeksData)
      .attr("fill", "none")
      .attr("stroke", "#f59e0b")
      .attr("stroke-width", 2)
      .attr("stroke-dasharray", "3,3")
      .attr("d", lineVega as any);
    
    // Current position marker
    g.append("circle")
      .attr("cx", xScale(beneficialAllocation))
      .attr("cy", yScale(currentDelta))
      .attr("r", 5)
      .attr("fill", "#8b5cf6")
      .attr("stroke", "white")
      .attr("stroke-width", 2);
    
    // Legend
    const legend = g.append("g")
      .attr("transform", `translate(${innerWidth - 100}, 10)`);
    
    legend.append("line")
      .attr("x1", 0).attr("x2", 20)
      .attr("y1", 0).attr("y2", 0)
      .attr("stroke", "#8b5cf6")
      .attr("stroke-width", 2);
    legend.append("text")
      .attr("x", 25).attr("y", 4)
      .attr("font-size", "10px")
      .text("Δ Delta");
    
    legend.append("line")
      .attr("x1", 0).attr("x2", 20)
      .attr("y1", 15).attr("y2", 15)
      .attr("stroke", "#ef4444")
      .attr("stroke-width", 2)
      .attr("stroke-dasharray", "5,5");
    legend.append("text")
      .attr("x", 25).attr("y", 19)
      .attr("font-size", "10px")
      .text("Γ Gamma");
    
    legend.append("line")
      .attr("x1", 0).attr("x2", 20)
      .attr("y1", 30).attr("y2", 30)
      .attr("stroke", "#f59e0b")
      .attr("stroke-width", 2)
      .attr("stroke-dasharray", "3,3");
    legend.append("text")
      .attr("x", 25).attr("y", 34)
      .attr("font-size", "10px")
      .text("ν Vega");
    
  });
  
  const chartData = $derived.by(() => {
    const data = [];
    for (let tb = 0; tb <= 100; tb += 2) {
      data.push({
        tB: tb,
        probability: calculateGoalProbability(tb, timeHorizon)
      });
    }
    return data;
  });
  
  const rer = $derived(nonBeneficialAllocation > 0 ? 
    (beneficialAllocation / nonBeneficialAllocation).toFixed(2) : '∞');
</script>

<div class="w-full max-w-7xl mx-auto p-6 bg-gradient-to-br from-blue-50 to-indigo-50">
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h1 class="text-3xl font-bold text-gray-800 mb-2 flex items-center gap-2">
          <Target class="text-indigo-600" />
          Recognition Greeks: Options-Style Risk Analytics
        </h1>
        <p class="text-gray-600 mb-4">
          Applying derivatives pricing theory to recognition allocation
        </p>
        
        <div class="bg-indigo-50 border-l-4 border-indigo-500 p-4 mb-4">
          <p class="text-sm text-gray-700">
            <strong>Recognition as Option:</strong> T(a,B) is like an option's underlying asset price, P(G) is the option payoff
            <br/>
            <strong>Budget Constraint:</strong> T(a,B) + T(a,N) = 1 → Zero-sum allocation creates delta-hedging dynamics
          </p>
        </div>

        <div class="bg-gray-50 border border-gray-200 rounded-lg p-4">
          <h3 class="font-semibold text-gray-800 mb-3 text-sm">The Recognition Greeks</h3>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-3 text-xs">
            <div>
              <span class="font-mono font-semibold text-purple-600">Δ (Delta)</span> = ∂P(G)/∂T(a,B)
              <p class="text-gray-600 ml-4">Directional exposure: how much P(G) changes per 1% change in T(B)</p>
            </div>
            <div>
              <span class="font-mono font-semibold text-red-600">Γ (Gamma)</span> = ∂²P(G)/∂T(a,B)²
              <p class="text-gray-600 ml-4">Convexity: rate of change of delta (acceleration of returns)</p>
            </div>
            <div>
              <span class="font-mono font-semibold text-blue-600">Θ (Theta)</span> = ∂P(G)/∂t
              <p class="text-gray-600 ml-4">Time decay: how goal achievement changes over time</p>
            </div>
            <div>
              <span class="font-mono font-semibold text-orange-600">ν (Vega)</span> = ∂P(G)/∂σ
              <p class="text-gray-600 ml-4">Volatility exposure: sensitivity to network instability</p>
            </div>
            <div>
              <span class="font-mono font-semibold text-green-600">ρ (Rho)</span> = ∂P(G)/∂r
              <p class="text-gray-600 ml-4">Rate sensitivity: impact of opportunity cost changes</p>
            </div>
          </div>
        </div>
      </div>

  <!-- Interactive Controls -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
          <Users class="text-green-600" />
          Position Parameters
        </h2>
        
        <div class="mb-6">
          <label for="beneficial-allocation" class="block text-sm font-medium text-gray-700 mb-2">
            Underlying: T(a,B) = {beneficialAllocation}% (Recognition to Beneficial Partners)
          </label>
      <input
        id="beneficial-allocation"
        type="range"
        min="0"
        max="100"
        bind:value={beneficialAllocation}
        class="w-full h-3 bg-gradient-to-r from-red-300 via-yellow-300 to-green-500 rounded-lg appearance-none cursor-pointer"
      />
        </div>

        <div class="mb-4">
          <label for="time-horizon" class="block text-sm font-medium text-gray-700 mb-2">
            Time to Expiry: t = {timeHorizon.toFixed(1)} years
          </label>
      <input
        id="time-horizon"
        type="range"
        min="0.1"
        max="5"
        step="0.1"
        bind:value={timeHorizon}
        class="w-full h-3 bg-gradient-to-r from-blue-200 to-blue-500 rounded-lg appearance-none cursor-pointer"
      />
          <div class="flex justify-between text-xs text-gray-500 mt-1">
            <span>0.1y (Short-term)</span>
            <span>5y (Long-term)</span>
          </div>
        </div>
      </div>

  <!-- Greeks Dashboard -->
      <div class="grid grid-cols-2 md:grid-cols-5 gap-4 mb-6">
        <div class="bg-white rounded-lg shadow-lg p-4">
          <div class="text-xs font-medium text-gray-600 mb-1">Δ Delta</div>
          <div class="text-2xl font-bold text-purple-700">
            {currentDelta.toFixed(3)}
          </div>
          <div class="text-xs text-gray-500 mt-1">Directional</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-4">
          <div class="text-xs font-medium text-gray-600 mb-1">Γ Gamma</div>
          <div class="text-2xl font-bold text-red-700">
            {currentGamma.toFixed(4)}
          </div>
          <div class="text-xs text-gray-500 mt-1">Convexity</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-4">
          <div class="text-xs font-medium text-gray-600 mb-1">Θ Theta</div>
          <div class="text-2xl font-bold text-blue-700">
            {currentTheta.toFixed(3)}
          </div>
          <div class="text-xs text-gray-500 mt-1">Time decay</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-4">
          <div class="text-xs font-medium text-gray-600 mb-1">ν Vega</div>
          <div class="text-2xl font-bold text-orange-700">
            {currentVega.toFixed(3)}
          </div>
          <div class="text-xs text-gray-500 mt-1">Volatility</div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-4">
          <div class="text-xs font-medium text-gray-600 mb-1">ρ Rho</div>
          <div class="text-2xl font-bold text-green-700">
            {currentRho.toFixed(3)}
          </div>
          <div class="text-xs text-gray-500 mt-1">Rate risk</div>
        </div>
      </div>

  <!-- Greeks Chart and Pie -->
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
        <div class="bg-white rounded-lg shadow-lg p-6">
          <h2 class="text-lg font-semibold text-gray-800 mb-3">
            Position: T(a,B) Portfolio
          </h2>
      <div bind:this={pieRef} class="flex justify-center"></div>
          <div class="mt-4 bg-blue-50 p-3 rounded text-xs">
            <strong>P(G) = {(currentProbability * 100).toFixed(1)}%</strong> (Option Payoff)
            <br/>
            <span class="text-gray-600">Strike = 0%, Moneyness = {beneficialAllocation}%</span>
          </div>
        </div>

        <div class="bg-white rounded-lg shadow-lg p-6">
          <h2 class="text-lg font-semibold text-gray-800 mb-3">
            Greeks Surface
          </h2>
      <div bind:this={greeksRef}></div>
        </div>
      </div>

  <!-- Greeks Interpretation -->
      <div class="bg-white rounded-lg shadow-lg p-6 mb-6">
        <h2 class="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
          <Zap class="text-yellow-500" />
          Trading the Recognition Greeks
        </h2>
        
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div class="p-4 bg-purple-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2 flex items-center gap-2">
              <span class="text-purple-600">Δ</span> Delta Hedging
            </h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p><strong>Current Δ:</strong> {currentDelta.toFixed(3)}</p>
              <p class="text-xs">
                {currentDelta > 0.005 ? 
                  "✓ Positive delta: Long directional exposure to T(B). Benefit from increasing beneficial recognition." :
                  "⚠ Low delta: Near optimal allocation, diminishing returns."}
              </p>
              <p class="text-xs font-mono">
                Delta-neutral: Hedge by diversifying goal portfolio
              </p>
            </div>
          </div>

          <div class="p-4 bg-red-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2 flex items-center gap-2">
              <span class="text-red-600">Γ</span> Gamma Scalping
            </h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p><strong>Current Γ:</strong> {currentGamma.toFixed(4)}</p>
              <p class="text-xs">
                {Math.abs(currentGamma) > 0.001 ?
                  currentGamma > 0 ? 
                    "✓ Positive gamma: Convex payoff structure. Benefit from recognition volatility." :
                    "⚠ Negative gamma: Concave region, vulnerable to swings." :
                  "Neutral gamma: Linear exposure region."}
              </p>
              <p class="text-xs font-mono">
                High |Γ| → Rebalance frequently
              </p>
            </div>
          </div>

          <div class="p-4 bg-blue-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2 flex items-center gap-2">
              <span class="text-blue-600">Θ</span> Time Value
            </h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p><strong>Current Θ:</strong> {currentTheta.toFixed(3)}</p>
              <p class="text-xs">
                {currentTheta > 0 ?
                  "✓ Positive theta: Goal achievement accelerates over time. Patience rewarded." :
                  "⚠ Negative theta: Time decay erodes value. Act urgently."}
              </p>
              <p class="text-xs font-mono">
                t = {timeHorizon.toFixed(1)}y → {(currentProbability * 100).toFixed(1)}% P(G)
              </p>
            </div>
          </div>

          <div class="p-4 bg-orange-50 rounded-lg">
            <h3 class="font-semibold text-gray-800 mb-2 flex items-center gap-2">
              <span class="text-orange-600">ν</span> Vega Exposure
            </h3>
            <div class="text-sm text-gray-700 space-y-2">
              <p><strong>Current ν:</strong> {currentVega.toFixed(3)}</p>
              <p class="text-xs">
                {currentVega > 0.2 ?
                  "⚠ High vega: Sensitive to network volatility. Partner stability critical." :
                  "✓ Low vega: Stable position, less affected by partnership uncertainty."}
              </p>
              <p class="text-xs font-mono">
                Hedge: Diversify partner portfolio
              </p>
            </div>
          </div>
    </div>
  </div>

  <!-- Options Analogy -->
  <div class="bg-white rounded-lg shadow-lg p-6">
    <h2 class="text-xl font-semibold text-gray-800 mb-4">Recognition as Derivatives: The Mapping</h2>
    
    <div class="overflow-x-auto">
      <table class="w-full text-sm">
        <thead class="bg-gray-100">
          <tr>
            <th class="p-3 text-left">Options Concept</th>
            <th class="p-3 text-left">Recognition Analog</th>
            <th class="p-3 text-left">Interpretation</th>
          </tr>
        </thead>
        <tbody class="divide-y">
          <tr>
            <td class="p-3 font-mono">Underlying (S)</td>
            <td class="p-3 font-mono">T(a,B)</td>
            <td class="p-3 text-xs">Recognition allocated to beneficial partners</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Option Value (V)</td>
            <td class="p-3 font-mono">P(G)</td>
            <td class="p-3 text-xs">Goal achievement probability (payoff)</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Strike (K)</td>
            <td class="p-3">Threshold</td>
            <td class="p-3 text-xs">Minimum T(B) needed for meaningful P(G)</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Time to Expiry (t)</td>
            <td class="p-3 font-mono">t</td>
            <td class="p-3 text-xs">Time horizon for goal achievement</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Volatility (σ)</td>
            <td class="p-3">Network stability</td>
            <td class="p-3 text-xs">Uncertainty in partner reliability</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Interest Rate (r)</td>
            <td class="p-3">Opportunity cost</td>
            <td class="p-3 text-xs">Alternative uses of recognition</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Delta hedging</td>
            <td class="p-3">Rebalancing T(B)</td>
            <td class="p-3 text-xs">Adjusting recognition as conditions change</td>
          </tr>
          <tr>
            <td class="p-3 font-mono">Gamma scalping</td>
            <td class="p-3">Dynamic allocation</td>
            <td class="p-3 text-xs">Exploiting convexity in P(G) function</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</div>