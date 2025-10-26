# The Compute Folder: Post-App-italism Computing

*Building a global computational commons where programs, data, and computation are public goods*

## What Is This?

This is **distributed computing infrastructure** that turns **millions of devices into a shared computational commons**. Think of it as:

- **Distributed storage** (Holster/Gun) + **Distributed computation** (RDL) = **A new computing paradigm**
- Like **Excel that runs across the entire internet**, where anyone can write formulas, subscribe to results, and verify calculations
- A **global network of verifiable computation** running for free in browsers and on disk
- **Post-app-italism**: Breaking out of the app prison into open, composable, public computation

Instead of apps as **private property** (closed, siloed, extractive), we have computation as a **commons** (open, composable, verifiable, free).

---

## The Problem: The App Prison

### How We Compute Today

Every app is a **walled garden**:

```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│   Facebook      │  │   Google        │  │   Notion        │
│                 │  │                 │  │                 │
│ • Your data     │  │ • Your data     │  │ • Your data     │
│ • Their code    │  │ • Their code    │  │ • Their code    │
│ • Their servers │  │ • Their servers │  │ • Their servers │
│ • Can't export  │  │ • Can't verify  │  │ • Can't compose │
│ • Can't remix   │  │ • Can't extend  │  │ • Can't share   │
└─────────────────┘  └─────────────────┘  └─────────────────┘
     Isolated            Isolated            Isolated
```

**Problems:**
- **Private property model**: Apps own your data, computation, and results
- **Black boxes**: Can't see how they calculate, can't verify results
- **Non-composable**: Can't use Facebook's data in Notion, can't extend Google's calculations
- **Extractive**: Pay for servers, pay for features, pay for API access
- **Centralized**: Company controls everything, can shut down, change rules, raise prices
- **Fragmented**: Every app reinvents the wheel, no shared infrastructure

### What We Lost

Before apps, we had:
- **Files** - data you could move, copy, remix
- **Programs** - code you could see, modify, share
- **Protocols** - standards that let things interoperate (email, web, RSS)

Apps locked everything back up. We traded **interoperability** for **convenience**, and **freedom** for **ease of use**.

---

## The Solution: Computational Commons

### A New Computing Paradigm

Instead of isolated apps, imagine:

```
┌──────────────────────────────────────────────────────────────┐
│                   GLOBAL COMPUTATIONAL COMMONS                │
│                                                               │
│  Anyone can:                                                  │
│  • Store data (distributed, encrypted, yours)                │
│  • Write programs (declarative, like Excel formulas)         │
│  • Run computations (free, on any device)                    │
│  • Subscribe to results (reactive, automatic updates)        │
│  • Verify everything (cryptographic provenance)              │
│  • Compose freely (use anyone's outputs as your inputs)      │
│                                                               │
│  All:                                                         │
│  • Free (P2P, no servers)                                    │
│  • Open (see all code, see all data)                         │
│  • Verifiable (check any calculation)                        │
│  • Composable (build on anyone's work)                       │
└──────────────────────────────────────────────────────────────┘
         ↓                    ↓                    ↓
   Your Browser         Their Browser        Another Device
   (IndexedDB)          (IndexedDB)          (Disk storage)
   
   All running the same programs, all seeing the same data,
   all verifying each other's work, all for free.
```

### The Three Layers

**Layer 1: Distributed Storage (Holster/Gun)**
- Your data lives on **your device** (browser, phone, computer)
- **Encrypted** peer-to-peer synchronization
- No servers, no company in the middle
- Like Dropbox, but decentralized and free

**Layer 2: Distributed Computation (RDL - This Folder)**
- Your programs run on **your device** (and mine, and theirs)
- **Declarative** programs (like Excel formulas, not step-by-step code)
- **Reactive** execution (auto-runs when data changes)
- **Verifiable** results (cryptographic provenance)
- Like Excel + Google Sheets, but decentralized and for any computation

**Layer 3: Public Data Commons**
- Everyone's **results are visible** (if they choose)
- Everyone's **programs are visible** (always)
- Everything is **verifiable** (check the math yourself)
- Everything is **composable** (use others' outputs as your inputs)
- Like the web itself, but for computation not just documents

---

## How It Works: Excel for the Internet

### The Spreadsheet Analogy (Deep Dive)

**Excel (Local)**:
```
Cell A1: 10
Cell A2: 20
Cell A3: =A1 + A2
Result:  30
```

When you change A1 to 15, A3 automatically updates to 35. **Reactive.**

**Our System (Distributed)**:
```
Alice's data (in her browser):
  ~/alice/offers = 10

Bob's data (in his browser):
  ~/bob/offers = 20

Charlie's program (running in his browser):
  inputs:
    alice: subscription to ~/alice/offers
    bob: subscription to ~/bob/offers
  compute: (alice, bob) => alice + bob
  output: ~/charlie/total
  
Charlie's result: 30
```

When Alice changes her offers to 15:
1. **Alice's device** updates `~/alice/offers` = 15
2. **P2P sync** propagates to network (Holster)
3. **Charlie's device** sees the change (subscribed)
4. **Charlie's program** auto-recomputes: 15 + 20 = 35
5. **Charlie's result** updates: `~/charlie/total` = 35
6. **Anyone subscribed to Charlie** sees the new result

**All automatic. All reactive. All verifiable. All free.**

### The Key Insight

**Every device is a spreadsheet cell.**

- Your device holds **your data** (like a cell value)
- Your device runs **your programs** (like formulas)
- Changes propagate **automatically** (like Excel)
- Everyone can **subscribe** to anyone (like referencing other cells)
- Everything is **cryptographically verified** (unlike Excel!)

But instead of one spreadsheet with 1,000 cells, it's **millions of devices**, each a cell, all computing together.

---

## What You Can Build

### Not Just Specific Apps

The power isn't in building "a time bank app" or "an allocation app." The power is in creating **infrastructure** where:

1. **Anyone can write programs** (declarative, like Excel formulas)
2. **Anyone can run programs** (free, in their browser)
3. **Anyone can subscribe to results** (reactive, automatic)
4. **Anyone can verify computations** (cryptographic provenance)
5. **Anyone can compose programs** (use outputs as inputs)

### Examples of What This Enables

**1. Collaborative Calculations**
- I track expenses, you track income
- A shared program calculates our budget
- Updates automatically when either of us changes data
- Anyone can audit: "show me the calculation"

**2. Distributed Data Analysis**
- 1,000 people contribute local data (stays on their devices)
- A program computes aggregate statistics
- Results are public, source data is private
- Provenance proves the calculation is correct

**3. Chain Computations**
- Program A: Computes mutual recognition from commitments
- Program B: Uses A's results to calculate priorities  
- Program C: Uses B's results to make allocations
- Each is independent, all compose automatically

**4. Living Data Pipelines**
- Alice publishes sensor data (temperature, air quality)
- Bob's program processes it (rolling averages, alerts)
- Carol's program aggregates Bob's results (city-wide stats)
- Dave's program visualizes Carol's aggregates (public dashboard)
- **All automatic, all reactive, all verifiable, all free**

**5. Open Algorithms**
- Someone writes a better allocation algorithm
- They publish it as a program
- Communities can **compare results** (old vs new algorithm)
- **Verify correctness** (check the provenance)
- **Fork and modify** (make their own version)
- **No permission needed** (it's a commons)

### The Meta-Pattern

Every time someone writes a program and publishes results:
1. **Others can subscribe** (use the results)
2. **Others can verify** (check the math)
3. **Others can fork** (create variations)
4. **Others can compose** (chain programs together)

It's like **open-source for live computation**, not just code.

---

## Breaking the App Prison

### What Apps Do

Traditional apps create **artificial scarcity**:

- **Data scarcity**: Your data is locked in their database
- **Computation scarcity**: Can only run their code on their servers
- **Access scarcity**: Pay for API calls, pay for features
- **Verification scarcity**: Can't see how they calculate, must trust them
- **Composition scarcity**: Can't mix their data with others' data

This is **computation as private property**. Apps own the means of computation.

### What This System Does

The computational commons creates **abundance**:

- **Data abundance**: Your data, on your device, shareable with anyone
- **Computation abundance**: Run any program on any device for free
- **Access abundance**: Everything is an open API (subscribe to any result)
- **Verification abundance**: Check any calculation, see any provenance
- **Composition abundance**: Any program can use any other program's results

This is **computation as commons**. Everyone shares the means of computation.

### Post-App-italism

The app model is **capitalism applied to software**:
- Apps are products (buy/rent them)
- Data is property (companies own it)
- Computation is labor (you pay for it)
- APIs are trade (controlled, metered, priced)

The computational commons is **a different model entirely**:
- Programs are recipes (anyone can use them)
- Data is speech (you publish what you want)
- Computation is collaboration (we compute together)
- Everything is composable (no permission needed)

**Post-app-italism**: We move beyond the app-as-commodity model to computation-as-commons.

---

## The Technical Magic (Simplified)

### How Can This Possibly Work?

**Q: Who pays for computation?**
**A: Nobody.** It runs on participants' devices (browsers, phones, computers). Like BitTorrent—the network is the infrastructure.

**Q: How do you trust the results?**
**A: Cryptographic provenance.** Every computation creates an immutable record:
```
Event = {
  id: hash(entire event),           // Content-addressed (tamper-proof)
  author: digital signature,         // Cryptographic proof of who
  payload: inputs + outputs,         // What happened
  itc: logical timestamp,            // When (without global clock)
  parents: [previous event hashes]   // Where it came from (hash-linked)
}

Provenance chain:
Event A → Event B → Event C
 ↓         ↓         ↓
All cryptographically linked
Change anything → breaks the chain
```

**Q: What if someone cheats?**
**A: They get caught.** The system uses:
- **Content-addressing**: Event ID = hash of content (can't fake)
- **Digital signatures**: ECDSA proves authorship (can't forge)
- **Merkle-DAG**: Hash-linking proves lineage (can't tamper)
- **ITC causality**: Logical time proves ordering (can't reorder)
- **ZK proofs**: Prove correctness without revealing data (privacy + verification)

If provenance doesn't match, the fraud is mathematically provable.

**Q: How fast is it?**
**A: Fast enough.** Updates propagate in seconds. Computations run in milliseconds. Verifications are instant. For coordination (not high-frequency trading), it's perfect.

**Q: What about privacy?**
**A: You choose.** 
- Keep everything local (default)
- Share results publicly, keep source data private
- Generate ZK proofs (prove correctness without revealing values)
- Encrypt for specific people
- It's your device, your data, your choice

**Q: How does it scale?**
**A: Peer-to-peer.** Every participant adds capacity (storage + computation). Like the web—adding users adds infrastructure.

---

## The Distributed Computing Layer

### Holster: Distributed Storage

**What it provides:**
- Every user has encrypted storage (their "space")
- Peer-to-peer synchronization (like Dropbox, but decentralized)
- Path-based addressing (`~/alice/data/item`)
- Subscriptions (watch for changes)
- No servers, no company, no fees

**Limitations:**
- Only stores data, doesn't compute
- No verification (you trust the data you receive)
- No composition (data just sits there)

### RDL: Distributed Computation

**What this folder adds:**
- Programs that describe computations (like Excel formulas)
- Automatic execution when data changes (reactive)
- Provenance tracking (cryptographic verification)
- Composition (use others' results as inputs)
- Program registry (discover and deploy programs)

**Together:**
- Holster = distributed hard drive
- RDL = distributed CPU + memory
- Combined = **distributed computer**

### The Result: Global Computational Commons

Anyone can:
1. **Store data** (on Holster)
2. **Write programs** (in RDL)
3. **Run computations** (on their device)
4. **Publish results** (to Holster)
5. **Subscribe to others** (automatic updates)
6. **Verify everything** (check provenance)
7. **Compose freely** (chain computations)

All for **free**. All **verifiable**. All **composable**.

It's like having **millions of computers, all running different programs, all sharing results, all checking each other's work, all automatically synchronized**.

A **planetary-scale computational commons**.

---

## Why This Changes Everything

### 1. Free Infrastructure

**Before:** Want to run a coordination system? Need:
- Servers ($50-$500/month)
- Database ($$)
- Scaling ($$$$)
- Maintenance ($$$$$)

**After:** 
- Zero servers (P2P)
- Zero database hosting (local storage)
- Zero scaling costs (each user adds capacity)
- Zero maintenance (no central infrastructure)

**Result:** Complex coordination becomes **economically accessible** to anyone.

### 2. Verifiable Computation

**Before:** Using a service? You trust them:
- Trust their algorithm (you can't see it)
- Trust their results (you can't verify)
- Trust they won't change things (no transparency)

**After:**
- See the program (it's published)
- Check the results (cryptographic provenance)
- Verify independently (run it yourself)

**Result:** **Trust through verification**, not through authority.

### 3. Composable Programs

**Before:** Want to combine two services?
- Need both companies to agree
- Pay for API access
- Hope they don't break compatibility
- Limited to what they expose

**After:**
- Subscribe to anyone's results
- Use outputs as inputs
- Chain programs freely
- Fork and modify

**Result:** **Innovation becomes permissionless**. Build on anything, no approval needed.

### 4. Distributed Experimentation

**Before:** New idea for coordination?
- Convince people to try your app (hard)
- Maintain separate infrastructure (expensive)
- Hope for adoption (low odds)

**After:**
- Write a program (minutes)
- Deploy to the commons (seconds)
- Let others subscribe and compare (automatic)
- Fork successful programs (easy)

**Result:** **Rapid evolution** of coordination systems. Try 100 algorithms, keep what works.

### 5. Data as Public Good

**Before:** Data locked in corporate databases
- Can't see it
- Can't verify it
- Can't use it
- Can't build on it

**After:** Data published to the commons (optionally)
- Public results (anyone can subscribe)
- Verifiable provenance (check the lineage)
- Composable (use as input to new programs)
- Remixable (fork and extend)

**Result:** **Knowledge accumulates** instead of being siloed. Every computation builds on previous computations.

---

## Real-World Transformation

### Not Apps, But Infrastructure

This isn't "build a time bank app" or "build an allocation app." 

This is: **Build infrastructure where anyone can create time banks, allocation systems, or anything else requiring coordination—without servers, without companies, without permission.**

### The Shift

**App thinking:**
- "We need a time bank app"
- Someone builds it (6 months, $50k)
- Everyone uses that one app
- If it shuts down, everyone loses

**Infrastructure thinking:**
- "We need computational infrastructure"
- Someone builds it once (this folder)
- Anyone writes time bank programs (minutes)
- Programs live forever (on the commons)
- Communities choose which programs to run
- Fork, modify, improve continuously

### What This Enables

**1. Computational Literacy**
Like Excel made "programming" accessible to accountants:
- Write programs by filling in schemas (like spreadsheet formulas)
- See immediate results (like cells updating)
- Share programs (like sharing spreadsheets)
- No "real programming" needed

**2. Democratic Algorithms**
Communities can:
- See exactly how algorithms work (programs are public)
- Verify results (provenance is checked)
- Propose alternatives (fork and modify)
- Compare outcomes (run both, see the difference)
- Vote with their devices (deploy what they prefer)

**3. Algorithmic Commons**
Instead of companies owning algorithms:
- Programs are public (anyone can read)
- Programs are free (anyone can run)
- Programs are forkable (anyone can modify)
- Programs are composable (anyone can chain)

Like **open-source, but for live, running, reactive computation**.

**4. Emergent Complexity**
Simple programs compose into complex systems:
- Alice: Tracks offers
- Bob: Tracks needs
- Carol: Matches offers to needs
- Dave: Prioritizes by urgency
- Eve: Handles time slots
- Frank: Resolves conflicts

Each is simple. Together: **sophisticated coordination system**.

No central coordinator needed. Each program does one thing well.

**5. Global Collaboration**
Programs from different communities can interact:
- Timebank in Seattle publishes hours
- Housing co-op in Berlin publishes space
- Someone writes a program: "Match Seattle hours to Berlin space"
- **Cross-community coordination emerges automatically**

Because everything is open, everything can compose.

---

## The Commons Metaphor

### Traditional Commons

**Physical commons** (historical):
- Common land (everyone can graze sheep)
- Common resources (everyone can fish)
- Common knowledge (everyone can learn)

**Problems:**
- Rivalry (your sheep eat grass I wanted)
- Excludability (hard to keep people out)
- Tragedy of the commons (overuse)

### Computational Commons (This System)

**Digital commons** (new):
- Common storage (Holster—everyone stores data)
- Common computation (RDL—everyone runs programs)
- Common verification (Provenance—everyone checks work)

**Advantages:**
- **Non-rivalrous**: Your use doesn't diminish mine (infinite copies)
- **Non-excludable by design**: Anyone can participate (P2P)
- **Anti-tragedy**: More users = more capacity (network effects)

### Why It Works

Traditional commons fail because resources are **scarce** and **rivalrous**.

Computational commons succeed because resources are **abundant** and **non-rivalrous**:
- **Storage**: Everyone brings their own device
- **Computation**: Everyone's device can compute
- **Programs**: Infinite copies, zero marginal cost
- **Results**: Can be shared infinitely
- **Verification**: Can be checked infinitely

**Adding users adds capacity, not consumption.**

The more people who join, the more powerful it becomes. That's why it's a **commons**, not a service.

---

## Breaking Out of the App Prison

### The App Model Trap

Apps train us to think:
- "There's an app for that" (one tool per task)
- "Download and install" (siloed, isolated)
- "Pay or tolerate ads" (extraction required)
- "Use what they give you" (no modification)
- "Data stays with them" (you're the product)

### The Commons Model Freedom

This system trains us to think:
- "There's a program for that—or write one" (composable tools)
- "Subscribe and compute" (open, connected)
- "Free infrastructure" (cooperation, not extraction)
- "Fork and modify" (remixable)
- "Data stays with you" (you own it)

### Post-App-italism in Practice

**Instead of:**
```
Company owns app → You pay rent → You use their computation
Company owns data → They monetize → You're the product
Company controls access → They gate features → You're locked in
```

**We have:**
```
Commons owns programs → You run locally → Computation is free
You own data → You choose to share → You're autonomous
Network enables access → Everything's open → You can fork
```

This is **post-capitalism for computation**—moving from private property to commons, from extraction to collaboration, from scarcity to abundance.

---

## The Vision: A Planetary Computer

### What We're Building

Not an app. Not a platform. **Infrastructure for a planetary-scale computer** where:

- **Every device is a node** (browser, phone, laptop, server)
- **Every person is a programmer** (declarative schemas, not code)
- **Every computation is verified** (cryptographic provenance)
- **Every result is public** (if you choose)
- **Every program is composable** (outputs → inputs)
- **Everything is free** (P2P, no rent-seeking)

### The Analogy

**The web** created a **planetary document system**:
- Anyone can publish documents (HTML)
- Anyone can link to documents (URLs)
- Anyone can read documents (browsers)
- No permission needed (open protocols)

**This system** creates a **planetary computation system**:
- Anyone can publish programs (RDL)
- Anyone can subscribe to results (Holster paths)
- Anyone can run computations (browsers + IndexedDB)
- No permission needed (open protocols)

### The Implications

**Technical:**
- Distributed computing becomes trivial
- Verification becomes automatic
- Composition becomes default
- Infrastructure becomes free

**Social:**
- Communities coordinate without companies
- Algorithms become democratic
- Knowledge accumulates as commons
- Innovation becomes permissionless

**Economic:**
- Coordination cost → zero
- Infrastructure cost → zero
- Verification cost → zero
- Composition cost → zero

**Political:**
- No centralized control
- No single point of failure
- No extractive intermediaries
- No permission gatekeepers

---

## Getting Started (Conceptually)

### For Individuals

1. **Run programs:** Choose programs from the commons, deploy them locally
2. **Publish data:** Share what you want, keep what you don't
3. **Subscribe to others:** Get results from programs you care about
4. **Verify everything:** Check provenance, trust through math

### For Communities

1. **Deploy coordination systems:** Choose programs for your needs
2. **Configure schemas:** Set parameters without programming
3. **Share results:** Publish outcomes for transparency
4. **Fork and improve:** Modify programs to fit your values

### For Developers

1. **Write programs:** Create RDL computation graphs
2. **Register functions:** Add computation logic to the registry
3. **Publish to commons:** Share programs for others to use
4. **Compose and extend:** Build on others' programs

### For Humanity

**This is infrastructure for post-app-italism.**

Where computation is a commons, not a commodity.  
Where verification replaces trust.  
Where composition is default.  
Where innovation is permissionless.  
Where abundance replaces artificial scarcity.

---

## Frequently Asked Questions

### "Isn't this just blockchain?"

**No.** Blockchain is:
- Slow (seconds per transaction)
- Expensive (gas fees)
- Wasteful (proof-of-work)
- Limited (simple computations only)

This is:
- Fast (milliseconds)
- Free (P2P, no fees)
- Efficient (computation only when needed)
- Powerful (arbitrary computations)

We get verification without consensus, provenance without blockchain.

### "How is this different from IPFS?"

IPFS is **content-addressed storage**—like a distributed hard drive.

This is **content-addressed storage + verified computation**—like a distributed computer.

IPFS stores files. This runs programs.

### "What about WebAssembly/WASM?"

WASM is **how to run code efficiently** in browsers.

This is **what code to run and how to coordinate it** across millions of browsers.

WASM is execution. This is coordination + verification + composition.

They could work together—RDL programs could run WASM modules!

### "Isn't this too complex for non-programmers?"

**Right now, yes.** Like Excel in 1987—powerful but rough.

**Future:** Visual builders, templates, marketplaces of programs. Fill in forms, not write code.

**The infrastructure is production-ready.** The UI/UX needs work. But that's easier than building the infrastructure.

### "What's the catch?"

**It's early.** 

- Core infrastructure works (this folder)
- Tooling for non-programmers is nascent
- Network effects haven't kicked in yet
- Documentation is technical (we're fixing this!)

It's like the web in 1992—foundation solid, but needs time to grow.

---

## What's In This Folder

This is the **RDL runtime**—the distributed computation layer:

### Core Files

**`schema.ts`** - The language definition
- Defines what RDL programs look like
- Like defining Excel's formula syntax
- Canonical specification (Zod schemas)

**`compute.svelte.ts`** - The interpreter
- Runs RDL programs
- Like Excel's calculation engine
- Handles reactivity, subscriptions, provenance

**`runtime-manager.svelte.ts`** - Program lifecycle
- Deploy, start, stop, monitor programs
- Like an OS process manager

**`kernel-core.ts`** - Language-agnostic infrastructure
- Program registry (any language)
- Subscriptions, causality, provenance
- The "operating system" layer

**`kernel-rdl.ts`** - RDL-specific storage
- RDL program storage and state
- Compute namespace management

**`program-hash.svelte.ts`** - Content addressing
- Unique IDs for programs
- Deterministic hashing
- Like git for programs

### The Stack

```
User's Programs (RDL)
        ↓
Runtime Manager (lifecycle)
        ↓
Compute Runtime (execute RDL)
        ↓
Kernel (program management)
        ↓
Store Layer (Holster integration)
        ↓
Holster/Gun (P2P storage)
        ↓
IndexedDB / Disk
```

Each layer is language-agnostic except the RDL-specific pieces. Could add SQL, WASM, or any language.

---

## Conclusion: Post-App-italism

We're building **infrastructure for a world beyond apps**.

Where:
- **Computation is a commons**, not a commodity
- **Verification replaces trust**
- **Composition is permissionless**
- **Innovation is decentralized**
- **Infrastructure is free**

This is **Excel for P2P coordination**, but that undersells it.

It's **computational infrastructure for planetary-scale collaboration**.

It's **breaking the app prison** and revealing what computing could be if it weren't trapped in the extractive, siloed, proprietary model.

It's **post-app-italism**—moving from computation-as-private-property to computation-as-commons.

The code in this folder doesn't just enable decentralized coordination.

**It creates a new computing paradigm entirely.**

---

## Learn More

- **RDL Language:** `compute/docs/RDL-LANGUAGE-ANALYSIS.md`
- **Architecture:** `compute/docs/LANGUAGE-AGNOSTIC-BOUNDARIES.md`
- **Vision:** `compute/docs/goal.md`
- **User Space:** `gitbook/user-space-structure.md`

Welcome to the computational commons. 🌍✨
