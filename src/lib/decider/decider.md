# Decider

## What is Decider?

**Decider is a structured way for groups to make decisions together fairly.**

Think of it like a guided conversation where everyone gets a voice, concerns are heard, and the group finds the best solution together—not just the most popular one.

***

### Why Decider Exists

When groups try to make decisions, they often run into problems:

* **The loudest voice wins** instead of the best idea
* **Good ideas get shot down** before they're fully understood
* **People don't explain their concerns** clearly
* **Quick votes miss important details** that come up later
* **Someone has to be "in charge"** of running the process

Decider solves these by giving everyone a clear path to contribute, challenge, improve, and ultimately support the best options.

***


### Summary

**Process Flow:**

```
For each agenda item:
  1. Express Proposals (all players)
     ↓
  2. Express Challenges (for each proposal)
     ├─ No challenges → PASS (exit early)
     └─ Has challenges → continue
        ↓
  3. Express Comments & Modifications
     ├─ No modifications → PASS as-is (exit early)
     └─ Has modifications → continue
        ↓
  4. Express Support (distribute points)
     ↓
  5. Calculate most supported version → PASS
```

**Core Components:**

1. **Players:** List of participants in the decision-making process
2. **Agenda:** Array of prompts/questions to decide on
3. **Passed Proposals:** Record of all proposals that have completed the process
4. **Time Windows:** Configurable duration for each phase (default: 24 hours / 86400000 milliseconds)

**Data Structures:**

Each **Proposal** contains:
* `content`: The current/final version of the proposal text
* `challenges`: Array of challenge strings raised by players
* `comments`: Array of comment strings from players
* `modificationProposals`: Array of alternative version strings
* `supportExpressions`: Array of objects mapping candidate versions to point values

***

### The Key Insight

**Decider isn't about voting—it's about collaboration.**

Traditional voting is binary: yes or no, winner or loser. It creates divisions.

Decider is about **iterative refinement**: propose, learn, improve, support. It creates alignment.

The best ideas aren't the ones people start with—they're the ones that emerge after everyone has contributed their knowledge, concerns, and creativity.

That's what Decider makes possible.

***

### The Complete Process

Let's walk through how three friends—**Alice, Bob, and Carol**—use Decider to figure out what to have for dinner.

#### **Step 1: Setting Up**

Alice wants to organize the decision. She:

1. Picks a question: "What should we have for dinner?"
2. Invites Bob and Carol to participate
3. Sets a time window (e.g., 24 hours) for each phase
4. Shares this information with everyone

Everyone agrees they're deciding together and can see who's involved.

**Why this matters:** Everyone knows what they're deciding, who's participating, and how long they have to contribute. No confusion about the goal or timeline.

***

#### **Step 2: Everyone Proposes Ideas** (Time Window: 24 hours)

Now each person shares their initial idea within the time window:

* **Alice suggests:** "Pizza"
* **Bob suggests:** "Sushi"
* **Carol suggests:** "Tacos"

Everyone can see all three ideas as they come in. Once the time window closes or everyone has submitted, the process moves to the next phase.

**Why this matters:** Getting all ideas on the table first, before any discussion or criticism, ensures no one holds back. Every voice is equal at this stage. The time window ensures the process keeps moving forward.

***

#### **Step 3: Raising Challenges** (Time Window: 24 hours)

Now people can raise concerns about any proposal. This isn't about attacking ideas—it's about surfacing important information.

* **Bob raises a concern about Alice's pizza:** "Pizza has gluten, and Carol is allergic to gluten."
* **Alice raises a concern about Bob's sushi:** "Sushi is really expensive, and we said we're on a budget this week."
* **No one raises concerns about Carol's tacos**

**Important:** If no one raises any challenges to a proposal within the time window, that proposal **passes immediately** and becomes an active decision. The remaining steps are skipped for that proposal.

In this case, Carol's tacos received no challenges, so they pass directly to the final decisions without needing further discussion.

**Why this matters:** Often, the person suggesting an idea doesn't know about problems others can see. This phase brings important facts to light before anyone commits to a decision. But if everyone agrees from the start, the process doesn't waste time—the proposal passes immediately.

***

#### **Step 4: Discussion and Comments** (Time Window: 24 hours)

For proposals that received challenges, people now discuss them to add context:

* **Carol comments on Bob's concern:** "Thanks Bob! I didn't know Alice forgot about my allergy. Good catch."
* **Alice adds to her concern about sushi:** "What if we bought grocery store sushi instead of going to a restaurant? That would be cheaper."

**Why this matters:** Not every concern kills an idea. Discussion helps everyone understand the full picture and sometimes reveals simple solutions to problems.

***

#### **Step 5: Proposing Improvements** (Time Window: 24 hours)

Based on what they learned, people can suggest modified versions of the original ideas:

* **Alice modifies her pizza idea:** "Gluten-free pizza" (addresses Bob's concern)
* **Bob modifies his sushi idea:** "Grocery store sushi rolls" (addresses Alice's budget concern)

**Important:** If no one proposes any modifications to a challenged proposal within the time window, that proposal **passes as-is** and becomes an active decision. The support expression step is skipped.

If modifications are proposed, the group now has multiple versions to choose from:

* Pizza OR gluten-free pizza
* Sushi OR grocery store sushi rolls

These move to the next phase for support expression.

**Why this matters:** This is where good ideas get better. Instead of just voting "yes/no" on original proposals, people can adapt ideas based on feedback. Collaboration improves outcomes. But if the group is satisfied with the original despite challenges, it can pass without modification.

***

#### **Step 6: Expressing Support** (Time Window: 24 hours)

Now comes the crucial part: **everyone distributes support points** across ALL the candidate versions for each proposal that had modifications.

This isn't simple voting. Each person gives more points to options they support more strongly.

For Alice's pizza proposal (had modifications):

**Alice's support:**
* Original pizza: 0 points
* Gluten-free pizza: 7 points

**Bob's support:**
* Original pizza: 0 points
* Gluten-free pizza: 8 points

**Carol's support:**
* Original pizza: 0 points
* Gluten-free pizza: 9 points

For Bob's sushi proposal (had modifications):

**Alice's support:**
* Original sushi: 2 points
* Grocery store sushi: 8 points

**Bob's support:**
* Original sushi: 1 point
* Grocery store sushi: 7 points

**Carol's support:**
* Original sushi: 0 points
* Grocery store sushi: 10 points

**Note:** Carol's tacos don't appear here because they passed immediately in Step 3 (no challenges were raised).

**Why this matters:** Point distribution reveals preference intensity and allows the group to choose between the original and modified versions. This captures nuance that simple yes/no votes miss.

***

#### **Step 7: Determining Final Versions**

Now Decider adds up all the points for each proposal's candidates:

**For Alice's pizza proposal:**
* Original pizza: 0 + 0 + 0 = 0 points
* Gluten-free pizza: 7 + 8 + 9 = **24 points** ← Winner

**Final version:** "Gluten-free pizza" passes as the active decision.

**For Bob's sushi proposal:**
* Original sushi: 2 + 1 + 0 = 3 points
* Grocery store sushi: 8 + 7 + 10 = **25 points** ← Winner

**Final version:** "Grocery store sushi rolls" passes as the active decision.

**For Carol's tacos proposal:**
* Already passed in Step 3 (no challenges)

**Final version:** "Tacos" is the active decision.

**The group's final decisions:** All three proposals have passed and are now active decisions:
1. Gluten-free pizza
2. Grocery store sushi rolls  
3. Tacos

**Why this matters:** Each proposal's most supported version becomes the active decision. Everyone participated in shaping the outcomes. The group has genuine consensus, not just bare majorities.

***

### Working Through an Agenda

In the example above, we focused on a single question: "What should we have for dinner?"

But Decider can handle **multiple decision items** through an **agenda**.

The agenda is simply a list of questions or topics that need decisions. The group works through them one at a time, applying the full Decider process to each item.

**Example Agenda:**
1. "What should we have for dinner?"
2. "What movie should we watch?"
3. "When should we meet next?"

Each item goes through the complete process:
* Everyone proposes ideas
* Challenges are raised
* Comments and modifications happen
* Support is expressed
* The most supported version becomes the decision

Once an item is complete, the group moves to the next agenda item.

**Why this matters:** Groups often need to make multiple related decisions. The agenda structure keeps things organized and ensures every decision gets proper attention. Each decision is tracked separately in the list of passed proposals.

***

### Time Windows Explained

Each phase of the Decider process has a **time window** (by default, 24 hours).

**How it works:**
* A phase opens and everyone can contribute
* The phase closes when either:
  * The time window expires, OR
  * Everyone has submitted their contribution

**Why time windows matter:**
* **Keeps momentum:** Decisions don't stall waiting for one person
* **Gives everyone time:** No one is rushed or excluded
* **Accommodates schedules:** People can participate across time zones and availability
* **Creates clarity:** Everyone knows the deadline

Time windows can be adjusted based on the group's needs:
* Urgent decisions: shorter windows (e.g., 1 hour)
* Complex decisions: longer windows (e.g., 1 week)
* The same window length applies to all phases for consistency

***

### How the Peer-to-Peer Part Works

Here's the clever part: **there's no central authority or server**.

Think of it like this:

**Traditional Group Decision (Centralized):**

* Everyone sends their ideas to one person (the organizer)
* That person collects everything
* That person shares it back with the group
* If the organizer disappears, everything is lost

**Decider Way (Peer-to-Peer):**

* Everyone keeps their own notes on their own device
* Everyone shares their notes directly with each other
* Everyone can see everyone else's notes in real-time
* No one is "in charge"—everyone has equal access to all information
* If one person disappears, everyone else still has all the information

It's like if you and your friends were all looking at the same whiteboard, but each person draws on their own section, and everyone can see the whole board automatically.

**Benefits:**

* **No single point of failure:** No one person controls everything
* **Transparent:** Everyone sees everything as it happens
* **Equal:** No hierarchy—everyone has the same power
* **Private:** Your personal data stays on your device, shared only with people you choose

***

### Efficiency Features: Early Exits

Decider doesn't force groups through unnecessary steps. Proposals can **pass early** if there's agreement:

**Exit Point 1: After Challenges (Step 3)**
* If NO challenges are raised → proposal passes immediately
* Remaining steps are skipped
* The original proposal becomes the active decision

**Exit Point 2: After Modifications (Step 5)**
* If challenges were raised BUT no modifications are proposed → proposal passes as-is
* Support expression is skipped
* The original proposal becomes the active decision

**Exit Point 3: After Support Expression (Step 6)**
* If modifications were proposed → support expression happens
* The most supported version becomes the active decision

**Why this matters:**
* **Saves time:** Groups don't waste effort on unnecessary steps
* **Respects consensus:** If everyone agrees, the process moves quickly
* **Focuses energy:** Complexity is only added when needed
* **Natural flow:** The process adapts to the level of disagreement

Think of it like this: if everyone already agrees, why force them through more steps? But if there's disagreement, the process provides structure to work through it.

***

### The Big Picture

Decider transforms how groups make decisions by:

1. **Giving structure to chaos:** Instead of endless discussion, there's a clear path forward with defined phases
2. **Ensuring everyone is heard:** The phases guarantee every voice matters within clear time windows
3. **Encouraging improvement:** Ideas evolve rather than just win or lose through challenges and modifications
4. **Building consensus:** The result has genuine group support through point distribution
5. **Removing gatekeepers:** No one person controls the process—it's peer-to-peer
6. **Being efficient:** Early exits mean groups don't waste time when there's already agreement
7. **Tracking outcomes:** All passed proposals are recorded so the group has a clear record of decisions

**Without Decider:**

* Discussions go in circles
* Good ideas get dismissed too quickly
* Strong personalities dominate
* People feel unheard
* Decisions feel arbitrary
* No clear record of what was decided
* Processes drag on unnecessarily

**With Decider:**

* Clear progression from ideas to decision
* Time-bounded phases keep things moving
* Concerns are systematically addressed
* Everyone contributes equally
* Ideas improve through collaboration
* Decisions feel fair and well-considered
* Complete audit trail of all passed proposals
* Process adapts to the level of disagreement

***

### Real-World Uses

**Small Groups:**

* Friends deciding where to travel
* Families planning meals or activities
* Roommates setting house rules

**Teams:**

* Which project to work on next
* How to approach a problem
* Setting team priorities

**Communities:**

* Deciding community guidelines
* Allocating shared resources
* Planning events

**Organizations:**

* Policy decisions
* Strategic planning
* Resource allocation

Anywhere a group needs to make a decision together, Decider provides a fair, structured way to do it.

***
