## Free-Association: A Call for Mutual Self-Actualization 🌱 [![](https://img.shields.io/opencollective/backers/playnet)](https://opencollective.com/playnet#section-contributors) [![Join us on Telegram](https://img.shields.io/badge/Join-Telegram-blue?logo=telegram)](https://t.me/+jS7u3ZcKLoxmYWU5)

_Free-Association_ is literally an actual alternative to capitalism that allows for the _spontaneous self-actualization of all through each other_ without any centralized control, planning, private-property, state intervention!

This requires more than mere abstract agreement or forced cooperation - it demands real contribution to each other's self-actualization (tangible and intangible contributions towards needs, values, goals, social and/or material dependencies etc.).

**_free-association_** essentially unifies the concepts of **_mutual-aid_**, **_mutual-recognition_**, **_mutual-fulfillment_**, and **capacity-distribution\_** in a single elegant and coherent framework that creates the mathematical foundations for a scalable gifting economy of co-creative abundance.

You can interact with an interface implementing this logic at [interplaynetary.github.io/free-association](https://interplaynetary.github.io/free-association/) (in active development) (p2p, distributed, running in the browser - no need for a download - for free)

### Here's how it works:

**Self-actualization** is self-defined (subjective), but its realization depends on objective access to capacities (food, skills, etc.).

1. Your **Total-Recognition** is always _100%_.

2. Your **Recognition** is your acknowledgment of _contributions towards your own self-actualization_
   - Always represents a share/portion/percentage of one's total recognition (100%)
   - Non-transferable (unlike shares or equity that can be traded)
   - Dynamically (re)adjustable as relationships and contributions evolve
   - Not limited to direct personal consumption - recognition can be given to those contributing to broader social values and needs you care about
   - Organized as a **contribution tree** that tracks who helps you with what

3. Your **Mutual Recognition** with another is the minimum of your recognition of each other.
   - For example: If you are 50% of my _total-recognition_, and I am 10% of your _total-recognition_, our mutual-recognition is 10%, the minimum of the both.
     - _Taking the minimum of both shares ensures reciprocity in proportion._
     - This is _mutual-recognition of contribution towards each other's self-actualization_.
   - **Self-recognition is valid**: You can have mutual recognition with yourself for time-shifting resources

4. **Contribution Trees**: Your recognition emerges from a tree structure tracking contributions
   - Each branch represents a type of contribution (healthcare, food, etc.)
   - Each branch has points distributed among contributors
   - Your global recognition of each person is calculated from their weighted contributions across all branches
   - Example: If 70% of your tree is healthcare (where Dr. Smith has 80 points out of 100), Dr. Smith gets 56% of your total recognition

5. **Two-Tier Allocation System**: When someone has capacity to give:
   - **Tier 1 - Mutual Recognition First**: People with mutual recognition get priority based on their declared needs
   - **Tier 2 - Generous Giving Second**: Remaining capacity goes to others you recognize (even without mutual recognition)
   - This enables both reciprocity and solidarity

6. **Your Allocation** from a provider's capacity is calculated through filtered normalization:
   - **Step 1**: Filter for compatible slots (time, location, type must match)
   - **Step 2**: Calculate your mutual recognition share among filtered recipients
   - **Step 3**: Weight by your active need (declared need × damping factor)
   - **Step 4**: Cap at your actual declared need (no accumulation possible)

7. **Needs Update Dynamically**: 
   - Your remaining need = maximum(0, Your-Declared-Need - Total-You-Received)
   - The system learns to prevent oscillation through adaptive damping factors
   - Each need type (food, healthcare, housing) tracks independently
   - Needs always decrease or stay the same, never increase from allocation

**Mathematically defined:**

```
Your Recognition = your acknowledgment of contributions towards your own self-actualization
Your Total-Recognition = 100%

Mutual-Recognition(You, Them) = MR(You, Them) = minimum(
    Their-share-of-Your-total-recognition, 
    Your-share-of-Their-total-recognition
)

Your-Mutual-Recognition-Share(You, Provider, FilteredSet) = 
    MR(You, Provider) / Σ MR(Provider, Each-Person-In-FilteredSet)

Your-Active-Need = Your-Declared-Need × Damping-Factor
    where Damping-Factor ∈ {0.5, 0.8, 1.0} based on oscillation detection

Your-Raw-Allocation(You, Provider) = 
    Provider's-Available-Capacity 
    × Your-Mutual-Recognition-Share
    × Your-Active-Need 
    / Σ (Each-Filtered-Person's-Active-Need × Their-Mutual-Recognition-Share)

Your-Final-Allocation(You, Provider) = minimum(
    Your-Raw-Allocation, 
    Your-Declared-Need
)

Your-Remaining-Need(tomorrow) = maximum(
    0, 
    Your-Declared-Need - Total-You-Received
)
```

<details>
  <summary><b><i>Being Explored: What if Organizations/States Freely-Associated?</i></b></summary>

Thus far we have principally spoken of free-association between individuals, but what about between organizations, communities, states etc.?

If States/Organizations mutually recognized eachother's contributions towards their own self actualization, and surplus flowed bi-directionally, there would be no more need for imports/exports or international-trade because resources and coordinations flows as surplus from mutual-recognized contributors.

Internally each state/organization would have a mechanism for collective setting of the proportions of the branches of self-actualization of their community.
For example: **Each member has an equal share of proportion-setting-power (at which levels?)**

Surplus would distribute according to mutual-fulfillment exactly the same as occurs between individuals.

We can also imagine a mechanism by which citizens could delegate a portion of their proportion-setting-power to another agent, within a particular category. So for example delegating 10% of your proportion-setting-power in the category of "environmental protection" to an ecologist.

The design space is vast, especially for all those decisions that do not concern proportions, for example:
- how is membership determined
- do all members get equal shares of proportion-setting-power (at which levels?) How is this determined?
- How are contributors added to nodes? How is this determined?
- Can nodes represent groups of contributors, and the tree represent a federation of groups? Could these groups have their own decision making logic for membership within them?
- how do new nodes get created in an organization's recognition-tree? (are there limits to this?) - can one add point to one's own created-node? Or to a node one is a part of?
- can one add points towards a node one is a part of?
- How are capacities collectively declared, and how are their absolute values determined? How are their filters on share-distribution determined

If all people in the world are seperated through at most 6 degrees of seperation, then we can imagine all organization must at most be seperated by 3 or 4. This would be a significant computational gain for calculating transitive surplus shares.

This computational advantage could make organizational-level Free-Association more immediately practical than individual-level implementations for certain types of surplus. Resources that naturally flow at organizational scales (like electricity generation, manufacturing capacity, or agricultural output) might be more efficiently distributed through these shorter organizational networks.

</details>


## Why does any of that matter?

With your **capacity-distribution**, you aren't just giving away surplus randomly—you give _precisely_ to those with whom you have **_mutual recognition_**, prioritizing those who recognize your contributions to their self-actualization just as you recognize theirs.

**The Two-Tier System Creates Natural Reciprocity and Solidarity:**

**Tier 1 - Mutual First**: Imagine you have 4 rooms in your apartment and you are using 1: you have 3 rooms surplus-capacity. **Who would you want to share space with?** Those who **mutually recognize** your contributions—people who help you realize your values, goals, and needs, and who you help in turn. That's **exactly** who gets priority in Tier 1 allocation.

**Tier 2 - Generosity Second**: After meeting mutual needs, remaining capacity flows to others you recognize (even if they don't recognize you back). This enables solidarity with newcomers or those who haven't yet built recognition networks—preventing isolation while maintaining incentives for genuine contribution.

**The system is needs-based**: You declare what you need (food, housing, skills, time), and allocations are calculated automatically based on:
- Your mutual recognition relationships
- Others' available capacity
- Your actual declared needs (never more)
- Compatible slots (time, location, type must match)

**Example**: Imagine a community kitchen with 100 meals. Alice, Bob, and Carol all need food and have mutual recognition with the kitchen based on their contributions (food prep, equipment, etc.). The kitchen's capacity flows to them proportionally based on mutual recognition and their actual needs. No money. No prices. No accumulation beyond need. Just mathematical fairness.

---

To illustrate with another example: Imagine a potluck where pies you bake that you don't need (surplus) automatically flow to neighbors who have mutual recognition with you and who _actually need food_. This helps them spend less time cooking, _freeing their time_ to fix their oven—the same oven that later bakes bread for your community garden party. That's mutual-fulfillment: Your surplus flows to those who contribute to your self-actualization (directly or indirectly), creating ripple effects of mutual-support while strengthening the network. **And it all happens automatically based on needs and recognition—no negotiation required.**

This effectively moves us from _time as money_ to _time as mutual-fulfillment_!

## How Your Contributions Flow Through the Network 🌊

When you contribute surplus to Free Association:

1. 🤝 **Your surplus flows via mutual recognition** - Those with whom you have mutual recognition receive priority based on their declared needs and your available capacity

2. 💝 **Tier 1 ensures reciprocity** - People who recognize your contributions AND whom you recognize get first priority, creating natural incentives for genuine mutual support

3. 🤲 **Tier 2 enables solidarity** - After meeting mutual needs, remaining capacity flows to others you recognize (even without mutual recognition), helping newcomers and those building their networks

4. 🔄 **The system converges rapidly** - Mathematical properties guarantee that if there's sufficient capacity, everyone's needs converge to zero in seconds (typically 5-20 iterations, ~0.5-2 seconds)

5. 🌍 **Your values shape broader society** - Recognition can be given to those working on causes you value (climate action, community spaces, education) regardless of whether you directly benefit, allowing your surplus to flow toward your social values

6. ⚖️ **The network self-corrects toward social-material-truth** - Mathematical properties ensure that false recognition naturally diminishes while true recognition strengthens:

Self-actualization is self-defined (subjective), but its realization depends on objective access to capacities (food, skills, etc.).

Let:
True is not False, False is not True

(Recognition does not need to be true/false in a binary sense but the question is what % of this recognition is true. This % does not take the form of a reified proposition.)

True-Recognition(You): Recognition that, when acted upon, reliably leads to the enhancement of your self-actualization (as defined by you) by connecting you with capacities that genuinely contribute to it. It is validated by positive material and social outcomes.

False-Recognition(You): Recognition that, when acted upon, fails to connect you with the necessary capacities or connects you with harmful ones, thereby undermining your self-actualization. It is invalidated by negative material and social outcomes (like hunger, in the example).

In essence, the truth or falsity is a function of the recognition's practical efficacy in the real world, as experienced by the individual in pursuit of their goals. It is not about correspondence with a statement but about successful navigation of the material-social environment.

```
For any participant:
Total Recognition = 100%
Total Recognition = True-Recognition + False-Recognition
   ∴ ↑False-Recognition = ↓True-Recognition
      ∴ ↓Mutual-Recognition with Actually-Beneficial-Contributors
         ∴ ↓Shares of Actually-Beneficial-Capacities 
         from Actually-Beneficial-Contributors
            ∴ ↓Real-Social-Material-Basis for Self-Actualization
               ∴ Social-Material-Truth is processually realized in Free-Association 
               by processual social-material negation of False-Recognition
```

This mathematical property ensures that inflating recognition or maintaining false-recognition only decreases your connection to actually-beneficial-contributors and their capacities.

Systems built on falsehood eventually collapse, they can't sustain themselves because they starve the very thing that makes them thrive, genuine connection and collaboration.

# Collective-Recognition: Applying Free-Association to Collective Resources
We use free-association to apportion and distribute collective-capacities such as the funds in the playnet open collective!

## How do we prioritize needs?
0\) Players define the collective as a set of members

* Playnet: { Alice, Bob, Charlie }

1\) Players submit their needs (via free-association interface)

* Alice: { needs $1000/month }
* Bob: { needs $500/month }
* Charlie: { needs $200/month }

2\) Players (via free-association interface) recognize the contributions of others towards the realization of their priorities (in the widest sense - as they define them)

* When players mutually recognize each others' contributions, they establish mutual-recognition percentages that determine allocation shares

3\) Derive **total-collective-mutual-recognition** by adding up all the **mutual-recognition** values across **all members of the collective**

4\) Derive **each player's share of the total-collective-mutual-recognition**

This becomes the **collective-priority-distribution** of needs

5\) Allocate the collective's capacity (e.g., funds) according to these shares, capped at each member's declared need

The same mathematical properties apply:
- **Needs-based**: No one receives more than their declared need
- **Recognition-weighted**: Those with stronger mutual recognition receive proportionally more
- **Convergent**: The system finds a stable allocation rapidly
- **Non-accumulative**: Excess recognition doesn't enable accumulation beyond need

## Advantages
* **No centralized definition of what is a meaningful contribution**
* **Distributed determination of value**
* **No centralized registry of a capacity's collective-membership**
* No centralized registry of capacity availability space-time quantity
* **Each player attributes their own membership-list to their view of a collective-capacity and its availability / space-time quantities (potentially subscribing to data sources to feed into their view)**

## Current Needs for Free Association Development 🚀

The free association project currently seeks support in:

- 💻 User-interface development
- 🏠 Housing and food for core contributors
- ⏰ Skills and time for implementation of remaining functionality
- 💰 Financial resources to sustain ongoing development

A basic interface for free association is almost complete but we are seeking support in user-interface-development as well as support in the form of housing, food etc.

## How to Contribute 🤝

<a href="https://opencollective.com/playnet">
    <img width="300" src="https://opencollective.com/playnet/donate/button@2x.png?color=blue" />
</a>

**Contribute Skills/Time:** Contact us to discuss how your skills align with current development needs

**Provide Housing/Space:** If you have surplus housing or workspace that could support Free Association contributors

**Spread Awareness:** Share the mathematical proof and potential of Free Association with others

---

Remember: In contributing to Free Association, you're not donating to charity or investing for return. You're participating in a network where **the free development of each is the condition for the free development of all**. Your contributions help build a system where individuals freely contribute to each other's self-actualization, where capacity flows according to mutual-recognition of real contributions toward each other's self-actualization, automatically allocated based on declared needs and compatible slots.

Your surplus becomes part of a living network of mutual-fulfillment that transcends both charity and investment—it becomes a concrete expression of truly free association.

## Appendix

### This Is Not Traditional Charity 🎯

When you contribute surplus to Free Association, you are not engaging in charity. In charity, resources flow one-way from giver to receiver, creating dependency and hierarchical relations. The giver likely receives nothing material in return, only perhaps some moral satisfaction or tax benefits.

In Free Association, your contributions become integrated into a network of mutual recognition. Your surplus (whether money, housing, skills, or time) helps others who **directly or transitively contribute to your own self-actualization**.

This transcends both traditional charity and market exchange by creating organic flows of surplus based on real contributions to each other's self-actualization. Your surplus becomes part of a living network that strengthens the social fabric while returning to you in unexpected ways.

### This Is Not Traditional Investment 💡

In traditional investment and equity structures, you provide capital expecting financial returns and ownership. You acquire equity — a fixed share that persists regardless of your ongoing contributions. The past (your initial investment) dominates the present.

Consider mutual stakeholding with traditional equity: Organizations swap shares, creating bidirectional dividend (surplus) flows. While this creates non-zero-sum economic relationships where each gains by increasing others' well-being, it has a **crucial** flaw: In equity, you _own_ a share of others.

This ownership model means:

- You can deploy duplicitous means to gain recognition
- Once you secure shares, you _own_ that recognition permanently
- The Past dominates the Present — you cannot remove recognition of past contributions that are no longer true
- The transferability of shares/tokens enables accumulation of false recognition that persists even when new information reveals the truth
- mutual-false-recognition persists indefinitely.

In Free Association:

- You don't "own" a share of anyone else — we each own 100% of ourselves
- Recognition can be re-attributed at any time based on real contributions
- The Present reigns over the Past — recognition adjusts to reflect current social-material reality
- False recognition naturally decays while true recognition strengthens through mathematical necessity:

```
For any participant:
Total Recognition = 100%
Total Recognition = True-Recognition + False-Recognition
   ∴ ↑False-Recognition = ↓True-Recognition
      ∴ ↓Mutual-Recognition with Actually-Beneficial-Contributors
         ∴ ↓Shares of Actually-Beneficial-Capacities 
         from Actually-Beneficial-Contributors
            ∴ ↓Real-Social-Material-Basis for Self-Actualization
               ∴ Social-Material-Truth is processually realized in Free-Association 
               by processual social-material negation of False-Recognition
```

This mathematical property ensures that inflating recognition or maintaining false-recognition only decreases your connection to real-contributors. You cannot accumulate power through ownership.

In Free Association, we are free! You don't own me, and I don't own you! We owe each other nothing!

This is Hegel's resolution of the master-slave dialectic realized in practice: mutual recognition without ownership or domination. Your contributions don't buy you permanent shares — they participate in an evolving network of mutual-recognition that continuously adjusts to reflect real social-material contributions.

## Learn more 📚

- [Playnet.lol](https://playnet.lol)
- [Glossary](GLOSSARY.md)
- [Dialectics of Love (Hegel and Mutual Recognition)](docs/love.md) Free-Association can be seen as the social-material inversion of Hegel's dialectics of Desire, Recognition, and Love
- [Free-Association as the Psychoanalysis of the Economic Unconscious](docs/matheme.md)

- **Inteface Overview (Older Version)**

[![Interface Overview (Old)](https://img.youtube.com/vi/gapy9mBpP5w/hqdefault.jpg)](https://youtu.be/gapy9mBpP5w?si=B2sbZpoaXStg4eKL)

## For Devs 📚

[Install bun](https://fenilsonani.com/articles/installation-step-of-bun)

### For development:

```V
bun install
bun run dev
```

We use [vitest](https://vitest.dev/) and [playwright](https://playwright.dev/) for testing:
```
npm test
```

### For production:

```
bun install
bun run build
```

We have a world to win! Lets play! 🌍✨☺️
