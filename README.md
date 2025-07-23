## Free-Association: A Call for Mutual Self-Actualization 🌱 [![](https://img.shields.io/opencollective/backers/playnet)](https://opencollective.com/playnet#section-contributors) [![Join us on Telegram](https://img.shields.io/badge/Join-Telegram-blue?logo=telegram)](https://t.me/+jS7u3ZcKLoxmYWU5)

_Free-Association_ is literally an actual alternative to capitalism that allows for the _spontaneous self-actualization of all through each other_ without any centralized control, planning, private-property, state intervention!

This requires more than mere abstract agreement or forced cooperation - it demands real contribution to each other's self-actualization (tangible and intangible contributions towards needs, values, goals, social and/or material dependencies etc.).

**_free-association_** essentially unifies the concepts of **_mutual-aid_**, **_mutual-recognition_**, **_mutual-fulfillment_**, and **capacity-distribution\_** in a single elegant and coherent framework that creates the mathematical foundations for a scalable gifting economy of co-creative abundance.

You can interact with an interface implementing this logic at [interplaynetary.github.io/free-association](https://interplaynetary.github.io/free-association/) (in active development) (p2p, distributed, running in the browser - no need for a download - for free)

### Here's how it works:

1. Your **Total-Recognition** is always _100%_.

2. Your **Recognition** is your acknowledgment of _contributions towards your own self-actualization_
   - Always represents a share/portion/percentage of one's total recognition (100%)
   - Non-transferable (unlike shares or equity that can be traded)
   - Dynamically (re)adjustable as relationships and contributions evolve
   - Not limited to direct personal consumption - recognition can be given to those contributing to broader social values and needs you care about.

3. Your **Mutual Recognition** with another is the minimum of your recognition of each other.
   - For example: If you are 50% of my _total-recognition_, and I am 10% of your _total-recognition_, our mutual-recognition is 10%, the minimum of the both.
     - _Taking the minimum of both shares ensures reciprocity in proportion._
     - This is _mutual-recognition of contribution towards each other's self-actualization_.

4. **Shares of Capacities** are distributed across networks of _mutual-recognition of contribution_.
   - Shares are percentages of 100%. They are dynamically _(re)attributed_ and _non-transferable_.
   - Shares of a particular surplus are shares of a _capacity-to-provide_ (they are shares of _verbs_, not shares of _objects_).
   - Determinate quantities (natural numbers 0,1,2,3 ...) can be (re)declared dynamically (today you have the capacity to provide 2 rooms, tomorrow 40, the day after 40 etc.)
   - Optionally declare max-divisibility (either amount or %) to reflect real-world limitations.

5. Your **general-share** in another's **capacities** equals _your mutual recognition with them, divided by their total-mutual-recognition with all others_.
   - For example: If we have 10% of _mutual-recognition_, and you are my only _mutual-relationship_, you would have 100% of my **total-mutual-recognition** (100% of my **general-share**). If I have two _mutual-relationships_ each with 10% mutual-recognition, each would have 50% of my **total-mutual-recognition** (50% of my **general-share**).

6. Your **specific-share** in another's _capacity_ is your **general-share** with _capacity's_ **filters** applied and then the result is normalized to 100%.
   - This is useful for example to provide to mutual-contributors to general-self-actualization, who satisfy a _specific_ criteria.
     For example, a filter could distribute shares only to people who you recognize as contributing in a particular category, or all those except those in a block-list.

**Mathematically defined:**

```
Your Recognition = your acknowledgment of contributions towards your own self-actualization
Your Total-Recognition = 100%

Mutual-Recognition(You, Them) = MR(You, Them) = minimum(Their-share-of-Your-total-recognition, Your-share-of-Their-total-recognition)

General-Share(You, Provider) = MR(You, Provider) / Σ MR(Provider, Each-of-Those-Provider-Recognizes)
```

<details>
  <summary><b><i>Capacity Composition</i></b></summary>

This allows us to compose our capacities into gift-economy based supply chains / higher-order collective capacities **while retaining mutual-recognition proportions**.

So we can organize complex and increasingly powerful cooperative processes, that operate within the realm of mutual-fulfillment and mutual-desire

The network effects of this system are particularly fascinating. As more participants engage in mutual composition, the complexity and richness of possible supply chains grows exponentially. A cooking capacity might compose ingredient supplies, which compose farming capacities, which compose transportation networks, which compose community gathering spaces—all operating on principles of mutual desire and shared benefit. Each link in these chains strengthens the whole network, creating resilient webs of interdependence that are fundamentally different from the fragile, hierarchical supply chains of traditional capitalism. When disruption occurs, the network can adapt and flow around obstacles because relationships are based on genuine mutual benefit rather than contractual obligation.

We have a world to win! Lets play! 🌍✨☺️

### How Composition Works:

1. **Compose-From**: You enhance your capacity using shares you have in others' capacities
   - Example: You enhance your "cooking-skill" using your share in someone's "ingredients-supply"
   - Constrained by your share percentage in their capacity

2. **Compose-Into**: You enhance others' capacities using shares they have in your capacities
   - Example: You enhance someone's "meal-service" using their share in your "cooking-skill"
   - Constrained by their share percentage in your capacity

3. **Resource Allocation**: When total desired composition exceeds available capacity
   - Applies proportional scaling across all participants
   - Maintains mathematical integrity

4. **Mutual Composition**: Most fulfilling when desires align bidirectionally
   - Both parties desire this composition

**Mathematically defined:**

```
Compose-From(You, Your-Capacity, Provider, Their-Capacity) = Your-Desired-Units-From-Their-Capacity-Into-Your-Capacity
Compose-Into(You, Your-Capacity, Recipient, Their-Capacity) = Your-Desired-Units-From-Your-Capacity-Into-Their-Capacity

Share-Constrained-From(You, Your-Capacity, Provider, Their-Capacity) =
    minimum(Compose-From(You, Your-Capacity, Provider, Their-Capacity),
            Your-Share-of-Their-Capacity × Their-Capacity-Quantity)

Share-Constrained-Into(You, Your-Capacity, Recipient, Their-Capacity) =
    minimum(Compose-Into(You, Your-Capacity, Recipient, Their-Capacity),
            Their-Share-of-Your-Capacity × Your-Capacity-Quantity)

**Compose-From Scaling (Provider's Perspective):**
For Provider's capacity j:
Total-Demand-For-Provider-Capacity(j) = Σ Share-Constrained-From(All-Participants, All-Their-Capacities, Provider, j)

Provider-Allocation-Factor(j) = minimum(1, Provider-Available-Units(j) / Total-Demand-For-Provider-Capacity(j))

Feasible-Compose-From(You, Your-Capacity, Provider, Their-Capacity) =
    Share-Constrained-From(You, Your-Capacity, Provider, Their-Capacity) ×
    Provider-Allocation-Factor(Their-Capacity)

**Compose-Into Scaling (Your Perspective):**
For Your capacity i:
Total-Desired-Distribution-From-Your-Capacity(i) = Σ Share-Constrained-Into(You, i, All-Recipients, All-Their-Capacities)

Your-Allocation-Factor(i) = minimum(1, Your-Available-Units(i) / Total-Desired-Distribution-From-Your-Capacity(i))

Feasible-Compose-Into(You, Your-Capacity, Recipient, Their-Capacity) =
    Share-Constrained-Into(You, Your-Capacity, Recipient, Their-Capacity) ×
    Your-Allocation-Factor(Your-Capacity)

**Proportional Scaling Properties:**
- **Compose-From**: When Provider-Allocation-Factor < 1, all participants wanting that provider's capacity get scaled by the same factor
- **Compose-Into**: When Your-Allocation-Factor < 1, all your desired distributions from your capacity get scaled by the same factor

Desire-Alignment(Your-Desire, Their-Desire) = minimum(Your-Desire, Their-Desire) / maximum(Your-Desire, Their-Desire)

Mutual-Compose-From(You, Your-Capacity, Provider, Their-Capacity) =
    minimum(Feasible-Compose-From(You, Your-Capacity, Provider, Their-Capacity),
            Compose-Into(Provider, Their-Capacity, You, Your-Capacity))

Mutual-Compose-Into(You, Your-Capacity, Recipient, Their-Capacity) =
    minimum(Feasible-Compose-Into(You, Your-Capacity, Recipient, Their-Capacity),
            Compose-From(Recipient, Their-Capacity, You, Your-Capacity))
```

</details>

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

With your **capacity-distribution**, you aren't just giving away surplus randomly, you give _precisely_ to your own direct/transitive **_mutual_** fulfillment.

Imagine you have 4 rooms in your apartment and you are using 1: you have 3 rooms surplus-capacity.

**_Now who would you want to share space with?_**

Well ideally, \*those that **satisfy your needs** (who contribution to the realization of your values, goals, etc.) in the **_widest_** sense of the term.\*

And even more than that, it is even _more satisfying_ **_when the feeling is mutual!_**

And that is **_exactly_** who is attributed _shares_ of your surplus-capacity!

---

To illustrate with another example: Imagine a potluck where pies you bake that you don't need (surplus) helps a neighbor spend less time cooking, _freeing their time_ to fix their oven — the same oven that later bakes bread for your community garden party. That's mutual-fulfillment: Your surplus doesn't vanish — is shared with those who directly/indirectly align with your needs/desires/values/goals, creating a ripple effect of mutual-support that comes back to you in unexpected ways while strengthening the network.

This effectively moves us from _time as money_ to _time as mutual-fulfillment_!

## How Your Contributions Flow Through the Network 🌊

When you contribute surplus to Free Association:

1. 🤝 **Your surplus flows to direct contributors** - Those you recognize as contributing to your self-actualization receive proportional shares of your surplus

2. 🌐 **Your surplus flows to transitive contributors** - Those who support your direct contributors also benefit, creating ripple effects through the network

3. 🔄 **Effects return to you in unexpected ways** - As others you've supported gain capacity, they can contribute to your self-actualization in ways you couldn't predict.

4. 🌍 **Your values shape broader society** - Recognition can be given to those working on causes you value (climate action, community spaces, education) regardless of whether you directly benefit, allowing your influence to flow toward your social values.

5. ⚖️ **The network self-corrects toward social-material-truth** - Mathematical properties ensure that false recognition naturally diminishes while true recognition strengthens:

```
For any participant:
Total Recognition = 100%
Total Recognition = True-Recognition + False-Recognition
   ∴ ↑False-Recognition = ↓True-Recognition
      ∴ ↓Mutual-Recognition with Actually-Beneficial-Contributors
         ∴ ↓Shares of Actually-Beneficial-Capacities from Actually-Beneficial-Contributors
            ∴ ↓Real-Social-Material-Basis for Self-Actualization
               ∴ Social-Material-Truth is processually realized in Free-Association by processual social-material negation of False-Recognition
```

This mathematical property ensures that inflating recognition or maintaining false-recognition only decreases your connection to actually-beneficial-contributors and their surplus-capacities.

Systems built on falsehood eventually collapse, they can't sustain themselves because they starve the very thing that makes them thrive, genuine connection and collaboration.

## The Power of Six Degrees: Sharing Humanity's Cooperative Wealth 🌍

In free association, surplus doesn't just flow to direct connections — it flows through networks of mutual recognition that ultimately connect all of humanity.

Consider this mathematical reality:

```
For any participants A, B, C:
- Direct Relation: A ↔ B (A and B directly connected)
- Transitive Relation: A ↔ B ↔ C (A and C transitively connected via B)
- Your share of anyone's surplus = MR(You, Them) / Σ MR(Them, Each-of-Those-They-Recognize)
```

Now combine this with a profound sociological discovery: everyone on Earth is connected to everyone else through at most six degrees of separation. In free association, this means:

1. **First Degree** 👥: Your direct contributors (friends, colleagues, local community)
   - Their surplus capacity is shared based on your mutual recognition
   - You gain access to their skills, knowledge, resources

2. **Second Degree** 🤝: Contributors to your contributors
   - A neighbor's friend who grows food
   - A colleague's network of skilled programmers
   - Each connection multiplies available surplus

3. **Third Degree** 🏘️: Entire communities and networks
   - Housing cooperatives
   - Knowledge networks
   - Production collectives
   - Each node represents hundreds of potential access relationships

4. **Fourth through Sixth Degrees** 🌏: Global reach
   - Access to surplus from every field of human endeavor
   - Connection to cultural production worldwide
   - Integration with global networks of mutual support
   - Each step exponentially increases available surplus

This means that in free association, you're never more than six relationship connections away from accessing any surplus capacity in the entire human network!

Unlike markets which restrict access through price mechanisms, or states which centrally plan distribution, free association creates organic access pathways based on real contributions to each other's self-actualization.

This exponential growth means that as free association spreads:

- Your potential access to surplus capacity grows exponentially
- The diversity of accessible contributions increases dramatically
- The network becomes increasingly resilient and antifragile
- False recognition becomes increasingly disadvantageous as real alternatives multiply

Imagine:

- 🎨 A designer in Brazil whose surplus creativity becomes available to enhance your local project
- 👩‍💻 An engineer in Kenya whose surplus technical knowledge helps solve challenges in your community
- 🏠 A housing cooperative in Denmark whose surplus space becomes available through chains of mutual recognition
- 🌳 A food forest in Thailand whose surplus production strengthens your local food security

All of these connections become possible through no more than six degrees of mutual recognition relationships. This isn't just an abstract possibility — it's mathematical certainty based on network-mathematics (contribution-graphs and contribution-distance) combined with free association's relationship mechanics.

The result? A world where:

- Every individual has potential access to humanity's full cooperative wealth
- Surplus capacities become accessible precisely where they contributes to real self-actualization
- False recognition naturally decays as real alternatives become visible
- The free development of each truly becomes the condition for the free development of all

This is why participating in free association isn't just about direct relationships — it's about establishing and strengthening a network that can ultimately connect all of humanity through mutual recognition and surplus distribution. Your contributions don't just provide for immediate recipients; they help build and strengthen relational pathways through which humanity's cooperative wealth flows more freely.

Note: this level of access to the wealth of social-abundance can be overwhelming for inviduals! Various organizational elements, games, organizations, structures, algorithms, interface extensions should be explored to help individuals navigate and interact with this abundance of access while minimizing overwhelm. When indivudals form communities that freely associate with eachother, communal organization of surplus can help minimize the burden on individuals to need to process the whole of this abundance. Free association between individuals, between organizations, between individuals and organizations, between organizations and individuals.

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

Remember: In contributing to Free Association, you're not donating to charity or investing for return. You're participating in a network where **the free development of each is the condition for the free development of all**. Your contributions help build a system where "individuals freely contribute to each other's self-actualization, where surplus flows according to mutual-recognition of real contributions towards each other's self-actualization, where relations activate only through mutual desire and participation."

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
         ∴ ↓Shares of Actually-Beneficial-Surplus from Actually-Beneficial-Contributors
            ∴ ↓Real-Social-Material-Basis for Self-Actualization
               ∴ Social-Material-Truth is processually realized in Free-Association by processual social-material negation of False-Recognition
```

This mathematical property ensures that inflating recognition or maintaining false-recognition only decreases your connection to real-contributors. You cannot accumulate power through ownership.

In Free Association, we are free! You don't own me, and I don't own you! We owe each other nothing!

This is Hegel's resolution of the master-slave dialectic realized in practice: mutual recognition without ownership or domination. Your contributions don't buy you permanent shares — they participate in an evolving network of mutual-recognition that continuously adjusts to reflect real social-material contributions.

## Learn more 📚

- [Playnet.lol](https://playnet.lol)
- [Glossary](GLOSSARY.md)
- [Dialectics of Love (Hegel and Mutual Recognition)](resources/love.md) Free-Association can be seen as the social-material inversion of Hegel's dialectics of Desire, Recognition, and Love
- [Free-Association as the Psychoanalysis of the Economic Unconscious](resources/matheme.md)

- **Inteface Overview (Older Version)**

[![Interface Overview (Old)](https://img.youtube.com/vi/ro-y4MnOgNo/hqdefault.jpg)](https://youtu.be/ro-y4MnOgNo?si=ae1nC7-uIwQ0t-YN)

## For Devs 📚

[Install bun](https://fenilsonani.com/articles/installation-step-of-bun)

### For development:

In one console:

```
bun install
bun run dev
```

In another console start the gun relay:

```
bun start
```

### For production:

```
bun install
bun run build
```
