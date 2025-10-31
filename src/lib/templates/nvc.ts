import { addChild, findNodeById } from '$lib/commons/v5/tree';
import type { RootNode } from '$lib/commons/v5/schemas';

/**
 * Populate tree with Universal Basic Needs (NVC-style) categories
 * Structure mirrors the SDG template approach: top-level categories at root,
 * with nested children for sub-items where specified by the user.
 */
export function populateNVCTree(rootNode: RootNode): RootNode {
  // Top-level categories
  addChild(rootNode, 'nvc-physical', '🩺 Physical Well-Being', 6);
  addChild(rootNode, 'nvc-connection', '💞 Connection', 6);
  addChild(rootNode, 'nvc-meaning', '🌱 Meaning', 6);
  addChild(rootNode, 'nvc-freedom', '🕊️ Freedom', 5);
  addChild(rootNode, 'nvc-honesty', '🤝 Honesty', 5);
  addChild(rootNode, 'nvc-play', '🎨 Play', 5);
  addChild(rootNode, 'nvc-harmony', '☯️ Harmony', 6);

  // Physical Well-Being
  const physical = findNodeById(rootNode, 'nvc-physical');
  if (physical) {
    addChild(physical, 'nvc-physical-air', 'Air', 10);
    addChild(physical, 'nvc-physical-nourishment', 'Nourishment (food, water)', 12);
    addChild(physical, 'nvc-physical-light', 'Light', 8);
    addChild(physical, 'nvc-physical-warmth', 'Warmth', 8);
    addChild(physical, 'nvc-physical-rest', 'Rest / Sleep', 14);
    addChild(physical, 'nvc-physical-movement', 'Movement / Physical exercise', 10);
    addChild(physical, 'nvc-physical-health', 'Health', 12);
    addChild(physical, 'nvc-physical-touch', 'Touch', 8);
    addChild(physical, 'nvc-physical-sexual', 'Sexual expression', 8);
    addChild(physical, 'nvc-physical-shelter', 'Shelter / Security / Safety', 14);

    // Nested under Shelter / Security / Safety
    const shelter = findNodeById(rootNode, 'nvc-physical-shelter');
    if (shelter) {
      addChild(shelter, 'nvc-physical-shelter-emotional-safety', 'Emotional safety', 50);
      addChild(
        shelter,
        'nvc-physical-shelter-protection',
        'Protection from pain / protection / preservation',
        50
      );
    }

    addChild(physical, 'nvc-physical-comfort', 'Comfort', 10);
  }

  // Connection
  const connection = findNodeById(rootNode, 'nvc-connection');
  if (connection) {
    addChild(connection, 'nvc-connection-love', 'Love / Self-love', 12);
    addChild(connection, 'nvc-connection-care', 'Care / Self-care', 12);
    addChild(connection, 'nvc-connection-belonging', 'Belonging', 10);
    addChild(connection, 'nvc-connection-closeness', 'Closeness / Intimacy', 10);
    addChild(connection, 'nvc-connection-empathy', 'Empathy / Compassion', 12);
    addChild(connection, 'nvc-connection-appreciation', 'Appreciation / Gratitude', 10);
    addChild(connection, 'nvc-connection-acceptance', 'Acceptance', 10);
    addChild(connection, 'nvc-connection-recognition', 'Recognition', 10);
    addChild(connection, 'nvc-connection-reassurance', 'Reassurance', 8);
    addChild(connection, 'nvc-connection-affection', 'Affection', 10);
    addChild(connection, 'nvc-connection-attention', 'Attention', 10);
    addChild(connection, 'nvc-connection-openness', 'Openness', 10);
    addChild(connection, 'nvc-connection-communication', 'Communication', 12);

    // Nested under Communication
    const communication = findNodeById(rootNode, 'nvc-connection-communication');
    if (communication) {
      addChild(communication, 'nvc-connection-communication-sharing', 'Sharing / Exchange', 25);
      addChild(communication, 'nvc-connection-communication-giving', 'Giving / Receiving', 25);
      addChild(communication, 'nvc-connection-communication-tenderness', 'Tenderness / Softness', 20);
      addChild(communication, 'nvc-connection-communication-sensitivity', 'Sensitivity / Kindness', 20);
    }

    addChild(connection, 'nvc-connection-seeing', 'Seeing (see & be seen)', 10);

    // Nested: Seeing/Hearing/Understanding/Inclusion are grouped as peers per image text
    addChild(connection, 'nvc-connection-hearing', 'Hearing (hear & be heard)', 10);
    addChild(
      connection,
      'nvc-connection-understanding',
      'Understanding (understand & be understood)',
      12
    );
    addChild(
      connection,
      'nvc-connection-inclusion',
      'Inclusion / Participation / Belonging',
      12
    );

    addChild(connection, 'nvc-connection-support', 'Support / Help / Nurturance', 12);
    addChild(connection, 'nvc-connection-cooperation', 'Cooperation / Collaboration', 12);
    addChild(
      connection,
      'nvc-connection-companionship',
      'Companionship / Fellowship / Community / Partnership',
      12
    );
    addChild(connection, 'nvc-connection-consistency', 'Consistency / Continuity', 10);
  }

  // Meaning
  const meaning = findNodeById(rootNode, 'nvc-meaning');
  if (meaning) {
    addChild(meaning, 'nvc-meaning-contribution', 'Contribution / Enrich life', 12);
    addChild(meaning, 'nvc-meaning-presence', 'Presence / Centeredness', 10);
    addChild(meaning, 'nvc-meaning-self-connection', 'Self-connection', 10);
    addChild(meaning, 'nvc-meaning-hope', 'Hope / Vision / Dream / Faith', 10);
    addChild(meaning, 'nvc-meaning-clarity', 'Clarity / Focus / Concentration', 10);

    // Nested under Clarity: To know (be in reality)
    const clarity = findNodeById(rootNode, 'nvc-meaning-clarity');
    if (clarity) {
      addChild(clarity, 'nvc-meaning-clarity-to-know', 'To know (be in reality)', 100);
    }

    addChild(meaning, 'nvc-meaning-learning', 'Learning', 10);
    addChild(meaning, 'nvc-meaning-awareness', 'Awareness / Consciousness', 10);
    addChild(meaning, 'nvc-meaning-inspiration', 'Inspiration / Creativity', 10);
    addChild(meaning, 'nvc-meaning-challenge', 'Challenge / Stimulation', 10);
    addChild(meaning, 'nvc-meaning-growth', 'Growth / Evolution / Progress / Expansion', 12);
    addChild(meaning, 'nvc-meaning-exploration', 'Exploration / Development', 10);
    addChild(meaning, 'nvc-meaning-purpose', 'Purpose / Strength / Empowerment', 12);
    addChild(meaning, 'nvc-meaning-self-esteem', 'Self-esteem / Capacity', 10);
    addChild(
      meaning,
      'nvc-meaning-self-value',
      'Self-value / Self-confidence / Self-efficacy',
      10
    );
    addChild(
      meaning,
      'nvc-meaning-self-acceptance',
      'Self-acceptance / Self-love / Self-trust / Self-worth',
      12
    );
    addChild(meaning, 'nvc-meaning-meaning', 'Meaning / Make meaning in life', 10);
    addChild(
      meaning,
      'nvc-meaning-spirituality',
      'Spirituality / Purpose / Transformation',
      12
    );
    addChild(meaning, 'nvc-meaning-interdependence', 'Interdependence', 10);
    addChild(meaning, 'nvc-meaning-simplicity', 'Simplicity', 8);
    addChild(meaning, 'nvc-meaning-celebration', 'Celebration / Mourning', 8);
  }

  // Freedom
  const freedom = findNodeById(rootNode, 'nvc-freedom');
  if (freedom) {
    addChild(
      freedom,
      'nvc-freedom-choice',
      'Choice / Acting out of my own spirituality',
      12
    );
    addChild(freedom, 'nvc-freedom-autonomy', 'Autonomy', 12);
    addChild(freedom, 'nvc-freedom-independence', 'Independence', 10);
    addChild(freedom, 'nvc-freedom-space-time', 'Space / Time', 10);
  }

  // Honesty
  const honesty = findNodeById(rootNode, 'nvc-honesty');
  if (honesty) {
    addChild(honesty, 'nvc-honesty-authenticity', 'Authenticity', 12);
    addChild(honesty, 'nvc-honesty-expression', '(Self) expression', 12);
    addChild(honesty, 'nvc-honesty-integrity', 'Integrity', 12);
    addChild(honesty, 'nvc-honesty-transparency', 'Transparency', 10);
    addChild(honesty, 'nvc-honesty-realness', 'Realness / Truth', 12);
  }

  // Play
  const play = findNodeById(rootNode, 'nvc-play');
  if (play) {
    addChild(play, 'nvc-play-liveliness', 'Liveliness / Alive / Vitality', 10);
    addChild(play, 'nvc-play-passion', 'Passion', 10);
    addChild(play, 'nvc-play-spontaneity', 'Spontaneity', 10);
    addChild(play, 'nvc-play-fun', 'Fun / Laugh / Lightness', 10);
    addChild(play, 'nvc-play-discovery', 'Discovery / Adventure', 10);
    addChild(play, 'nvc-play-variety', 'Variety / Diversity', 10);
    addChild(play, 'nvc-play-renewal', 'Renewal / Refreshment', 10);
  }

  // Harmony
  const harmony = findNodeById(rootNode, 'nvc-harmony');
  if (harmony) {
    addChild(harmony, 'nvc-harmony-peace', 'Peace', 12);
    addChild(harmony, 'nvc-harmony-beauty', 'Beauty', 10);
    addChild(harmony, 'nvc-harmony-calm', 'Calm / Equanimity', 10);
    addChild(harmony, 'nvc-harmony-relaxation', 'Relaxation / Tranquility', 10);
    addChild(harmony, 'nvc-harmony-ease', 'Ease', 10);
    addChild(harmony, 'nvc-harmony-order', 'Order', 10);
    addChild(harmony, 'nvc-harmony-coherence', 'Coherence / Congruence', 10);
    addChild(harmony, 'nvc-harmony-sustainability', 'Sustainability', 10);
    addChild(harmony, 'nvc-harmony-stability', 'Stability / Balance', 10);
    addChild(harmony, 'nvc-harmony-community', 'Community / Wholeness', 10);
    addChild(harmony, 'nvc-harmony-completion', 'Completion / Digestion / Integration', 10);
    addChild(harmony, 'nvc-harmony-predictability', 'Predictability / Familiarity', 10);
    addChild(harmony, 'nvc-harmony-equality', 'Equality / Justice / Fairness', 12);
  }

  return rootNode;
}


