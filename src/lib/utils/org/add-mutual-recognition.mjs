/**
 * Add Mutual Recognition Relationships
 * If org A recognizes org B, then org B should recognize org A back (where appropriate)
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/demo/orgs.json', 'utf8'));

// Build recognition graph
const recognizes = {}; // who recognizes whom
const recognizedBy = {}; // who is recognized by whom

Object.entries(trees).forEach(([slug, tree]) => {
  recognizes[slug] = new Map(); // target -> points
  if (!recognizedBy[slug]) recognizedBy[slug] = new Map();

  (tree.recognizes || []).forEach(r => {
    const targetSlug = r.id.replace('org_demo_', '');
    recognizes[slug].set(targetSlug, r.points);

    if (!recognizedBy[targetSlug]) recognizedBy[targetSlug] = new Map();
    recognizedBy[targetSlug].set(slug, r.points);
  });
});

// Function to add recognition
function addRecognition(fromSlug, toSlug, points) {
  if (!trees[fromSlug] || !trees[toSlug]) return false;

  // Check if already recognizes
  const existing = trees[fromSlug].recognizes.find(r => r.id === `org_demo_${toSlug}`);
  if (existing) return false;

  // Add recognition
  trees[fromSlug].recognizes.push({
    id: `org_demo_${toSlug}`,
    points
  });

  return true;
}

// Calculate appropriate points for reciprocal recognition
function calculateReciprocalPoints(originalPoints) {
  // Match the original points but vary slightly for realism
  const variation = Math.floor(Math.random() * 11) - 5; // -5 to +5
  const reciprocal = originalPoints + variation;
  return Math.max(15, Math.min(50, reciprocal)); // Keep between 15-50
}

console.log('🔄 Adding Mutual Recognition Relationships\n');

let added = 0;
let skipped = 0;

// Priority order: most important orgs should reciprocate first
const priorityOrgs = [
  'undp', 'unep', 'fao', 'irena', 'greenclimatefund', 'unfccc',
  'worldbank', 'ipcc', 'africandevbank', 'asiandevbank', 'iadb',
  'europeanunion', 'conservationinternational', 'wwf', 'oxfam'
];

priorityOrgs.forEach(orgSlug => {
  if (!recognizedBy[orgSlug]) return;

  const needsToRecognize = Array.from(recognizedBy[orgSlug].entries());

  // Sort by points (higher points = more important relationship)
  needsToRecognize.sort((a, b) => b[1] - a[1]);

  // Current recognition count
  const currentCount = trees[orgSlug]?.recognizes?.length || 0;

  // Add reciprocal recognitions (limit to avoid too many)
  const maxToAdd = Math.min(needsToRecognize.length, Math.max(0, 12 - currentCount));

  console.log(`${orgSlug} (${trees[orgSlug]?.name}): adding ${maxToAdd} reciprocal recognitions`);

  needsToRecognize.slice(0, maxToAdd).forEach(([fromSlug, originalPoints]) => {
    const points = calculateReciprocalPoints(originalPoints);
    if (addRecognition(orgSlug, fromSlug, points)) {
      added++;
      console.log(`  ✓ ${orgSlug} → ${fromSlug} (${points} points)`);
    } else {
      skipped++;
    }
  });

  console.log('');
});

// Handle remaining organizations with fewer reciprocations needed
console.log('═══ Other Organizations ═══\n');

const remainingOrgs = Object.keys(recognizedBy)
  .filter(org => !priorityOrgs.includes(org))
  .filter(org => recognizedBy[org] && recognizedBy[org].size > 0);

remainingOrgs.forEach(orgSlug => {
  const needsToRecognize = Array.from(recognizedBy[orgSlug].entries());
  const currentCount = trees[orgSlug]?.recognizes?.length || 0;

  // For smaller orgs, add fewer reciprocal recognitions
  const maxToAdd = Math.min(needsToRecognize.length, Math.max(0, 8 - currentCount));

  if (maxToAdd > 0) {
    console.log(`${orgSlug}: adding ${maxToAdd} reciprocal recognitions`);

    // Sort by points
    needsToRecognize.sort((a, b) => b[1] - a[1]);

    needsToRecognize.slice(0, maxToAdd).forEach(([fromSlug, originalPoints]) => {
      const points = calculateReciprocalPoints(originalPoints);
      if (addRecognition(orgSlug, fromSlug, points)) {
        added++;
      } else {
        skipped++;
      }
    });
  }
});

// Save
fs.writeFileSync('./src/lib/demo/orgs.json', JSON.stringify(trees, null, 2), 'utf8');

// Recalculate statistics
const oneWayAfter = [];
const recognizesAfter = {};

Object.entries(trees).forEach(([slug, tree]) => {
  recognizesAfter[slug] = new Set();

  (tree.recognizes || []).forEach(r => {
    const targetSlug = r.id.replace('org_demo_', '');
    recognizesAfter[slug].add(targetSlug);
  });
});

Object.entries(recognizesAfter).forEach(([slug, targets]) => {
  targets.forEach(target => {
    if (!recognizesAfter[target]?.has(slug)) {
      oneWayAfter.push({ from: slug, to: target });
    }
  });
});

console.log('\n═══════════════════════════════════════');
console.log('✅ MUTUAL RECOGNITION COMPLETE');
console.log('═══════════════════════════════════════');
console.log(`Added: ${added} new reciprocal recognitions`);
console.log(`Skipped: ${skipped} (already existed)`);
console.log(`One-way relationships: 442 → ${oneWayAfter.length}`);
console.log('═══════════════════════════════════════\n');

// Show some examples
console.log('📊 Examples of new mutual recognition:\n');

const examples = ['undp', 'fao', 'irena', 'greenclimatefund'];
examples.forEach(slug => {
  if (trees[slug]) {
    console.log(`${trees[slug].name}:`);
    console.log(`  Total recognitions: ${trees[slug].recognizes.length}`);
    console.log(`  Sample: ${trees[slug].recognizes.slice(0, 5).map(r => r.id.replace('org_demo_', '')).join(', ')}`);
    console.log('');
  }
});

