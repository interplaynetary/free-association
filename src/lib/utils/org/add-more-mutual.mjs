/**
 * Second Pass: Add More Mutual Recognition
 * More aggressive approach to ensure better reciprocity
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/demo/orgs.json', 'utf8'));

// Build recognition graph
const recognizedBy = {}; // who is recognized by whom

Object.entries(trees).forEach(([slug, tree]) => {
  if (!recognizedBy[slug]) recognizedBy[slug] = new Map();

  (tree.recognizes || []).forEach(r => {
    const targetSlug = r.id.replace('org_demo_', '');

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
  const variation = Math.floor(Math.random() * 11) - 5; // -5 to +5
  const reciprocal = originalPoints + variation;
  return Math.max(15, Math.min(50, reciprocal));
}

console.log('🔄 Second Pass: Adding More Mutual Recognition\n');

let added = 0;

// Process all organizations that still need to reciprocate
const allOrgs = Object.keys(recognizedBy)
  .filter(org => recognizedBy[org] && recognizedBy[org].size > 0)
  .sort((a, b) => recognizedBy[b].size - recognizedBy[a].size); // Sort by most needed

allOrgs.forEach(orgSlug => {
  const needsToRecognize = Array.from(recognizedBy[orgSlug].entries());
  const currentCount = trees[orgSlug]?.recognizes?.length || 0;

  // More generous limits - allow up to 15 recognitions
  const maxToAdd = Math.min(needsToRecognize.length, Math.max(0, 15 - currentCount));

  if (maxToAdd > 0) {
    console.log(`${orgSlug}: adding ${maxToAdd} more recognitions (current: ${currentCount})`);

    // Sort by points (prioritize important relationships)
    needsToRecognize.sort((a, b) => b[1] - a[1]);

    // Filter out ones we already recognize
    const toAdd = needsToRecognize
      .filter(([fromSlug]) => {
        return !trees[orgSlug].recognizes.find(r => r.id === `org_demo_${fromSlug}`);
      })
      .slice(0, maxToAdd);

    toAdd.forEach(([fromSlug, originalPoints]) => {
      const points = calculateReciprocalPoints(originalPoints);
      if (addRecognition(orgSlug, fromSlug, points)) {
        added++;
      }
    });
  }
});

// Save
fs.writeFileSync('./src/lib/demo/orgs.json', JSON.stringify(trees, null, 2), 'utf8');

// Recalculate statistics
const recognizesAfter = {};

Object.entries(trees).forEach(([slug, tree]) => {
  recognizesAfter[slug] = new Set();

  (tree.recognizes || []).forEach(r => {
    const targetSlug = r.id.replace('org_demo_', '');
    recognizesAfter[slug].add(targetSlug);
  });
});

const oneWayAfter = [];
Object.entries(recognizesAfter).forEach(([slug, targets]) => {
  targets.forEach(target => {
    if (!recognizesAfter[target]?.has(slug)) {
      oneWayAfter.push({ from: slug, to: target });
    }
  });
});

// Count mutual relationships
let mutualCount = 0;
Object.entries(recognizesAfter).forEach(([slug, targets]) => {
  targets.forEach(target => {
    if (recognizesAfter[target]?.has(slug)) {
      mutualCount++; // Will count each pair twice, divide by 2 later
    }
  });
});
mutualCount = Math.floor(mutualCount / 2);

// Total recognition relationships
let totalRecognitions = 0;
Object.values(trees).forEach(tree => {
  totalRecognitions += tree.recognizes.length;
});

console.log('\n═══════════════════════════════════════');
console.log('✅ SECOND PASS COMPLETE');
console.log('═══════════════════════════════════════');
console.log(`Added: ${added} more reciprocal recognitions`);
console.log(`Total recognition relationships: ${totalRecognitions}`);
console.log(`Mutual relationships: ${mutualCount}`);
console.log(`One-way relationships: 311 → ${oneWayAfter.length}`);
console.log(`Mutuality rate: ${Math.round((mutualCount / (mutualCount + oneWayAfter.length)) * 100)}%`);
console.log('═══════════════════════════════════════\n');

// Show distribution of recognition counts
const distribution = {};
Object.values(trees).forEach(tree => {
  const count = tree.recognizes.length;
  distribution[count] = (distribution[count] || 0) + 1;
});

console.log('📊 Recognition Count Distribution:');
Object.keys(distribution).sort((a, b) => Number(a) - Number(b)).forEach(count => {
  console.log(`  ${count} recognitions: ${distribution[count]} orgs`);
});

