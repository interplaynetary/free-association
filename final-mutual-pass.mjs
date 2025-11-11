/**
 * Final Pass: Maximize Mutual Recognition
 * Most aggressive approach to maximize bidirectional relationships
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/config/org-trees.json', 'utf8'));

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
  const variation = Math.floor(Math.random() * 11) - 5;
  const reciprocal = originalPoints + variation;
  return Math.max(15, Math.min(50, reciprocal));
}

console.log('🚀 Final Pass: Maximizing Mutual Recognition\n');

let added = 0;

// Process ALL organizations, allowing up to 20 recognitions each
const allOrgs = Object.keys(recognizedBy)
  .filter(org => recognizedBy[org] && recognizedBy[org].size > 0)
  .sort((a, b) => recognizedBy[b].size - recognizedBy[a].size);

allOrgs.forEach(orgSlug => {
  const needsToRecognize = Array.from(recognizedBy[orgSlug].entries());
  const currentCount = trees[orgSlug]?.recognizes?.length || 0;
  
  // Very generous limit - up to 20 recognitions
  const maxToAdd = Math.min(needsToRecognize.length, Math.max(0, 20 - currentCount));
  
  if (maxToAdd > 0) {
    // Sort by points
    needsToRecognize.sort((a, b) => b[1] - a[1]);
    
    // Filter out ones we already recognize
    const toAdd = needsToRecognize
      .filter(([fromSlug]) => {
        return !trees[orgSlug].recognizes.find(r => r.id === `org_demo_${fromSlug}`);
      })
      .slice(0, maxToAdd);
    
    if (toAdd.length > 0) {
      console.log(`${orgSlug}: adding ${toAdd.length} more (current: ${currentCount})`);
      
      toAdd.forEach(([fromSlug, originalPoints]) => {
        const points = calculateReciprocalPoints(originalPoints);
        if (addRecognition(orgSlug, fromSlug, points)) {
          added++;
        }
      });
    }
  }
});

// Save
fs.writeFileSync('./src/lib/config/org-trees.json', JSON.stringify(trees, null, 2), 'utf8');

// Calculate final statistics
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
const mutualPairs = new Set();

Object.entries(recognizesAfter).forEach(([slug, targets]) => {
  targets.forEach(target => {
    if (recognizesAfter[target]?.has(slug)) {
      const pairKey = [slug, target].sort().join('-');
      if (!mutualPairs.has(pairKey)) {
        mutualPairs.add(pairKey);
        mutualCount++;
      }
    }
  });
});

// Total recognition relationships
let totalRecognitions = 0;
Object.values(trees).forEach(tree => {
  totalRecognitions += tree.recognizes.length;
});

// Average recognitions per org
const avgRecognitions = (totalRecognitions / 143).toFixed(1);

console.log('\n═══════════════════════════════════════════════════');
console.log('🎉 FINAL MUTUAL RECOGNITION COMPLETE!');
console.log('═══════════════════════════════════════════════════');
console.log(`Added in final pass: ${added} reciprocal recognitions`);
console.log(`Total recognition relationships: ${totalRecognitions} (avg ${avgRecognitions} per org)`);
console.log(`Mutual relationships: ${mutualCount}`);
console.log(`One-way relationships: ${oneWayAfter.length}`);
console.log(`Mutuality rate: ${Math.round((mutualCount / (mutualCount + oneWayAfter.length)) * 100)}%`);
console.log('═══════════════════════════════════════════════════\n');

// Show organizations with best mutual recognition
const mutualityScores = {};
Object.entries(recognizesAfter).forEach(([slug, targets]) => {
  let mutual = 0;
  targets.forEach(target => {
    if (recognizesAfter[target]?.has(slug)) {
      mutual++;
    }
  });
  mutualityScores[slug] = {
    mutual,
    total: targets.size,
    rate: targets.size > 0 ? Math.round((mutual / targets.size) * 100) : 0
  };
});

console.log('📊 Top Organizations by Mutual Recognition Rate:\n');

const topMutual = Object.entries(mutualityScores)
  .sort((a, b) => b[1].rate - a[1].rate)
  .slice(0, 10);

topMutual.forEach(([slug, score]) => {
  console.log(`  ${trees[slug]?.name || slug}: ${score.mutual}/${score.total} mutual (${score.rate}%)`);
});

console.log('\n📈 Recognition Count Distribution:\n');
const distribution = {};
Object.values(trees).forEach(tree => {
  const count = tree.recognizes.length;
  distribution[count] = (distribution[count] || 0) + 1;
});

Object.keys(distribution).sort((a, b) => Number(a) - Number(b)).forEach(count => {
  const bar = '█'.repeat(Math.floor(distribution[count] / 5));
  console.log(`  ${String(count).padStart(2)} recognitions: ${String(distribution[count]).padStart(3)} orgs ${bar}`);
});

