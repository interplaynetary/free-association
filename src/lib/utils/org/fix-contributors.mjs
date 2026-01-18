/**
 * Fix Contributors - Remove from Non-Leaf Nodes
 * Contributors should ONLY exist on leaf nodes (nodes with no children)
 */

import fs from 'fs';

const trees = JSON.parse(fs.readFileSync('./src/lib/demo/orgs.json', 'utf8'));

let fixed = 0;
let totalContributorsRemoved = 0;

console.log('🔧 Fixing contributors on non-leaf nodes...\n');

Object.entries(trees).forEach(([slug, tree]) => {
  tree.tree.children.forEach(priority => {
    // If this priority has children (not a leaf) AND has contributors, remove them
    if (priority.children && priority.children.length > 0 && priority.contributors && priority.contributors.length > 0) {
      console.log(`${slug}: Removing ${priority.contributors.length} contributors from '${priority.name}' (has ${priority.children.length} children)`);
      totalContributorsRemoved += priority.contributors.length;
      priority.contributors = [];
      fixed++;
    }

    // Also check sub-priorities (though they should be leaf nodes)
    priority.children.forEach(subPriority => {
      if (subPriority.children && subPriority.children.length > 0 && subPriority.contributors && subPriority.contributors.length > 0) {
        console.log(`${slug}: Removing ${subPriority.contributors.length} contributors from sub-priority '${subPriority.name}' (has ${subPriority.children.length} children)`);
        totalContributorsRemoved += subPriority.contributors.length;
        subPriority.contributors = [];
        fixed++;
      }
    });
  });
});

// Save
fs.writeFileSync('./src/lib/demo/orgs.json', JSON.stringify(trees, null, 2), 'utf8');

console.log('\n═══════════════════════════════════════');
console.log('✅ FIX COMPLETE');
console.log('═══════════════════════════════════════');
console.log(`Fixed nodes: ${fixed}`);
console.log(`Contributors removed: ${totalContributorsRemoved}`);
console.log('═══════════════════════════════════════\n');

// Verify fix
let remaining = 0;
Object.values(trees).forEach(tree => {
  tree.tree.children.forEach(priority => {
    if (priority.children && priority.children.length > 0 && priority.contributors && priority.contributors.length > 0) {
      remaining++;
    }
    priority.children.forEach(subPriority => {
      if (subPriority.children && subPriority.children.length > 0 && subPriority.contributors && subPriority.contributors.length > 0) {
        remaining++;
      }
    });
  });
});

if (remaining === 0) {
  console.log('✅ Verification: No non-leaf nodes have contributors!');

  // Count leaf nodes with contributors
  let leafWithContributors = 0;
  let totalLeafContributors = 0;

  Object.values(trees).forEach(tree => {
    tree.tree.children.forEach(priority => {
      priority.children.forEach(subPriority => {
        // If it has no children (is a leaf), count contributors
        if (!subPriority.children || subPriority.children.length === 0) {
          if (subPriority.contributors && subPriority.contributors.length > 0) {
            leafWithContributors++;
            totalLeafContributors += subPriority.contributors.length;
          }
        }
      });
    });
  });

  console.log(`📊 Leaf nodes with contributors: ${leafWithContributors}`);
  console.log(`📊 Total contributor relationships on leaf nodes: ${totalLeafContributors}`);
} else {
  console.log(`❌ ERROR: ${remaining} non-leaf nodes still have contributors!`);
}

