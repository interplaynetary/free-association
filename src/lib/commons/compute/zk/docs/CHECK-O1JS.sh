#!/bin/bash
# Check o1js version and IndexedMerkleMap availability

echo "=== o1js Version Check ==="
echo ""

# Check installed version
INSTALLED_VERSION=$(npm list o1js --depth=0 2>/dev/null | grep o1js | awk '{print $2}' | sed 's/@//')
echo "Installed version: $INSTALLED_VERSION"

# Check latest version
LATEST_VERSION=$(npm view o1js version)
echo "Latest version: $LATEST_VERSION"

echo ""
echo "=== Experimental API Check ==="
echo ""

# Check what's in Experimental
node -e "
const o1js = require('o1js');
const exp = o1js.Experimental || {};
const keys = Object.keys(exp).sort();

console.log('Available Experimental APIs:');
keys.forEach(k => console.log('  - ' + k));

console.log('');
console.log('IndexedMerkleMap available:', !!exp.IndexedMerkleMap);

if (exp.IndexedMerkleMap) {
  console.log('✅ IndexedMerkleMap is AVAILABLE!');
  console.log('   You can now use provable DAG operations.');
  console.log('   See DAG-FUTURE.md for implementation.');
} else {
  console.log('❌ IndexedMerkleMap is NOT available in this version.');
  console.log('   Current ZK module works without it.');
  console.log('   Upgrade to latest o1js to get IndexedMerkleMap.');
}
"

echo ""
echo "=== Upgrade Instructions ==="
echo ""
echo "To upgrade o1js:"
echo "  npm install o1js@latest"
echo ""
echo "After upgrading, run this script again to verify."

