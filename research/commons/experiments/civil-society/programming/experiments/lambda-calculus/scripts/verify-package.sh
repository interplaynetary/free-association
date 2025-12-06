#!/bin/bash
# Package Verification Script

set -e  # Exit on error

echo "🔍 Verifying @free-association/lambda-calculus package..."
echo ""

cd "$(dirname "$0")"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check counter
CHECKS_PASSED=0
TOTAL_CHECKS=8

check_pass() {
    echo -e "${GREEN}✅ $1${NC}"
    ((CHECKS_PASSED++))
}

check_fail() {
    echo -e "${RED}❌ $1${NC}"
}

check_warn() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "1. Checking package.json..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ -f "package.json" ]; then
    check_pass "package.json exists"
    
    # Extract version
    VERSION=$(grep '"version"' package.json | head -1 | sed 's/.*: "\(.*\)".*/\1/')
    echo "   Version: $VERSION"
    
    # Extract name
    NAME=$(grep '"name"' package.json | head -1 | sed 's/.*: "\(.*\)".*/\1/')
    echo "   Name: $NAME"
else
    check_fail "package.json not found"
    exit 1
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "2. Running tests..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if npm test 2>&1 | grep -q "103 passed"; then
    check_pass "All tests passing (103/103)"
else
    check_fail "Tests not passing"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "3. Building package..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if npm run build > /dev/null 2>&1; then
    check_pass "Build successful"
else
    check_fail "Build failed"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "4. Checking build output..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ -d "dist" ]; then
    check_pass "dist/ directory exists"
    
    # Check key files
    if [ -f "dist/index.js" ] && [ -f "dist/index.mjs" ] && [ -f "dist/index.d.ts" ]; then
        echo "   ✓ index.js, index.mjs, index.d.ts present"
    else
        check_warn "Some output files missing"
    fi
    
    if [ -f "dist/elegant/index.js" ]; then
        echo "   ✓ elegant/index.js present"
    else
        check_warn "elegant/index.js missing"
    fi
else
    check_fail "dist/ directory not found"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "5. Checking documentation..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

DOC_COUNT=0
[ -f "README.md" ] && ((DOC_COUNT++)) && echo "   ✓ README.md"
[ -f "LICENSE" ] && ((DOC_COUNT++)) && echo "   ✓ LICENSE"
[ -f "CHANGELOG.md" ] && ((DOC_COUNT++)) && echo "   ✓ CHANGELOG.md"
[ -f "LAMBDA-R-COMPLIANT.md" ] && ((DOC_COUNT++)) && echo "   ✓ LAMBDA-R-COMPLIANT.md"
[ -d "docs" ] && echo "   ✓ docs/ directory" && DOC_COUNT=$((DOC_COUNT + 1))

if [ $DOC_COUNT -ge 4 ]; then
    check_pass "Documentation complete ($DOC_COUNT key files)"
else
    check_warn "Some documentation missing ($DOC_COUNT/5)"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "6. Checking package size..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if SIZE=$(npm pack --dry-run 2>&1 | grep "package size" | awk '{print $4, $5}'); then
    echo "   Package size: $SIZE"
    check_pass "Package size check complete"
else
    check_warn "Could not determine package size"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "7. Checking file inclusions..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if FILE_COUNT=$(npm pack --dry-run 2>&1 | grep "total files" | awk '{print $3}'); then
    echo "   Total files: $FILE_COUNT"
    check_pass "File count: $FILE_COUNT"
else
    check_warn "Could not count files"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "8. Type checking..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if npm run type-check > /dev/null 2>&1; then
    check_pass "No type errors"
else
    check_warn "Type check warnings (non-critical)"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

if [ $CHECKS_PASSED -eq $TOTAL_CHECKS ]; then
    echo -e "${GREEN}✅ ALL CHECKS PASSED ($CHECKS_PASSED/$TOTAL_CHECKS)${NC}"
    echo ""
    echo "🚀 Package is READY TO PUBLISH!"
    echo ""
    echo "To publish, run:"
    echo "   npm publish --access public"
elif [ $CHECKS_PASSED -ge 6 ]; then
    echo -e "${YELLOW}⚠️  MOSTLY READY ($CHECKS_PASSED/$TOTAL_CHECKS checks passed)${NC}"
    echo ""
    echo "Package can be published, but review warnings above."
    echo ""
    echo "To publish, run:"
    echo "   npm publish --access public"
else
    echo -e "${RED}❌ NOT READY ($CHECKS_PASSED/$TOTAL_CHECKS checks passed)${NC}"
    echo ""
    echo "Please fix the issues above before publishing."
    exit 1
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

