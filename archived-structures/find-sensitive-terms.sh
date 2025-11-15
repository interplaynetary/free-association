#!/bin/bash

# Script to find politically sensitive terms in documentation
# Usage: bash find-sensitive-terms.sh > sensitive-terms-report.txt

echo "========================================="
echo "POLITICALLY SENSITIVE TERMS AUDIT REPORT"
echo "========================================="
echo ""
echo "Generated: $(date)"
echo ""

# Define search locations
SEARCH_DIRS="research/ README.md DPI.md DPIv6.md PROTOCOL.md GOVERNANCE.md"

# Marxist/Communist terminology
echo "============================================"
echo "1. MARXIST/COMMUNIST TERMINOLOGY"
echo "============================================"
echo ""
grep -r -i -n --include="*.md" -E "(communis|marxis|marx\b)" $SEARCH_DIRS 2>/dev/null | head -30
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "(communis|marxis|marx\b)" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Bourgeois/Proletariat"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "(bourgeois|proletariat)" $SEARCH_DIRS 2>/dev/null
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "(bourgeois|proletariat)" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Class Struggle/War"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "(class.struggle|class.war|class.conflict)" $SEARCH_DIRS 2>/dev/null
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "(class.struggle|class.war|class.conflict)" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Revolutionary language
echo "============================================"
echo "2. REVOLUTIONARY/RADICAL LANGUAGE"
echo "============================================"
echo ""
echo "--------------------------------------------"
echo "Revolution/Revolutionary"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\brevolution" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\brevolution" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Radical/Radicalize"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\bradical" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\bradical" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Abolish/Abolition"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\babolish" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\babolish" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Overthrow"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\boverthrow" $SEARCH_DIRS 2>/dev/null
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\boverthrow" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Capitalism references
echo "============================================"
echo "3. CAPITALISM/CAPITALIST REFERENCES"
echo "============================================"
echo ""
grep -r -i -n --include="*.md" -E "\bcapitalis" $SEARCH_DIRS 2>/dev/null | head -30
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\bcapitalis" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Exploitation/Oppression
echo "============================================"
echo "4. EXPLOITATION/OPPRESSION LANGUAGE"
echo "============================================"
echo ""
echo "--------------------------------------------"
echo "Exploitation/Exploitative"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\bexploit" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\bexploit" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Oppression/Oppressor"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\boppress" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\boppress" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Wage labor/slavery
echo "============================================"
echo "5. WAGE LABOR/SLAVERY"
echo "============================================"
echo ""
grep -r -i -n --include="*.md" -E "(wage.slave|wage.labor)" $SEARCH_DIRS 2>/dev/null
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "(wage.slave|wage.labor)" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Alienation
echo "============================================"
echo "6. ALIENATION (MARXIST SENSE)"
echo "============================================"
echo ""
grep -r -i -n --include="*.md" -E "\balienat" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\balienat" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Private property (critical usage)
echo "============================================"
echo "7. PRIVATE PROPERTY (CRITICAL USAGE)"
echo "============================================"
echo ""
grep -r -i -n --include="*.md" -E "private.property" $SEARCH_DIRS 2>/dev/null | head -20
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "private.property" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Imperialism
echo "============================================"
echo "8. IMPERIALISM"
echo "============================================"
echo ""
grep -r -i -n --include="*.md" -E "\bimperial" $SEARCH_DIRS 2>/dev/null
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\bimperial" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Liberation/Resistance
echo "============================================"
echo "9. LIBERATION/RESISTANCE"
echo "============================================"
echo ""
echo "--------------------------------------------"
echo "Liberation"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\bliberat" $SEARCH_DIRS 2>/dev/null | head -15
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\bliberat" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

echo "--------------------------------------------"
echo "Resistance"
echo "--------------------------------------------"
grep -r -i -n --include="*.md" -E "\bresistance" $SEARCH_DIRS 2>/dev/null | head -15
echo ""
echo "Total occurrences: $(grep -r -i --include="*.md" -E "\bresistance" $SEARCH_DIRS 2>/dev/null | wc -l)"
echo ""

# Summary
echo "============================================"
echo "SUMMARY"
echo "============================================"
echo ""
echo "Total files searched:"
find research/ -name "*.md" 2>/dev/null | wc -l
echo ""
echo "Files with sensitive terms:"
grep -r -l -i --include="*.md" -E "(communis|marxis|marx\b|revolution|radical|abolish|capitalis|exploit|oppress|wage.slave|alienat|private.property|imperial|liberat|resistance)" $SEARCH_DIRS 2>/dev/null | wc -l
echo ""
echo "========================================="
echo "END OF REPORT"
echo "========================================="

