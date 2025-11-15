#!/bin/bash

# validate-org-pr.sh
# Validates that a PR comes from the correct organizational account

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

print_info() { echo -e "${BLUE}ℹ${NC} $1"; }
print_success() { echo -e "${GREEN}✓${NC} $1"; }
print_error() { echo -e "${RED}✗${NC} $1"; }

usage() {
    cat << EOF
Usage: $0 GITHUB_ACCOUNT TARGET_PATH

Validates if a GitHub account can make PRs to the target path.

EXAMPLES:
    $0 undp research/institutions/undp/climate-policy.md
    $0 alice research/individual/alice/paper.md
    $0 platform-coop research/private-sector/platform-coop/governance/

RULES:
    - GitHub account name must match the organization folder name
    - Can only PR to your own organization's folder
    - Cross-organization PRs are rejected

EOF
    exit 1
}

if [ $# -lt 2 ]; then
    usage
fi

GITHUB_ACCOUNT=$1
TARGET_PATH=$2

echo ""
print_info "Validating PR permissions"
echo ""

# Extract org folder from target path
# Expected format: research/[form]/[org-name]/...
if [[ ! "$TARGET_PATH" =~ ^research/[^/]+/([^/]+) ]]; then
    print_error "Invalid target path format"
    print_error "Expected: research/[form]/[org-name]/..."
    print_error "Got: $TARGET_PATH"
    exit 1
fi

ORG_NAME="${BASH_REMATCH[1]}"

echo "GitHub Account: $GITHUB_ACCOUNT"
echo "Target Path: $TARGET_PATH"
echo "Organization: $ORG_NAME"
echo ""

# Validate
if [ "$GITHUB_ACCOUNT" == "$ORG_NAME" ]; then
    print_success "VALID: Account matches organization"
    print_success "PR from @$GITHUB_ACCOUNT to $TARGET_PATH is ALLOWED"
    echo ""
    exit 0
else
    print_error "INVALID: Account does not match organization"
    print_error "PR from @$GITHUB_ACCOUNT to $TARGET_PATH is REJECTED"
    echo ""
    print_info "To contribute to $ORG_NAME:"
    print_info "  1. Open an issue or discussion"
    print_info "  2. Coordinate with @$ORG_NAME"
    print_info "  3. They can incorporate your contribution"
    echo ""
    exit 1
fi

