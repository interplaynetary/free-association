#!/bin/bash

# create-org-expression.sh
# Creates an organizational expression folder

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

print_info() { echo -e "${BLUE}ℹ${NC} $1"; }
print_success() { echo -e "${GREEN}✓${NC} $1"; }
print_warning() { echo -e "${YELLOW}⚠${NC} $1"; }
print_error() { echo -e "${RED}✗${NC} $1"; }

RESEARCH_ROOT="research"

usage() {
    cat << EOF
Usage: $0 FORM ORG_NAME [OPTIONS]

Creates an organizational expression folder in the research directory.

FORMS:
    institutions    - UN agencies, World Bank, IMF, etc.
    nations         - National governments
    civil-society   - NGOs, movements, coalitions
    private-sector  - Corporations, cooperatives, startups
    academic        - Universities, research institutes
    individual      - Individual researchers

ORG_NAME:
    Must match the GitHub account name that will make PRs

OPTIONS:
    --help          Show this help message

EXAMPLES:
    $0 institutions undp
    $0 individual alice
    $0 civil-society climate-action-network
    $0 private-sector platform-cooperative

EOF
    exit 1
}

if [ $# -lt 2 ]; then
    usage
fi

FORM=$1
ORG_NAME=$2

# Validate form
case "$FORM" in
    institutions|nations|civil-society|private-sector|academic|individual|commons)
        ;;
    *)
        print_error "Invalid form: $FORM"
        echo ""
        usage
        ;;
esac

# Validate org name
if [[ ! "$ORG_NAME" =~ ^[a-zA-Z0-9_-]+$ ]]; then
    print_error "Org name can only contain letters, numbers, hyphens, and underscores"
    exit 1
fi

ORG_PATH="$RESEARCH_ROOT/$FORM/$ORG_NAME"

echo ""
print_info "Creating organizational expression folder"
echo ""

# Check if already exists
if [ -d "$ORG_PATH" ]; then
    print_error "Organization folder already exists: $ORG_PATH"
    exit 1
fi

# Create folder
mkdir -p "$ORG_PATH"

# Create README
cat > "$ORG_PATH/README.md" << EOF
# ${ORG_NAME}

**Organizational Form**: ${FORM}  
**GitHub Account**: ${ORG_NAME}  
**Created**: $(date +%Y-%m-%d)

## About

[Description of your organization and its mission]

## Expression Organization

This folder contains our organizational expressions. We organize our content as follows:

[Describe your folder structure and organization principles]

## Contributing

This folder is governed by **${ORG_NAME}**. 

To contribute:
- If you are part of ${ORG_NAME}: Submit PRs from the ${ORG_NAME} GitHub account
- If you are external: Open an issue or discussion to coordinate

## Contact

- GitHub: @${ORG_NAME}
- [Other contact information]

---

**Access Control**: Only PRs from @${ORG_NAME} to this folder are accepted.

See [EXPRESSION-PROTOCOL.md](../../EXPRESSION-PROTOCOL.md) for the full protocol.
EOF

print_success "Created: $ORG_PATH"
print_success "Created: $ORG_PATH/README.md"

echo ""
print_info "Next steps:"
print_info "  1. Edit $ORG_PATH/README.md with your organization info"
print_info "  2. Organize your expressions however you like"
print_info "  3. Submit PRs from @${ORG_NAME} GitHub account"
print_info "  4. Only PRs from @${ORG_NAME} to $ORG_PATH will be accepted"

echo ""
print_success "Organization expression folder ready!"
echo ""

