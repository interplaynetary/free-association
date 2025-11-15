# For Developers

## Development Setup

### Prerequisites

- [Bun](https://bun.sh/) - Fast JavaScript runtime and package manager
- Git
- Modern browser for testing

### Quick Start

```bash
# Clone repository
git clone https://github.com/interplaynetary/free-association.git
cd free-association

# Install dependencies
bun install

# Start development server
bun run dev

# Open browser to localhost (port shown in terminal)
```

### Available Commands

```bash
# Development
bun run dev          # Start dev server with hot reload

# Testing
npm test            # Run vitest and playwright tests

# Production
bun run build       # Build for production
bun run preview     # Preview production build
```

---

## Project Structure

### Key Directories

```
free-association/
├── src/                    # Source code
│   ├── lib/               # Core algorithm implementation
│   ├── components/        # UI components
│   ├── stores/            # State management
│   └── routes/            # Application routes
├── tests/                 # Test suite
├── docs/                  # Documentation (GitBook)
├── research/              # Research materials and experiments
└── static/                # Static assets
```

### Core Components

**Algorithm Implementation:**
- `src/lib/allocation/` - Resource allocation engine
- `src/lib/recognition/` - Recognition calculation
- `src/lib/convergence/` - System convergence logic

**Data Structures:**
- Entity profiles (recognition, capacity, needs)
- Network state management
- Allocation results

**UI Layer:**
- Network visualization
- Entity configuration
- Allocation display
- Real-time updates

---

## Architecture Overview

### System Design

**Peer-to-Peer Architecture:**
- No central server for coordination
- Each entity publishes data locally
- Distributed calculation
- Browser-based reference implementation

**Data Flow:**
1. Entities declare: recognition, capacity, needs
2. System calculates: mutual recognition, proportional shares
3. Algorithm determines: optimal allocation
4. Results display: transparent to all participants
5. Updates trigger: recalculation and convergence

**State Management:**
- Entity data stored locally
- Network state computed from entity declarations
- Allocation results derived from network state
- Updates propagate through reactive system

### Key Algorithms

**Mutual Recognition Calculation:**
```javascript
function calculateMutualRecognition(entityA, entityB) {
  const aRecognizesB = entityA.recognition[entityB.id] || 0;
  const bRecognizesA = entityB.recognition[entityA.id] || 0;
  return Math.min(aRecognizesB, bRecognizesA);
}
```

**Proportional Share Calculation:**
```javascript
function calculateShare(recipient, provider, allRecipients) {
  const mutualRecognition = calculateMutualRecognition(recipient, provider);
  const totalMR = allRecipients.reduce((sum, r) => 
    sum + calculateMutualRecognition(r, provider), 0
  );
  return totalMR > 0 ? mutualRecognition / totalMR : 0;
}
```

**Allocation with Need Cap:**
```javascript
function allocate(recipient, provider, share) {
  const rawAllocation = provider.capacity * share;
  return Math.min(rawAllocation, recipient.declaredNeed);
}
```

---

## Development Priorities

### Current Focus Areas

**1. User Interface Refinement**
- Network visualization improvements
- Entity configuration workflow
- Allocation result clarity
- Mobile responsiveness

**2. Protocol Implementation**
- P2P communication layer
- Data synchronization
- Identity management
- Security considerations

**3. Testing and Validation**
- Algorithm correctness tests
- Convergence property validation
- Edge case handling
- Performance optimization

**4. Documentation**
- API documentation
- Integration guides
- Example implementations
- Best practices

### Contribution Areas

**High Priority:**
- UI/UX improvements
- Test coverage expansion
- Documentation enhancement
- Performance optimization

**Medium Priority:**
- Additional visualization options
- Export/import functionality
- Analytics and metrics
- Integration examples

**Exploratory:**
- Alternative front-end implementations
- Mobile applications
- API development
- Protocol extensions

---

## Contributing

### Ways to Contribute

**Code Contributions:**
- Feature development
- Bug fixes
- Test coverage
- Performance improvements

**Documentation:**
- Technical documentation
- Tutorials and guides
- Example implementations
- Translation

**Testing:**
- Manual testing and bug reports
- Automated test development
- Edge case identification
- Performance benchmarking

**Design:**
- UI/UX improvements
- Visualization enhancements
- Accessibility
- Mobile experience

### Contribution Process

**1. Explore Codebase**
- Review existing code and structure
- Run development environment
- Understand core algorithms
- Identify contribution area

**2. Discuss Approach**
- Open issue describing proposed work
- Discuss implementation approach
- Align with project direction
- Coordinate with other contributors

**3. Develop**
- Create feature branch
- Implement changes
- Write/update tests
- Document as needed

**4. Submit**
- Open pull request
- Describe changes and rationale
- Address review feedback
- Merge when approved

### Development Guidelines

**Code Style:**
- Follow existing code patterns
- Use meaningful variable names
- Comment complex logic
- Keep functions focused

**Testing:**
- Write tests for new features
- Ensure existing tests pass
- Include edge cases
- Test convergence properties

**Documentation:**
- Update relevant documentation
- Add inline comments for complex code
- Include usage examples
- Document breaking changes

**Commits:**
- Clear, descriptive commit messages
- Logical commit organization
- Reference related issues
- Keep commits focused

---

## Technical Resources

### Protocol Specification

See [Protocol Specification](../technical/protocol.md) for formal protocol documentation including:
- Data structures
- Calculation algorithms
- Network communication
- Security considerations

### Mathematical Foundations

See [Mathematical Foundations](../technical/mathematics.md) for:
- Formal properties
- Proof sketches
- Convergence analysis
- Fairness guarantees

### Research Materials

See `research/` directory for:
- Theoretical explorations
- Alternative designs
- Use case analysis
- Experimental features

---

## Development Workflow

### Local Development

**1. Start Development Server**
```bash
bun run dev
```
- Hot module replacement enabled
- Changes reflect immediately
- Console shows compilation status

**2. Make Changes**
- Edit source files
- Browser updates automatically
- Check console for errors
- Test functionality

**3. Run Tests**
```bash
npm test
```
- Unit tests with vitest
- Integration tests with playwright
- Coverage reports available

**4. Build for Production**
```bash
bun run build
```
- Optimized production bundle
- Static files in `dist/`
- Preview with `bun run preview`

### Testing Strategy

**Unit Tests:**
- Algorithm correctness
- Individual function behavior
- Edge case handling
- Data structure manipulation

**Integration Tests:**
- Full allocation scenarios
- Network dynamics
- Convergence properties
- Multi-entity interactions

**Manual Testing:**
- UI workflows
- Visualization accuracy
- User experience
- Cross-browser compatibility

---

## Getting Help

### Resources

**Documentation:**
- This guide
- [Protocol Specification](../technical/protocol.md)
- [Mathematical Foundations](../technical/mathematics.md)
- Inline code comments

**Community:**
- [GitHub Issues](https://github.com/interplaynetary/free-association/issues)
- Email: info@openassociation.org

**Code Review:**
- Open pull request for feedback
- Tag maintainers for review
- Discuss in issue comments

### Contact

**Technical Questions:**
- Open GitHub issue
- Email: info@openassociation.org

**Development Coordination:**
- Discuss in existing issues
- Propose new features via issues

---

## Supporting the Project

### Financial Support

Support infrastructure and development:

<a href="https://opencollective.com/free-association">
    <img width="300" src="https://opencollective.com/free-association/donate/button@2x.png?color=blue" />
</a>

### Non-Code Contributions

- Share with developer communities
- Write tutorials or blog posts
- Create example implementations
- Provide feedback and testing
- Improve documentation
- Translate materials

---

## License

**GNU Affero General Public License v3.0** with [Additional Terms](../../LICENSE-ADDITIONAL-TERMS.md)

**Key Points:**
- Open source, anyone can use
- Network service requires sharing source
- Attribution required
- Modified versions must be clearly marked
- Interoperability commitment

See [License & Governance](../project/license.md) for details.

