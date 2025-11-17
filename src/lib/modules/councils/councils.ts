import { z } from 'zod';

// Councils
// Councils elect and send rotating revokable delegates with mandates representing their interests/needs to other councils of which they are a member. The decisions of the target-councils (target-council) are binding to its' member-councils, to not accept/follow the decision is to revoke membership (the target-council can vote to revoke membership).

// internal-membership criteria of source-councilshould be approved by the target-council in order to accept the number of members that the mandate carries as voting power within the council.
	
// Proposals should be processed in the Council.
// Gathering support from delegates, once a quorum is reached, the proposal is passed.
// the weight of a delegates support should = delegate.mandate.supporters.length
// the quorum should be 50% of the total voting power of the council.

// The proposal system should include sending a delegate! 
// As well as approving the validity of the mandate.supporters.length

// A council can send only one delegate/mandate to another council.

// Improvements:
// list of supporters should be the supporters of the proposal (should be auto-approved via voting)

// verbal-processes -> everything is recorded. Share of speech-time = share of votingPower.
// verbal-processes are sent with the mandate!

// controllers can be sent to enforce the decisions of the soviet.

// optional observer from the local-council. to ensure the voter votes according to their mandate! For immediate revokation!
// the representative who comes back to the local-council, secretary, and observer, reveal he hasnt enforced the mandate, and we take a vote: is he revoked?
// how? observer view :: from the council-view, live-updates.

// signal violation! of Mandate! Enought support for violation, leads to automatic proposal for revokation!

// mandate is a list of desires.

// proposal is a Map of entities (councils) -> actions to be taken! (description, method call)

// a mandate is in a sense a proposal that is already approved.

/*

Here's an explanation of the system in its own terms:

The system we've built is a flexible framework for coordinated decision-making across interconnected autonomous groups. At its core, it enables groups (councils) to make decisions that affect both their internal operations and their relationships with other groups, while maintaining clear lines of accountability and democratic control.

Each council operates through a system of proposals and delegates. Proposals are structured decisions that can specify actions to be taken by one or multiple councils. These actions, once approved, can execute specific methods or trigger changes across the system. The brilliant part is that delegation itself works through this same proposal mechanism - when a council needs to interact with another council, they do so by first creating a proposal (mandate) that specifies who will represent them and what their scope of authority will be.

The delegate system is particularly innovative because it maintains continuous democratic legitimacy. A delegate's voting power in their target council is directly tied to the ongoing support for their mandate-proposal in their source council. This creates a dynamic system where authority is never truly transferred but rather continuously validated by active support. If support for a delegate's mandate changes, their voting power adjusts automatically, ensuring they always accurately represent their current base of support.

The system also handles inter-council relationships elegantly. When a council makes a decision that affects its member councils, it can specify different actions for different councils within a single proposal. The system then tracks whether member councils accept and implement these decisions, providing a mechanism for maintaining coordination while respecting autonomy. Councils that consistently reject decisions may have their membership relationship reviewed, ensuring that the network of councils maintains coherence in its decision-making.

What makes this system powerful is its unification of concepts. Everything flows through the proposal mechanism - from simple decisions to delegate election to inter-council coordination. This creates a system that is both simple to understand and flexible enough to handle complex organizational needs. The use of modern programming concepts like private fields, revocable proxies, and async iterators ensures that the system is secure, maintainable, and capable of handling real-world complexity while maintaining its conceptual clarity.

*/

// TODO: secure the Council class's properties from being accessed improperly!
// TODO: make better bootstrapping.
// TODO: add a method to delete proposals! Or to hide them from the UI!

// ==================== Zod Schemas ====================

const VoteDecisionSchema = z.enum(['yes', 'no', 'abstain']);
type VoteDecision = z.infer<typeof VoteDecisionSchema>;

const ActionSchema = z.object({
    description: z.string(),
    methodName: z.string().nullable(),
    methodArgs: z.array(z.any())
});
type Action = z.infer<typeof ActionSchema>;

const VoteResultSchema = z.object({
    yes: z.number().nonnegative(),
    no: z.number().nonnegative()
});
type VoteResult = z.infer<typeof VoteResultSchema>;

const ProposalStatusSchema = z.object({
    proposal: z.any(), // Will be Proposal instance
    description: z.string(),
    votes: VoteResultSchema,
    totalVotingPower: z.number().nonnegative(),
    quorum: z.number().nonnegative(),
    isApproved: z.boolean()
});
type ProposalStatus = z.infer<typeof ProposalStatusSchema>;

const ResponseStatusSchema = z.object({
    council: z.string(),
    proposal: z.string(),
    accepted: z.boolean(),
    completed: z.boolean()
});
type ResponseStatus = z.infer<typeof ResponseStatusSchema>;

// ==================== Type Definitions ====================

type Voter = Member | Delegate;
type CouncilMember = Member | CouncilProxy;

interface DelegateEntry {
    proxy: Delegate;
    revoke: () => void;
}

interface CouncilProxy extends Council {
    proxyRef: CouncilProxy | null;
}

interface BootstrapInterface {
    addMember: (memberName: string) => Member;
    addMethod: (methodName: string, method: (...args: any[]) => any) => CouncilProxy;
}

// ==================== Classes ====================

class Delegate {
    public readonly name: string;
    public readonly mandate: Proposal;
    public readonly from: CouncilProxy;
    public readonly to: CouncilProxy;

    constructor(name: string, mandate: Proposal, from: CouncilProxy, to: CouncilProxy) {
        this.name = z.string().parse(name);
        this.mandate = mandate;
        this.from = from;
        this.to = to;
    }

    propose(description: string, actions: Map<CouncilProxy, Action> = new Map()): Proposal {
        return this.to.addProposal(description, actions);
    }

    castVote(proposal: Proposal, decision: VoteDecision): void {
        const validatedDecision = VoteDecisionSchema.parse(decision);
        const weight = this.mandate.supporters.length;
        console.log(`${this.name} votes ${validatedDecision} on proposal: ${proposal.description} with weight ${weight}`);
        this.to.castVote(this, proposal, validatedDecision);
    }
}


class Proposal {
    public readonly description: string;
    public readonly votes: Map<Voter, VoteDecision>;
    public readonly actions: Map<CouncilProxy, Action>;

    constructor(description: string) {
        this.description = z.string().parse(description);
        this.votes = new Map();
        this.actions = new Map();
    }

    addAction(
        council: CouncilProxy,
        description: string,
        methodName: string | null = null,
        methodArgs: any[] = []
    ): void {
        const validatedAction = ActionSchema.parse({
            description,
            methodName,
            methodArgs
        });
        this.actions.set(council, validatedAction);
    }

    castVote(voter: Voter, vote: VoteDecision): void {
        const validatedVote = VoteDecisionSchema.parse(vote);
        this.votes.set(voter, validatedVote);
    }

    getCurrentVotes(): VoteResult {
        let yes = 0;
        let no = 0;
        
        this.votes.forEach((vote, voter) => {
            const weight = 'mandate' in voter && voter.mandate ? voter.mandate.supporters.length : 1;
            
            if (vote === 'yes') {
                yes += weight;
            } else if (vote === 'no') {
                no += weight;
            }
        });

        return VoteResultSchema.parse({ yes, no });
    }

    getActionsForCouncil(council: CouncilProxy): Action | undefined {
        console.log('Looking up actions for:', council);
        console.log('Available actions:', Array.from(this.actions.entries()));
        
        const action = this.actions.get(council);
        return action;
    }

    get allActions(): Array<Action & { council: string }> {
        return Array.from(this.actions.entries()).map(([council, action]) => ({
            council: council.name,
            ...action
        }));
    }

    get supporters(): Voter[] {
        return Array.from(this.votes.entries())
            .filter(([_, vote]) => vote === 'yes')
            .map(([voter, _]) => voter);
    }
}

class Member {
    public readonly name: string;
    public readonly council: CouncilProxy;

    constructor(name: string, council: CouncilProxy) {
        this.name = z.string().parse(name);
        this.council = council;
    }

    castVote(proposal: Proposal, decision: VoteDecision): void {
        const validatedDecision = VoteDecisionSchema.parse(decision);
        console.log(`${this.name} votes ${validatedDecision} on proposal: ${proposal.description}`);
        this.council.castVote(this, proposal, validatedDecision);
    }
}


// we are going to want to protect the methods from being executed improperly!
class Council {
    #name: string;
    #members: CouncilMember[] = [];
    #delegates: DelegateEntry[] = [];
    #proposals: Proposal[] = [];
    #pendingResponses: Map<CouncilProxy, Map<string, Proposal>> = new Map();
    
    public proxyRef: CouncilProxy | null = null;
    public isExecutingProposal?: boolean = false;
    [key: string]: any; // Index signature for dynamic methods

    constructor(name: string) {
        this.#name = z.string().parse(name);
    }

    bootstrap(): BootstrapInterface {
        return {
            addMember: (memberName: string): Member => {
                if (!this.proxyRef) {
                    throw new Error('Council proxy reference not set');
                }
                const member = new Member(memberName, this.proxyRef);
                this.#members.push(member);
                console.log(`Bootstrapped member ${member.name} added to ${this.#name}`);
                return member;
            },
            addMethod: (methodName: string, method: (...args: any[]) => any): CouncilProxy => {
                if (!this.proxyRef) {
                    throw new Error('Council proxy reference not set');
                }
                this[methodName] = method;
                console.log(`Bootstrapped method ${methodName} added to ${this.#name}`);
                return this.proxyRef;
            }
        };
    }

    get name(): string {
        return this.#name;
    }

    get members(): CouncilMember[] {
        return [...this.#members];
    }

    get delegates(): Delegate[] {
        console.log('Getting delegates:', this.#delegates);
        return this.#delegates.map(delegate => delegate.proxy);
    }

    get proposals(): Proposal[] {
        return [...this.#proposals];
    }

    addProposal(description: string, actions: Map<CouncilProxy, Action> = new Map()): Proposal {
        console.log('Adding proposal with actions:', actions);
        const proposal = new Proposal(description);
        
        actions.forEach((action, council) => {
            console.log(`Adding action for council ${council.name}:`, action);
            proposal.addAction(council, action.description, action.methodName, action.methodArgs);
        });

        this.#proposals.push(proposal);
        return proposal;
    }

    castVote(voter: Voter, proposal: Proposal, decision: VoteDecision): void {
        const validatedDecision = VoteDecisionSchema.parse(decision);
        
        const isDirectMember = this.#members.includes(voter as CouncilMember);
        const isDelegateFromMemberCouncil = 'mandate' in voter && voter.mandate && 
            this.#members.includes((voter as Delegate).from);

        if (!isDirectMember && !isDelegateFromMemberCouncil) {
            console.log(`${voter.name} is not a member or delegate from a member of ${this.#name}`);
            return;
        }

        proposal.castVote(voter, validatedDecision);
        console.log(`${voter.name} votes ${validatedDecision} on proposal: ${proposal.description}`);
    }

    addMember(member: CouncilMember): void {
        this.#members.push(member);
    }

    electDelegate(delegateName: string, mandateDescription: string, targetCouncil: CouncilProxy): Delegate | undefined {
        console.log('Starting electDelegate method');
        const existingDelegate = this.#delegates.find(d => d.proxy.to === targetCouncil);
        if (existingDelegate) {
            console.log(`A delegate already exists for ${targetCouncil.name}. Cannot send another delegate.`);
            return undefined;
        }

        const mandate = this.#proposals.find(p => p.description === mandateDescription);
        if (!mandate) {
            console.log(`No mandate proposal found with description: ${mandateDescription}`);
            return undefined;
        }

        console.log('Using mandate proposal:', mandate);
        
        if (!this.proxyRef) {
            throw new Error('Council proxy reference not set');
        }

        const { proxy: mandateProxy, revoke: _revokeMandate } = Proxy.revocable(mandate, {});
        const delegate = new Delegate(delegateName, mandateProxy, this.proxyRef, targetCouncil);
        
        console.log('Delegate created:', delegate);

        const { proxy: delegateProxy, revoke: revokeDelegate } = Proxy.revocable(delegate, {});
        
        console.log('Current delegates before push:', this.#delegates);
        this.#delegates.push({ proxy: delegateProxy, revoke: revokeDelegate });
        console.log('Current delegates after push:', this.#delegates);

        console.log(`Delegate ${delegateProxy.name} created and added to ${this.#name}`);
        return delegateProxy;
    }

    withdrawDelegate(delegateProxy: Delegate): void {
        const delegateEntry = this.#delegates.find(d => d.proxy === delegateProxy);
        if (delegateEntry) {
            delegateEntry.revoke();
            this.#delegates = this.#delegates.filter(d => d.proxy !== delegateProxy);
            console.log(`${delegateProxy.name} with mandate "${delegateProxy.mandate.description}" has been revoked from ${this.#name}`);
            delegateProxy.to.removeDelegate(delegateProxy);
        }
    }

    substituteDelegate(delegateProxy: Delegate, newDelegateName: string, newMandateDescription: string): void {
        const delegateEntry = this.#delegates.find(d => d.proxy === delegateProxy);
        if (delegateEntry) {
            delegateEntry.revoke();
            const newMandate = new Proposal(newMandateDescription);
            const { proxy: newMandateProxy, revoke: _revokeNewMandate } = Proxy.revocable(newMandate, {});
            
            if (!this.proxyRef) {
                throw new Error('Council proxy reference not set');
            }
            
            const newDelegate = new Delegate(newDelegateName, newMandateProxy, this.proxyRef, delegateProxy.to);
            const { proxy: newDelegateProxy, revoke: revokeNewDelegate } = Proxy.revocable(newDelegate, {});

            this.#delegates = this.#delegates.map(d => d.proxy === delegateProxy ? { proxy: newDelegateProxy, revoke: revokeNewDelegate } : d);
            console.log(`${delegateProxy.name} with mandate "${delegateProxy.mandate.description}" has been replaced by ${newDelegate.name} with mandate "${newDelegate.mandate.description}" in ${this.#name}`);
            delegateProxy.to.removeDelegate(delegateProxy);
        }
    }

    revokeMandate(_mandateProxy: Proposal): void {
        // Note: This method references 'this.mandates' which doesn't exist in the current implementation
        console.log(`revokeMandate called but mandates tracking not implemented`);
    }

    removeDelegate(delegate: Delegate): void {
        this.#delegates = this.#delegates.filter(d => d.proxy !== delegate);
        console.log(`${delegate.name} has been removed from ${this.#name}`);
    }

    #execute(proposal: Proposal): void {
        if (!this.proxyRef) {
            throw new Error('Council proxy reference not set');
        }
        
        const actions = proposal.getActionsForCouncil(this.proxyRef);
        console.log('Executing proposal with actions:', actions);
        console.log('Available methods on council:', Object.getOwnPropertyNames(this));
        console.log('Looking for method:', actions?.methodName);
        
        if (actions && actions.methodName && typeof this[actions.methodName] === 'function') {
            console.log(`Executing method ${actions.methodName} on ${this.#name}`);
            this[actions.methodName](...actions.methodArgs);
        } else {
            console.log(`No valid method to execute for proposal: ${proposal.description}`);
            if (actions) {
                const methodExists = actions.methodName ? typeof this[actions.methodName] : 'undefined';
                console.log('Action exists but method not found. Action details:', {
                    methodName: actions.methodName,
                    methodExists: methodExists,
                    methodArgs: actions.methodArgs
                });
            } else {
                console.log('No actions found for this council');
            }
        }
    }

    async checkMemberResponses(): Promise<ResponseStatus[]> {
        const responseStatus: ResponseStatus[] = [];
        
        for (const [council, proposals] of this.#pendingResponses) {
            for (const [description, proposal] of proposals) {
                const status: ResponseStatus = {
                    council: council.name,
                    proposal: description,
                    accepted: (proposal as any).isApproved ?? false,
                    completed: (proposal as any).isComplete ?? false
                };
                responseStatus.push(status);
                
                if (status.completed && !status.accepted) {
                    // Could trigger membership review process
                }
            }
        }
        
        return responseStatus;
    }

    revokeMembership(council: CouncilProxy): void {
        this.#members = this.#members.filter(c => c !== council);
        console.log(`${council.name} has been revoked from ${this.#name}`);
    }

    addMethod(methodName: string, method: (...args: any[]) => any): void {
        this[methodName] = method;
    }

    get memberVotingPower(): number {
        return this.#members.reduce((sum, member) => {
            if ('memberVotingPower' in member && typeof member.memberVotingPower === 'number') {
                return sum + member.memberVotingPower;
            }
            return sum + 1;
        }, 0);
    }
    
    async *processProposals(): AsyncGenerator<ProposalStatus, void, unknown> {
        for (const proposal of this.#proposals) {
            const currentVotes = proposal.getCurrentVotes();
            
            const delegateVotingPower = this.#delegates
                .reduce((sum, delegate) => {
                    return sum + (delegate.proxy.mandate.supporters?.length || 0);
                }, 0);
                
            const memberVotingPower = this.#members.length;
            const totalVotingPower = delegateVotingPower + memberVotingPower;
            
            const quorum = totalVotingPower * 0.5;

            const status: ProposalStatus = ProposalStatusSchema.parse({
                proposal: proposal,
                description: proposal.description,
                votes: currentVotes,
                totalVotingPower,
                quorum,
                isApproved: currentVotes.yes >= quorum
            });

            yield status;

            if (status.isApproved) {
                this.#execute(status.proposal);
                
                const memberProposals = await Promise.all(this.#members
                    .filter((member): member is CouncilProxy => 
                        'proxyRef' in member && member.proxyRef !== null
                    )
                    .map(async member => {
                        if (!member.proxyRef) return null;
                        
                        const actions = status.proposal.getActionsForCouncil(member.proxyRef);
                        if (actions) {
                            const memberProposal = member.addProposal(
                                status.proposal.description,
                                new Map([[member, actions]])
                            );
                            return { council: member as CouncilProxy, proposal: memberProposal };
                        }
                        return null;
                    }));

                memberProposals.filter((mp): mp is { council: CouncilProxy; proposal: Proposal } => mp !== null)
                    .forEach(({ council, proposal }) => {
                        if (!this.#pendingResponses.has(council)) {
                            this.#pendingResponses.set(council, new Map());
                        }
                        const councilResponses = this.#pendingResponses.get(council);
                        if (councilResponses) {
                            councilResponses.set(proposal.description, proposal);
                        }
                    });
            }
        }
    }

    getMethods(): string[] {
        const prototypeMethods = Object.getOwnPropertyNames(Council.prototype)
            .filter(prop => prop !== 'constructor')
            .filter(prop => {
                const protoValue = (Council.prototype as any)[prop];
                return typeof protoValue === 'function';
            });

        const instanceMethods = Object.getOwnPropertyNames(this)
            .filter(prop => typeof this[prop] === 'function');

        return [...new Set([...prototypeMethods, ...instanceMethods])];
    }
}

function createCouncil(name: string): CouncilProxy {
    const council = new Council(name);
    
    const publicInterface = new Set<string>([
        'name',
        'members',
        'delegates',
        'proposals',
        'addProposal',
        'castVote',
        'bootstrap',
        'processProposals',
        'getMethods'
    ]);

    const directProperties = new Set<string>([
        'name',
        'members',
        'delegates',
        'proposals',
        'isExecutingProposal'
    ]);

    const proxy = new Proxy(council, {
        get(target: Council, prop: string | symbol, receiver: any): any {
            if (typeof prop === 'symbol') {
                return Reflect.get(target, prop, receiver);
            }

            if (directProperties.has(prop)) {
                return target[prop as keyof Council];
            }

            if (publicInterface.has(prop)) {
                const value = target[prop as keyof Council];
                if (typeof value === 'function') {
                    return function(this: any, ...args: any[]): any {
                        const result = value.apply(target, args);
                        return result === target ? proxy : result;
                    };
                }
                return value;
            }

            if (target.isExecutingProposal) {
                const value = target[prop as keyof Council];
                if (typeof value === 'function') {
                    return function(this: any, ...args: any[]): any {
                        const result = value.apply(target, args);
                        return result === target ? proxy : result;
                    };
                }
                return value;
            }

            console.log(`Attempted to access restricted method/property: ${String(prop)}`);
            return undefined;
        }
    }) as CouncilProxy;

    council.proxyRef = proxy;
    
    return proxy;
}
    
async function main(): Promise<void> {
    const councilA = createCouncil('Council A');
    const councilB = createCouncil('Council B');

    const bootstrapA = councilA.bootstrap();
    const member1 = bootstrapA.addMember('Member 1');
    const member2 = bootstrapA.addMember('Member 2');
    bootstrapA.addMethod('increaseFunding', function(this: CouncilProxy, amount: number) {
        console.log(`${this.name} increasing funding by ${amount}`);
    });

    const bootstrapB = councilB.bootstrap();
    bootstrapB.addMethod('acceptFunding', function(this: CouncilProxy, amount: number) {
        console.log(`${this.name} accepting funding of ${amount}`);
    });

    const mandateDescription = 'Negotiate trade agreement with Council B';
    const mandateActions = new Map<CouncilProxy, Action>();
    mandateActions.set(councilA, {
        description: 'Elect delegate with negotiation powers',
        methodName: 'electDelegate',
        methodArgs: ['Ruzgar', mandateDescription, councilB]
    });

    const mandateProposal = councilA.addProposal(mandateDescription, mandateActions);

    member1.castVote(mandateProposal, 'yes');
    member2.castVote(mandateProposal, 'yes');

    let delegate: Delegate | null = null;
    for await (const status of councilA.processProposals()) {
        console.log('Proposal status:', status);
        if (status.isApproved) {
            // The delegate will be created after this iteration
        }
    }

    const delegates = councilA.delegates;
    console.log('Available delegates after mandate approval:', delegates);
    
    if (delegates.length > 0) {
        delegate = delegates[0];
        console.log('Selected delegate:', delegate);

        const proposalActions = new Map<CouncilProxy, Action>();
        proposalActions.set(councilA, {
            description: 'Increase funding for public works',
            methodName: 'increaseFunding',
            methodArgs: [1000]
        });
        proposalActions.set(councilB, {
            description: 'Accept funding increase',
            methodName: 'acceptFunding',
            methodArgs: [1000]
        });

        delegate.propose('Inter-council funding proposal', proposalActions);
    } else {
        console.log('No delegates available after mandate approval');
    }
}

main().catch(console.error);

async function testVotingScenarios(): Promise<void> {
    console.log('=== Testing Voting Scenarios ===');

    // Test Case 1: Simple Majority
    console.log('\nTest Case 1: Simple Majority');
    const councilA = createCouncil('Council A');
    const bootstrapA = councilA.bootstrap();
    const member1 = bootstrapA.addMember('Member 1');
    const member2 = bootstrapA.addMember('Member 2');
    
    const proposal1 = councilA.addProposal('Simple majority test');
    member1.castVote(proposal1, 'yes');
    member2.castVote(proposal1, 'no');
    
    for await (const status of councilA.processProposals()) {
        console.log('Status:', status);
    }

    // Test Case 2: Unanimous Approval
    console.log('\nTest Case 2: Unanimous Approval');
    const councilB = createCouncil('Council B');
    const bootstrapB = councilB.bootstrap();
    const memberB1 = bootstrapB.addMember('Member B1');
    const memberB2 = bootstrapB.addMember('Member B2');
    const memberB3 = bootstrapB.addMember('Member B3');
    
    const proposal2 = councilB.addProposal('Unanimous test');
    memberB1.castVote(proposal2, 'yes');
    memberB2.castVote(proposal2, 'yes');
    memberB3.castVote(proposal2, 'yes');
    
    for await (const status of councilB.processProposals()) {
        console.log('Status:', status);
    }

    // Test Case 3: Mixed Delegate and Member Voting
    console.log('\nTest Case 3: Mixed Delegate and Member Voting');
    const councilC = createCouncil('Council C');
    const councilD = createCouncil('Council D');

    const bootstrapC = councilC.bootstrap();
    const bootstrapD = councilD.bootstrap();
    bootstrapC.addMethod('testMethod', function(this: CouncilProxy) {
        console.log('Test method called in Council C');
    });
    bootstrapD.addMethod('testMethod', function(this: CouncilProxy) {
        console.log('Test method called in Council D');
    });

    const membershipProposal = councilD.addProposal('Accept Council C as member', new Map<CouncilProxy, Action>([
        [councilD, {
            description: 'Add Council C as member',
            methodName: 'addMember',
            methodArgs: [councilC]
        }]
    ]));

    const memberD1 = bootstrapD.addMember('Member D1');
    memberD1.castVote(membershipProposal, 'yes');

    for await (const status of councilD.processProposals()) {
        console.log('Membership Status:', status);
    }

    // Test Case 4: Below Quorum
    console.log('\nTest Case 4: Below Quorum');
    const councilE = createCouncil('Council E');
    const bootstrapE = councilE.bootstrap();
    const memberE1 = bootstrapE.addMember('Member E1');
    const memberE2 = bootstrapE.addMember('Member E2');
    const memberE3 = bootstrapE.addMember('Member E3');
    
    const proposal4 = councilE.addProposal('Below quorum test');
    memberE1.castVote(proposal4, 'yes');
    
    for await (const status of councilE.processProposals()) {
        console.log('Status:', status);
    }
}

testVotingScenarios().catch(console.error);

export { createCouncil, type CouncilProxy, type Delegate, type Member, type Proposal, type Action, type VoteDecision, type VoteResult, type ProposalStatus, type ResponseStatus };