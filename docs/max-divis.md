Does it make more sense to prune just from the list?
 
Efficient Provider-Centric Distribution Calculation with Max-Divisibility Pruning

Calculate once from provider outward, then lookup individual shares

DistributeShares(Provider, MaxDepth, MaxDivisibility):
  // Initialize share distribution map and remainder collection
  ShareMap = empty map of {Person → Share}
  RemainderMap = empty map of {Depth → Remainder}
  VisitedNodes = empty set
  TotalRemainder = 0
  
  // Start with direct shares at depth 1
  for each Person that Provider recognizes:
    DirectShare = Direct-Share(Person, Provider)
    ShareMap[Person] = DirectShare
  
  // Process each depth level from 2 to MaxDepth
  for depth from 2 to MaxDepth:
    // Create temporary map for this depth's calculations
    NewShares = empty map
    DepthRemainder = 0
    
    // For each person who received shares in previous depths
    for each Recipient in ShareMap who hasn't been fully processed:
      VisitedNodes.add(Recipient)
      RecipientShare = ShareMap[Recipient]
      
      // Distribute Recipient's share to those they recognize
      for each Connection that Recipient recognizes:
        if Connection ∉ VisitedNodes:
          // Connection gets a share proportional to their direct share from Recipient
          // multiplied by Recipient's share from Provider
          ConnectionDirectShare = Direct-Share(Connection, Recipient)
          TransitiveShare = RecipientShare × ConnectionDirectShare
          
          // Apply max-divisibility pruning
          if TransitiveShare < MaxDivisibility:
            // This share is too small, add it to the remainder for this depth
            DepthRemainder += TransitiveShare
            continue
          
          // Add to Connection's existing share (if any)
          if Connection in NewShares:
            NewShares[Connection] += TransitiveShare
          else:
            NewShares[Connection] = TransitiveShare
    
    // Store the remainder for this depth
    RemainderMap[depth] = DepthRemainder
    TotalRemainder += DepthRemainder
    
    // Merge new shares into overall ShareMap
    for each Person in NewShares:
      if Person in ShareMap:
        ShareMap[Person] += NewShares[Person]
      else:
        ShareMap[Person] = NewShares[Person]
  
  return {
    Shares: ShareMap,
    Remainders: RemainderMap,
    TotalRemainder: TotalRemainder
  }

// Individual's total share is then a simple lookup
Total-Share(You, Provider, MaxDepth, MaxDivisibility):
  Results = DistributeShares(Provider, MaxDepth, MaxDivisibility)
  return Results.Shares[You] if You in Results.Shares else 0

// Get distribution efficiency metrics
GetDistributionEfficiency(Provider, MaxDepth, MaxDivisibility):
  Results = DistributeShares(Provider, MaxDepth, MaxDivisibility)
  TotalDistributed = sum(Results.Shares.values())
  TotalRemainder = Results.TotalRemainder
  DistributionEfficiency = TotalDistributed / (TotalDistributed + TotalRemainder)
  return {
    DistributedPercentage: DistributionEfficiency * 100,
    RemainderPercentage: (1 - DistributionEfficiency) * 100,
    RemaindersByDepth: Results.Remainders
  }