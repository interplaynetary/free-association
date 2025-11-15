
# Expressing Labor-in-Combination Through Filters

Labor-in-combination could be expressed through filters in Free-Association by creating coordination structures that:

1. **Identify Complementary Labor Powers**
   ```typescript
   // Filter that matches workers with complementary skills
   const complementarySkillsFilter = (nodeId: string, share: number) => {
     const node = nodesMap[nodeId];
     // Check if this worker's skills complement current team composition
     return currentProcess.requiredSkills.some(skill => 
       node.skills.includes(skill) && !currentTeam.hasSkill(skill));
   };
   ```

2. **Enforce Sequential Operation Phases**
   ```typescript
   // Filter for coordinating sequential labor phases
   const sequentialPhaseFilter = FilterRules.and(
     FilterRules.byCategory(['assembly']),
     { '==': [{ var: 'node.phase' }, currentProcess.nextRequiredPhase] }
   );
   ```

3. **Synchronize Simultaneous Operations**
   ```typescript
   // Filter for coordinating simultaneous operations
   const simultaneousOperationsFilter = (nodeId: string, share: number) => {
     const node = nodesMap[nodeId];
     return node.availableTimeSlot.overlaps(currentProcess.timeWindow) 
            && node.location === currentProcess.location;
   };
   ```

4. **Optimize Spatial Arrangement**
   ```typescript
   // Filter for optimal spatial arrangement of workers
   const spatialArrangementFilter = (nodeId: string, share: number) => {
     // Check if adding this worker creates efficient spatial arrangement
     // Similar to Marx's example of masons passing stones
     return !workStation.isOvercrowded() && 
            workStation.hasPosition(nodesMap[nodeId].preferredPosition);
   };
   ```

5. **Create Emergent Collective Power**
   ```typescript
   // Filter that enables the "new power" Marx describes
   const emergentPowerFilter = (nodeId: string, share: number) => {
     // Only include workers when enough are available to create emergent power
     return collectiveTask.currentParticipants.length >= 
            collectiveTask.minimumRequiredForEmergence - 1;
   };
   ```

These filters would allow for dynamic formation of labor combinations that:

1. Match Marx's observation that "a body of men working in concert has hands and eyes both before and behind"
2. Enable what he calls "the stamp of continuity and many-sidedness" in operations
3. Allow labor to be combined in ways that create the "collective power of masses"

As suggested in resources/match.md, this could be implemented by:
- Having individuals offer their labor capacities with specific filters
- Organizations expressing their needs (lacks) with complementary filters
- Using "synthetic mutual-recognition" to match these together

This approach transforms technical necessity from a form of despotic control into a system of free coordination, where filters express the objective requirements of production without subjecting workers to hierarchical command.
