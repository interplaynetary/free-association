stores.svelte.ts:982 [📡 NETWORK-SUB] Received commitment from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:998 [📡 NETWORK-SUB] Commitment contains 5 recognition entries (5 non-zero)
stores.svelte.ts:1005 [📡 NETWORK-SUB] IPF fields: constraint_scaling_factors=true (1 entries), total_seed_by_need=true (1 entries)
stores.svelte.ts:1007 [📡 NETWORK-SUB] constraint_scaling_factors: {need_1766968420513_0.23341680744669047: 1}
stores.svelte.ts:1010 [📡 NETWORK-SUB] total_seed_by_need: {need_1766968420513_0.23341680744669047: 0.000001}
stores.svelte.ts:1023 [📡 NETWORK-SUB] Normalized recognition weights for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:317 [VERSIONED-STORE] 🔀 Merged ITC stamps for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:784 [DETECT-FIELD-CHANGES:needs] Comparing values:
v-store.svelte.ts:785   oldValue: []
v-store.svelte.ts:786   newValue: [{…}]
v-store.svelte.ts:787   oldValue JSON: []
v-store.svelte.ts:788   newValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:789   equalityChecker: jsonEquals
v-store.svelte.ts:797   changed: true
stores.svelte.ts:860 [SLOTS-CACHE] Updating uS-ytluRW3AtvnnTvJ6V... (1 needs, 0 capacity)
stores.svelte.ts:866 [SLOTS-CACHE] Caching slots from 1 users for offline allocation
v-store.svelte.ts:476 [DERIVE-FIELD:needs] Processing 1 entities
v-store.svelte.ts:489 [DERIVE-FIELD:needs] Entity uS-ytluRW3AtvnnTvJ6V...: currentVer=5, lastVer=4, inMap=true, mapValue= []
v-store.svelte.ts:501 [DERIVE-FIELD:needs] ✅ Extracted from uS-ytluRW3AtvnnTvJ6V...: [{…}]
v-store.svelte.ts:502 [DERIVE-FIELD:needs] Entity data: {capacity_slots: Array(0), need_slots: Array(1), slot_allocations: Array(0), global_recognition_weights: {…}, others_recognition_of_me: {…}, …}
stores.svelte.ts:698 [🤝 MUTUAL-REC] Computing mutual recognition (local-first)...
stores.svelte.ts:716 [🤝 MUTUAL-REC] My recognition: 1 entries
stores.svelte.ts:717 [🤝 MUTUAL-REC] Cached others' rec: 1 entries
stores.svelte.ts:740 [🤝 MUTUAL-REC]   uS-ytluRW3AtvnnTvJ6V...: I→them=100.00%, them→me=15.37%, MR=15.37% [CACHED]
stores.svelte.ts:745 [🤝 MUTUAL-REC] ✅ Computed 1 mutual relationships (local-first!)
stores.svelte.ts:541 [NETWORK-NEEDS-FLAT] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
stores.svelte.ts:542 [NETWORK-NEEDS-FLAT] networkNeedSlotsMap entries: 1
stores.svelte.ts:543 [NETWORK-NEEDS-FLAT] My pubkey: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:547 [NETWORK-NEEDS-FLAT] Map contents: [{…}]
stores.svelte.ts:557 [NETWORK-NEEDS-FLAT]   uS-ytluRW3AtvnnTvJ6V...: 1 needs (self=false)
stores.svelte.ts:561 [NETWORK-NEEDS-FLAT]     → Added need: need_1766968420... qty=100 (from network)
stores.svelte.ts:565 [NETWORK-NEEDS-FLAT] ✅ Total flattened needs: 1 (including self)
+page.svelte:162 [📊 UI-MR] Mutual recognition changed - generating segments for bar...
+page.svelte:172 [📊 UI-MR] Mutual recognition has 1 entries (1 non-zero)
+page.svelte:183 [📊 UI-MR] ✅ Generated 1 segments for mutual recognition bar:
+page.svelte:185   • uS-ytluRW3AtvnnTvJ6V... → 15.37%
stores.svelte.ts:1999 [AUTO-NEED-SYNC] Filtered 0 slots from 0 sources
stores.svelte.ts:2004 [AUTO-NEED-SYNC] Merged: 0 declared + 0 network = 0 total
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 0
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 0
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 0 capacity slots, 1 needs
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 0 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 0
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 0
v-store.svelte.ts:386 [VERSIONED-STORE] ✅ Updated [recognition, needs, constraint_scaling_factors, total_seed_by_need]: uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:1031 [📡 NETWORK-SUB] ✅ Updated [recognition, needs, constraint_scaling_factors, total_seed_by_need] from uS-ytluRW3AtvnnTvJ6V...
store.svelte.ts:317 [MESH-STORE:allocation/commitment] 💾 SAVING - Data: {need_slots: Array(0), capacity_slots: Array(0), global_recognition_weights: {…}, others_recognition_of_me: {…}, itcStamp: {…}, …}
store.svelte.ts:318 [MESH-STORE:allocation/commitment] 💾 SAVING - Timestamp: 1766968421345
store.svelte.ts:319 [MESH-STORE:allocation/commitment] 💾 SAVING - JSON size: 1298 bytes
stores.svelte.ts:2179 [📝 COMPOSE] Composing commitment from sources...
stores.svelte.ts:2126 [ITC-MERGE] ✅ Merged 1 network ITC stamps into local commitment
stores.svelte.ts:2244 [📝 COMPOSE] ✅ Composed commitment:
stores.svelte.ts:2245   • Recognition: 1 entries (1 non-zero) [includes self if present in tree]
stores.svelte.ts:2246   • Others' rec cache: 1 entries
stores.svelte.ts:2247   • Need Slots: 0
stores.svelte.ts:2248   • Capacity Slots: 0
stores.svelte.ts:2257 [📝 COMPOSE] Recognition weights being published to network:
stores.svelte.ts:2261     • uS-ytluRW3AtvnnTvJ6V... → 100.00%
stores.svelte.ts:2343 [AUTO-COMPOSE] ⏭️  Skipped: commitment data unchanged (network recognition changed)
stores.svelte.ts:2179 [📝 COMPOSE] Composing commitment from sources...
stores.svelte.ts:2126 [ITC-MERGE] ✅ Merged 1 network ITC stamps into local commitment
stores.svelte.ts:2244 [📝 COMPOSE] ✅ Composed commitment:
stores.svelte.ts:2245   • Recognition: 1 entries (1 non-zero) [includes self if present in tree]
stores.svelte.ts:2246   • Others' rec cache: 1 entries
stores.svelte.ts:2247   • Need Slots: 0
stores.svelte.ts:2248   • Capacity Slots: 0
stores.svelte.ts:2257 [📝 COMPOSE] Recognition weights being published to network:
stores.svelte.ts:2261     • uS-ytluRW3AtvnnTvJ6V... → 100.00%
stores.svelte.ts:2343 [AUTO-COMPOSE] ⏭️  Skipped: commitment data unchanged (network recognition changed)
allocation.svelte.ts:269 [AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set
store.svelte.ts:332 [MESH-STORE:allocation/commitment] ✅ Saved successfully
stores.svelte.ts:982 [📡 NETWORK-SUB] Received commitment from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:998 [📡 NETWORK-SUB] Commitment contains 5 recognition entries (5 non-zero)
stores.svelte.ts:1005 [📡 NETWORK-SUB] IPF fields: constraint_scaling_factors=true (1 entries), total_seed_by_need=true (1 entries)
stores.svelte.ts:1007 [📡 NETWORK-SUB] constraint_scaling_factors: {need_1766968420513_0.23341680744669047: 1}
stores.svelte.ts:1010 [📡 NETWORK-SUB] total_seed_by_need: {need_1766968420513_0.23341680744669047: 0.000001}
stores.svelte.ts:1023 [📡 NETWORK-SUB] Normalized recognition weights for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:317 [VERSIONED-STORE] 🔀 Merged ITC stamps for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:784 [DETECT-FIELD-CHANGES:needs] Comparing values:
v-store.svelte.ts:785   oldValue: [{…}]
v-store.svelte.ts:786   newValue: [{…}]
v-store.svelte.ts:787   oldValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:788   newValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:789   equalityChecker: jsonEquals
v-store.svelte.ts:797   changed: false
v-store.svelte.ts:476 [DERIVE-FIELD:needs] Processing 1 entities
v-store.svelte.ts:489 [DERIVE-FIELD:needs] Entity uS-ytluRW3AtvnnTvJ6V...: currentVer=5, lastVer=5, inMap=true, mapValue= [{…}]
v-store.svelte.ts:509 [DERIVE-FIELD:needs] ⏭️  Skipped extraction for uS-ytluRW3AtvnnTvJ6V... (versions match)
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 0
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 0
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 0 capacity slots, 1 needs
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 0 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 0
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 0
v-store.svelte.ts:361 [VERSIONED-STORE] ⏭️  No field changes: uS-ytluRW3AtvnnTvJ6V... (causality updated)
stores.svelte.ts:1033 [📡 NETWORK-SUB] ⏭️  Skipped from uS-ytluRW3AtvnnTvJ6V... (No field changes)
allocation.svelte.ts:269 [AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set
stores.svelte.ts:982 [📡 NETWORK-SUB] Received commitment from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:998 [📡 NETWORK-SUB] Commitment contains 5 recognition entries (5 non-zero)
stores.svelte.ts:1005 [📡 NETWORK-SUB] IPF fields: constraint_scaling_factors=true (1 entries), total_seed_by_need=true (1 entries)
stores.svelte.ts:1007 [📡 NETWORK-SUB] constraint_scaling_factors: {need_1766968420513_0.23341680744669047: 1}
stores.svelte.ts:1010 [📡 NETWORK-SUB] total_seed_by_need: {need_1766968420513_0.23341680744669047: 0.000001}
stores.svelte.ts:1023 [📡 NETWORK-SUB] Normalized recognition weights for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:317 [VERSIONED-STORE] 🔀 Merged ITC stamps for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:784 [DETECT-FIELD-CHANGES:needs] Comparing values:
v-store.svelte.ts:785   oldValue: [{…}]
v-store.svelte.ts:786   newValue: [{…}]
v-store.svelte.ts:787   oldValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:788   newValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:789   equalityChecker: jsonEquals
v-store.svelte.ts:797   changed: false
v-store.svelte.ts:476 [DERIVE-FIELD:needs] Processing 1 entities
v-store.svelte.ts:489 [DERIVE-FIELD:needs] Entity uS-ytluRW3AtvnnTvJ6V...: currentVer=5, lastVer=5, inMap=true, mapValue= [{…}]
v-store.svelte.ts:509 [DERIVE-FIELD:needs] ⏭️  Skipped extraction for uS-ytluRW3AtvnnTvJ6V... (versions match)
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 0
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 0
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 0 capacity slots, 1 needs
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 0 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 0
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 0
v-store.svelte.ts:386 [VERSIONED-STORE] ✅ Updated [recognition]: uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:1031 [📡 NETWORK-SUB] ✅ Updated [recognition] from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:2179 [📝 COMPOSE] Composing commitment from sources...
stores.svelte.ts:2126 [ITC-MERGE] ✅ Merged 1 network ITC stamps into local commitment
stores.svelte.ts:2244 [📝 COMPOSE] ✅ Composed commitment:
stores.svelte.ts:2245   • Recognition: 1 entries (1 non-zero) [includes self if present in tree]
stores.svelte.ts:2246   • Others' rec cache: 1 entries
stores.svelte.ts:2247   • Need Slots: 0
stores.svelte.ts:2248   • Capacity Slots: 0
stores.svelte.ts:2257 [📝 COMPOSE] Recognition weights being published to network:
stores.svelte.ts:2261     • uS-ytluRW3AtvnnTvJ6V... → 100.00%
stores.svelte.ts:2343 [AUTO-COMPOSE] ⏭️  Skipped: commitment data unchanged (network recognition changed)
stores.svelte.ts:2179 [📝 COMPOSE] Composing commitment from sources...
stores.svelte.ts:2126 [ITC-MERGE] ✅ Merged 1 network ITC stamps into local commitment
stores.svelte.ts:2244 [📝 COMPOSE] ✅ Composed commitment:
stores.svelte.ts:2245   • Recognition: 1 entries (1 non-zero) [includes self if present in tree]
stores.svelte.ts:2246   • Others' rec cache: 1 entries
stores.svelte.ts:2247   • Need Slots: 0
stores.svelte.ts:2248   • Capacity Slots: 0
stores.svelte.ts:2257 [📝 COMPOSE] Recognition weights being published to network:
stores.svelte.ts:2261     • uS-ytluRW3AtvnnTvJ6V... → 100.00%
stores.svelte.ts:2343 [AUTO-COMPOSE] ⏭️  Skipped: commitment data unchanged (network recognition changed)
allocation.svelte.ts:269 [AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set
stores.svelte.ts:982 [📡 NETWORK-SUB] Received commitment from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:998 [📡 NETWORK-SUB] Commitment contains 5 recognition entries (5 non-zero)
stores.svelte.ts:1005 [📡 NETWORK-SUB] IPF fields: constraint_scaling_factors=true (1 entries), total_seed_by_need=true (1 entries)
stores.svelte.ts:1007 [📡 NETWORK-SUB] constraint_scaling_factors: {need_1766968420513_0.23341680744669047: 1}
stores.svelte.ts:1010 [📡 NETWORK-SUB] total_seed_by_need: {need_1766968420513_0.23341680744669047: 0.7392337801804647}
stores.svelte.ts:1023 [📡 NETWORK-SUB] Normalized recognition weights for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:317 [VERSIONED-STORE] 🔀 Merged ITC stamps for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:784 [DETECT-FIELD-CHANGES:needs] Comparing values:
v-store.svelte.ts:785   oldValue: [{…}]
v-store.svelte.ts:786   newValue: [{…}]
v-store.svelte.ts:787   oldValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:788   newValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:789   equalityChecker: jsonEquals
v-store.svelte.ts:797   changed: false
stores.svelte.ts:860 [SLOTS-CACHE] Updating uS-ytluRW3AtvnnTvJ6V... (1 needs, 1 capacity)
stores.svelte.ts:866 [SLOTS-CACHE] Caching slots from 1 users for offline allocation
v-store.svelte.ts:476 [DERIVE-FIELD:needs] Processing 1 entities
v-store.svelte.ts:489 [DERIVE-FIELD:needs] Entity uS-ytluRW3AtvnnTvJ6V...: currentVer=5, lastVer=5, inMap=true, mapValue= [{…}]
v-store.svelte.ts:509 [DERIVE-FIELD:needs] ⏭️  Skipped extraction for uS-ytluRW3AtvnnTvJ6V... (versions match)
stores.svelte.ts:698 [🤝 MUTUAL-REC] Computing mutual recognition (local-first)...
stores.svelte.ts:716 [🤝 MUTUAL-REC] My recognition: 1 entries
stores.svelte.ts:717 [🤝 MUTUAL-REC] Cached others' rec: 1 entries
stores.svelte.ts:740 [🤝 MUTUAL-REC]   uS-ytluRW3AtvnnTvJ6V...: I→them=100.00%, them→me=15.37%, MR=15.37% [CACHED]
stores.svelte.ts:745 [🤝 MUTUAL-REC] ✅ Computed 1 mutual relationships (local-first!)
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
stores.svelte.ts:1865 [AUTO-CAPACITY-SYNC] Filtered 0 slots from 0 sources
stores.svelte.ts:1870 [AUTO-CAPACITY-SYNC] Merged: 0 declared + 0 network = 0 total
+page.svelte:162 [📊 UI-MR] Mutual recognition changed - generating segments for bar...
+page.svelte:172 [📊 UI-MR] Mutual recognition has 1 entries (1 non-zero)
+page.svelte:183 [📊 UI-MR] ✅ Generated 1 segments for mutual recognition bar:
+page.svelte:185   • uS-ytluRW3AtvnnTvJ6V... → 15.37%
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 0
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 0
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 0 capacity slots, 1 needs
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 0 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 0
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 0
v-store.svelte.ts:386 [VERSIONED-STORE] ✅ Updated [capacity, allocations, total_seed_by_need]: uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:1031 [📡 NETWORK-SUB] ✅ Updated [capacity, allocations, total_seed_by_need] from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:982 [📡 NETWORK-SUB] Received commitment from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:998 [📡 NETWORK-SUB] Commitment contains 5 recognition entries (5 non-zero)
stores.svelte.ts:1005 [📡 NETWORK-SUB] IPF fields: constraint_scaling_factors=true (1 entries), total_seed_by_need=true (1 entries)
stores.svelte.ts:1007 [📡 NETWORK-SUB] constraint_scaling_factors: {need_1766968420513_0.23341680744669047: 1}
stores.svelte.ts:1010 [📡 NETWORK-SUB] total_seed_by_need: {need_1766968420513_0.23341680744669047: 0.7392337801804647}
stores.svelte.ts:1023 [📡 NETWORK-SUB] Normalized recognition weights for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:317 [VERSIONED-STORE] 🔀 Merged ITC stamps for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:784 [DETECT-FIELD-CHANGES:needs] Comparing values:
v-store.svelte.ts:785   oldValue: [{…}]
v-store.svelte.ts:786   newValue: [{…}]
v-store.svelte.ts:787   oldValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:788   newValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:789   equalityChecker: jsonEquals
v-store.svelte.ts:797   changed: false
v-store.svelte.ts:476 [DERIVE-FIELD:needs] Processing 1 entities
v-store.svelte.ts:489 [DERIVE-FIELD:needs] Entity uS-ytluRW3AtvnnTvJ6V...: currentVer=5, lastVer=5, inMap=true, mapValue= [{…}]
v-store.svelte.ts:509 [DERIVE-FIELD:needs] ⏭️  Skipped extraction for uS-ytluRW3AtvnnTvJ6V... (versions match)
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 0
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 0 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 0
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 0 capacity slots, 1 needs
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 0 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 0
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 0
v-store.svelte.ts:361 [VERSIONED-STORE] ⏭️  No field changes: uS-ytluRW3AtvnnTvJ6V... (causality updated)
stores.svelte.ts:1033 [📡 NETWORK-SUB] ⏭️  Skipped from uS-ytluRW3AtvnnTvJ6V... (No field changes)
store.svelte.ts:317 [MESH-STORE:allocation/commitment] 💾 SAVING - Data: {need_slots: Array(0), capacity_slots: Array(0), global_recognition_weights: {…}, others_recognition_of_me: {…}, itcStamp: {…}, …}
store.svelte.ts:318 [MESH-STORE:allocation/commitment] 💾 SAVING - Timestamp: 1766968425852
store.svelte.ts:319 [MESH-STORE:allocation/commitment] 💾 SAVING - JSON size: 1487 bytes
allocation.svelte.ts:269 [AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set
store.svelte.ts:332 [MESH-STORE:allocation/commitment] ✅ Saved successfully
stores.svelte.ts:2126 [ITC-MERGE] ✅ Merged 1 network ITC stamps into local commitment
store.svelte.ts:481 [MESH-STORE:allocation/commitment] 🔄 SET called
stores.svelte.ts:698 [🤝 MUTUAL-REC] Computing mutual recognition (local-first)...
stores.svelte.ts:716 [🤝 MUTUAL-REC] My recognition: 1 entries
stores.svelte.ts:717 [🤝 MUTUAL-REC] Cached others' rec: 1 entries
stores.svelte.ts:740 [🤝 MUTUAL-REC]   uS-ytluRW3AtvnnTvJ6V...: I→them=100.00%, them→me=15.37%, MR=15.37% [CACHED]
 [🤝 MUTUAL-REC] ✅ Computed 1 mutual relationships (local-first!)
 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
 [📤 ALLOC-GEN] Generating allocations as provider
 [📤 ALLOC-GEN] My capacity slots: 1
 [📤 ALLOC-GEN] My need slots: 0
 [📤 ALLOC-GEN] Network need slots: 1
 [📤 ALLOC-GEN] Total needs for allocation: 1
 [📤 ALLOC-GEN] Network need slots (including self): 1
 [GET-ALL-COMMITMENTS] Network commitments: 1
 [GET-ALL-COMMITMENTS] My commitment: yes
 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
 [GET-ALL-COMMITMENTS] Returning 2 total commitments
 [📤 ALLOC-GEN] All commitments: 2
 [📤 ALLOC-GEN] Row scalings (x_p): 0
 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
 [GENERATE-PROPOSALS] Starting with 1 capacity slots, 1 needs
 [GENERATE-PROPOSALS] Generated 0 proposals
 [📤 ALLOC-GEN] Generated proposals: 0
 [📤 ALLOC-GEN] Final allocations: 0
 [GET-ALL-COMMITMENTS] Network commitments: 1
 [GET-ALL-COMMITMENTS] My commitment: yes
 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
 [GET-ALL-COMMITMENTS] Returning 2 total commitments
 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
 [📊 UI-MR] Mutual recognition changed - generating segments for bar...
 [📊 UI-MR] Mutual recognition has 1 entries (1 non-zero)
 [📊 UI-MR] ✅ Generated 1 segments for mutual recognition bar:
   • uS-ytluRW3AtvnnTvJ6V... → 15.37%
 [IPF-Sync] Publishing new allocations: 0
 [🤝 MUTUAL-REC] Computing mutual recognition (local-first)...
 [🤝 MUTUAL-REC] My recognition: 1 entries
 [🤝 MUTUAL-REC] Cached others' rec: 1 entries
 [🤝 MUTUAL-REC]   uS-ytluRW3AtvnnTvJ6V...: I→them=100.00%, them→me=15.37%, MR=15.37% [CACHED]
 [🤝 MUTUAL-REC] ✅ Computed 1 mutual relationships (local-first!)
 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
 [📤 ALLOC-GEN] Generating allocations as provider
 [📤 ALLOC-GEN] My capacity slots: 1
 [📤 ALLOC-GEN] My need slots: 0
 [📤 ALLOC-GEN] Network need slots: 1
 [📤 ALLOC-GEN] Total needs for allocation: 1
 [📤 ALLOC-GEN] Network need slots (including self): 1
 [GET-ALL-COMMITMENTS] Network commitments: 1
 [GET-ALL-COMMITMENTS] My commitment: yes
 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
 [GET-ALL-COMMITMENTS] Returning 2 total commitments
 [📤 ALLOC-GEN] All commitments: 2
 [📤 ALLOC-GEN] Row scalings (x_p): 1
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 1 capacity slots, 1 needs
allocation-ipf-distributed.ts:161 [GENERATE-PROPOSALS] Processing capacity slot capacity_1... (x_p=255.0413)
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
allocation-ipf-distributed.ts:180 [GENERATE-PROPOSALS]   Need need_17669... - k_pr=0.3921, y_r=1.0000, raw=100.00, final=100.00
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 1 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 1
allocation.svelte.ts:160 [📤 ALLOC-GEN] Sample proposal: {capacity: 'capacity_1', need: 'need_17669', recipient: 'uS-ytluRW3', qty: 100}
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
+page.svelte:162 [📊 UI-MR] Mutual recognition changed - generating segments for bar...
+page.svelte:172 [📊 UI-MR] Mutual recognition has 1 entries (1 non-zero)
+page.svelte:183 [📊 UI-MR] ✅ Generated 1 segments for mutual recognition bar:
+page.svelte:185   • uS-ytluRW3AtvnnTvJ6V... → 15.37%
allocation.svelte.ts:91 [IPF-Sync] Publishing new allocations: 1
stores.svelte.ts:698 [🤝 MUTUAL-REC] Computing mutual recognition (local-first)...
stores.svelte.ts:716 [🤝 MUTUAL-REC] My recognition: 1 entries
stores.svelte.ts:717 [🤝 MUTUAL-REC] Cached others' rec: 1 entries
stores.svelte.ts:740 [🤝 MUTUAL-REC]   uS-ytluRW3AtvnnTvJ6V...: I→them=100.00%, them→me=15.37%, MR=15.37% [CACHED]
stores.svelte.ts:745 [🤝 MUTUAL-REC] ✅ Computed 1 mutual relationships (local-first!)
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 1
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 1
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 1 capacity slots, 1 needs
allocation-ipf-distributed.ts:161 [GENERATE-PROPOSALS] Processing capacity slot capacity_1... (x_p=255.0413)
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
allocation-ipf-distributed.ts:180 [GENERATE-PROPOSALS]   Need need_17669... - k_pr=0.3921, y_r=1.0000, raw=100.00, final=100.00
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 1 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 1
allocation.svelte.ts:160 [📤 ALLOC-GEN] Sample proposal: {capacity: 'capacity_1', need: 'need_17669', recipient: 'uS-ytluRW3', qty: 100}
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
+page.svelte:162 [📊 UI-MR] Mutual recognition changed - generating segments for bar...
+page.svelte:172 [📊 UI-MR] Mutual recognition has 1 entries (1 non-zero)
+page.svelte:183 [📊 UI-MR] ✅ Generated 1 segments for mutual recognition bar:
+page.svelte:185   • uS-ytluRW3AtvnnTvJ6V... → 15.37%
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 1
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 1
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":1}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 1 capacity slots, 1 needs
allocation-ipf-distributed.ts:161 [GENERATE-PROPOSALS] Processing capacity slot capacity_1... (x_p=255.0413)
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
allocation-ipf-distributed.ts:180 [GENERATE-PROPOSALS]   Need need_17669... - k_pr=0.3921, y_r=1.0000, raw=100.00, final=100.00
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 1 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 1
allocation.svelte.ts:160 [📤 ALLOC-GEN] Sample proposal: {capacity: 'capacity_1', need: 'need_17669', recipient: 'uS-ytluRW3', qty: 100}
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 1
stores.svelte.ts:935 [SET-CAPACITY-SLOTS] Updated: 1 slots
allocation.svelte.ts:280 [AUTO-PUBLISH-ALLOC] ⏭️  Skipped: allocations unchanged in commitment
store.svelte.ts:317 [MESH-STORE:allocation/commitment] 💾 SAVING - Data: {need_slots: Array(0), capacity_slots: Array(1), global_recognition_weights: {…}, others_recognition_of_me: {…}, itcStamp: {…}, …}
store.svelte.ts:318 [MESH-STORE:allocation/commitment] 💾 SAVING - Timestamp: 1766968428705
store.svelte.ts:319 [MESH-STORE:allocation/commitment] 💾 SAVING - JSON size: 1581 bytes
store.svelte.ts:332 [MESH-STORE:allocation/commitment] ✅ Saved successfully
stores.svelte.ts:982 [📡 NETWORK-SUB] Received commitment from uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:998 [📡 NETWORK-SUB] Commitment contains 5 recognition entries (5 non-zero)
stores.svelte.ts:1005 [📡 NETWORK-SUB] IPF fields: constraint_scaling_factors=true (1 entries), total_seed_by_need=true (1 entries)
stores.svelte.ts:1007 [📡 NETWORK-SUB] constraint_scaling_factors: {need_1766968420513_0.23341680744669047: 0.5}
stores.svelte.ts:1010 [📡 NETWORK-SUB] total_seed_by_need: {need_1766968420513_0.23341680744669047: 1.1313270778724847}
stores.svelte.ts:1023 [📡 NETWORK-SUB] Normalized recognition weights for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:317 [VERSIONED-STORE] 🔀 Merged ITC stamps for uS-ytluRW3AtvnnTvJ6V...
v-store.svelte.ts:784 [DETECT-FIELD-CHANGES:needs] Comparing values:
v-store.svelte.ts:785   oldValue: [{…}]
v-store.svelte.ts:786   newValue: [{…}]
v-store.svelte.ts:787   oldValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:788   newValue JSON: [{"id":"need_1766968420513_0.23341680744669047","quantity":100,"type_id":"money","max_natural_div":1,"min_allocation_percentage":0.01,"name":"Rent","unit":"USD","recurrence":"monthly"}]
v-store.svelte.ts:789   equalityChecker: jsonEquals
v-store.svelte.ts:797   changed: false
stores.svelte.ts:860 [SLOTS-CACHE] Updating uS-ytluRW3AtvnnTvJ6V... (1 needs, 1 capacity)
stores.svelte.ts:866 [SLOTS-CACHE] Caching slots from 1 users for offline allocation
v-store.svelte.ts:476 [DERIVE-FIELD:needs] Processing 1 entities
v-store.svelte.ts:489 [DERIVE-FIELD:needs] Entity uS-ytluRW3AtvnnTvJ6V...: currentVer=5, lastVer=5, inMap=true, mapValue= [{…}]
v-store.svelte.ts:509 [DERIVE-FIELD:needs] ⏭️  Skipped extraction for uS-ytluRW3AtvnnTvJ6V... (versions match)
stores.svelte.ts:698 [🤝 MUTUAL-REC] Computing mutual recognition (local-first)...
stores.svelte.ts:716 [🤝 MUTUAL-REC] My recognition: 1 entries
stores.svelte.ts:717 [🤝 MUTUAL-REC] Cached others' rec: 1 entries
stores.svelte.ts:740 [🤝 MUTUAL-REC]   uS-ytluRW3AtvnnTvJ6V...: I→them=100.00%, them→me=15.37%, MR=15.37% [CACHED]
stores.svelte.ts:745 [🤝 MUTUAL-REC] ✅ Computed 1 mutual relationships (local-first!)
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
+page.svelte:162 [📊 UI-MR] Mutual recognition changed - generating segments for bar...
+page.svelte:172 [📊 UI-MR] Mutual recognition has 1 entries (1 non-zero)
+page.svelte:183 [📊 UI-MR] ✅ Generated 1 segments for mutual recognition bar:
+page.svelte:185   • uS-ytluRW3AtvnnTvJ6V... → 15.37%
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
allocation.svelte.ts:124 [📤 ALLOC-GEN] ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
allocation.svelte.ts:125 [📤 ALLOC-GEN] Generating allocations as provider
allocation.svelte.ts:126 [📤 ALLOC-GEN] My capacity slots: 1
allocation.svelte.ts:127 [📤 ALLOC-GEN] My need slots: 0
allocation.svelte.ts:128 [📤 ALLOC-GEN] Network need slots: 1
allocation.svelte.ts:134 [📤 ALLOC-GEN] Total needs for allocation: 1
allocation.svelte.ts:135 [📤 ALLOC-GEN] Network need slots (including self): 1
stores.svelte.ts:1173 [GET-ALL-COMMITMENTS] Network commitments: 1
stores.svelte.ts:1174 [GET-ALL-COMMITMENTS] My commitment: yes
stores.svelte.ts:1175 [GET-ALL-COMMITMENTS] My pub: AJC9LspVYqaD5_8K6gG8...
stores.svelte.ts:1178 [GET-ALL-COMMITMENTS] ✅ Including my commitment with 0 needs, 1 capacity
stores.svelte.ts:1186 [GET-ALL-COMMITMENTS] Returning 2 total commitments
allocation.svelte.ts:138 [📤 ALLOC-GEN] All commitments: 2
allocation.svelte.ts:139 [📤 ALLOC-GEN] Row scalings (x_p): 1
allocation.svelte.ts:140 [📤 ALLOC-GEN] Cached remote scalings (y_r): 1
allocation.svelte.ts:142 [📤 ALLOC-GEN] y_r values: {"need_1766968420513_0.23341680744669047":0.5}
allocation-ipf-distributed.ts:152 [GENERATE-PROPOSALS] Starting with 1 capacity slots, 1 needs
allocation-ipf-distributed.ts:161 [GENERATE-PROPOSALS] Processing capacity slot capacity_1... (x_p=510.0827)
ipf-core.ts:69 [SEED-CALC] Provider AJC9LspVYq... → Recipient uS-ytluRW3...
ipf-core.ts:70 [SEED-CALC]   providerPriority=1.0000 (provider's recognition of recipient)
ipf-core.ts:71 [SEED-CALC]   recipientPriority=0.1537 (recipient's recognition of provider)
ipf-core.ts:72 [SEED-CALC]   Provider's global_recognition_weights: {uS-ytluRW3AtvnnTvJ6VZ7sDwaTunNMzePUFXBIXbUo.-RW0Pa4Mdp3IVx0aPuXxo7DKqZ7h38UoZYINHWhonMs: 1}
ipf-core.ts:83 [SEED-CALC]   providerTerm=1.000000, recipientTerm=0.392093, k_pr=0.3921
allocation-ipf-distributed.ts:180 [GENERATE-PROPOSALS]   Need need_17669... - k_pr=0.3921, y_r=0.5000, raw=100.00, final=100.00
allocation-ipf-distributed.ts:196 [GENERATE-PROPOSALS] Generated 1 proposals
allocation.svelte.ts:158 [📤 ALLOC-GEN] Generated proposals: 1
allocation.svelte.ts:160 [📤 ALLOC-GEN] Sample proposal: {capacity: 'capacity_1', need: 'need_17669', recipient: 'uS-ytluRW3', qty: 100}
allocation.svelte.ts:198 [📤 ALLOC-GEN] Final allocations: 1
v-store.svelte.ts:386 [VERSIONED-STORE] ✅ Updated [constraint_scaling_factors, total_seed_by_need]: uS-ytluRW3AtvnnTvJ6V...
stores.svelte.ts:1031 [📡 NETWORK-SUB] ✅ Updated [constraint_scaling_factors, total_seed_by_need] from uS-ytluRW3AtvnnTvJ6V...
store.svelte.ts:317 [MESH-STORE:allocation/commitment] 💾 SAVING - Data: {need_slots: Array(0), capacity_slots: Array(1), global_recognition_weights: {…}, others_recognition_of_me: {…}, itcStamp: {…}, …}
store.svelte.ts:318 [MESH-STORE:allocation/commitment] 💾 SAVING - Timestamp: 1766968430450
store.svelte.ts:319 [MESH-STORE:allocation/commitment] 💾 SAVING - JSON size: 2188 bytes
allocation.svelte.ts:269 [AUTO-PUBLISH-ALLOC] ⏭️  Skipped: already published this exact allocation set
store.svelte.ts:332 [MESH-STORE:allocation/commitment] ✅ Saved successfully
