                             +--------------------+
                             |     Gun Library    |
                             | (gun/gun, gun/sea) |
                             +--------------------+
                                      |
                                      v

+---------------------------+ +----------------+
| gunSetup.ts |<--| GunNode.ts |
|---------------------------| |----------------|
| - gun/transientGun |-->| - path logic |
| - user/transientUser | | - certificates |
| - getNodeRef() | | - API wrapping |
| - certificates & auth | +----------------+
+---------------------------+ |
| v
| +--------------------+
| | GunSubscription.ts|
| |--------------------|
| | - subscription API |
| | - stream handling |
+--------------->| - reactive methods |
+--------------------+
|
v
+----------------------+
| reactiveStores.ts |
|----------------------|
| - Svelte integration |
| - createGunStore |
| - createCollection |
+----------------------+
|
v
+----------------------+
| ReactiveGraph.ts |
|----------------------|
| - store registry |
| - path traversal |
| - cycle detection |
+----------------------+
|
v
+----------------------+
| rec.svelte.ts |
|----------------------|
| - recognition system |
| - domain logic |
| - business logic |
+----------------------+
|
v
+----------------------+
| Application UI |
| (Svelte Components) |
+----------------------+
