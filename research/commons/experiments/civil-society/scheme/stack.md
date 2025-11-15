Guile Scheme
Guix

Deploy on Server
Expose Rest-API

Next Steps:
Contact Christina for going Goblins

With the state as a lisp, it could make rendering it as HTML even easier?

Persistance -> upload / download becomes ways easier

Calculations:
- sharesOfGeneralFulfillment
- 

Player
- Has Global Unique ID
- Has RootNode
- Has Caches

Player Caches:
- sharesOfGeneralFulfillment :: Map (Player => Proportion[0-1])
- MutualFulfillment :: Map (Player => [0-1])
- ProviderShares :: Map (Depth => Map(Player => Proportion[0-1]))

All Caches are (normalized) upon read. Even if we want a value from one-entry, we will normalize and read the entry from the normalized map.
This is important because if someone lies about sharesOfGeneralFulfillment (the proportions dont add up to 1), we noramlize the lie.

MutualFulfillment, ProviderShares, and all other calculations beyond sharesOfGeneralFulfillment 
should operate only on the cached data. 
This means mutualFulfillment should operate only on the last cached data of the sharesOfGeneralFulfillment of each entity, only trying to calculate them if there is no cached value, similarly providerShares (at any depth beyond 1) should only use the cached value, only calculating depth 1 (and caching it) if there is no cached value


(define-syntax-rule (with-verifiable-recognition node other-node value)
  `(recognition 
     (from ,(node-id node))
     (to ,(node-id other-node)) 
     (value ,value)
     (timestamp ,(current-time))
     (signature ,(sign-data 
                   (string-append (node-id node) (node-id other-node) 
                                  (number->string value))
                   (private-key node)))))