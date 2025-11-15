;; protocol/sexp.scm - S-expression based implementation of the free-association protocol
(use-modules (srfi srfi-1)   ; List library
             (ice-9 match)   ; Pattern matching
             (ice-9 receive)) ; Multiple values

;;----------------------
;; Core Data Structure
;;----------------------

;; In this implementation, we represent nodes as S-expressions rather than record types
;; A node is represented as: 
;; (node (id "...") (name "...") (points N) (children (...)) (contributors (...)) ...)

;; Cache values are represented as tagged lists:
;; (float 3.14)
;; (int 42)
;; (string-list ("a" "b" "c"))
;; (share-map ((key1 . value1) (key2 . value2)))

;; Create a new node
(define (make-node id name points contributors manual-fulfillment)
  `(node 
     (id ,id)
     (name ,name)
     (points ,points)
     (children ())
     (contributors ,contributors)
     (manual-fulfillment ,(and manual-fulfillment (max 0 (min 1 manual-fulfillment))))
     (capacities ())
     (capacity-shares ())
     (cache ())))

;; Helper functions for accessing node fields
(define (node-ref node field)
  (match node
    (('node . fields)
     (let ((field-pair (assq field fields)))
       (and field-pair (cadr field-pair))))
    (_ (error "Invalid node:" node))))

(define (node-set! node field value)
  (match node
    (('node . fields)
     (let ((new-fields (map (lambda (f)
                              (if (eq? (car f) field)
                                  (list field value)
                                  f))
                            fields)))
       `(node ,@new-fields)))
    (_ (error "Invalid node:" node))))

;; Specific node accessors
(define (node-id node) (node-ref node 'id))
(define (node-name node) (node-ref node 'name))
(define (node-points node) (node-ref node 'points))
(define (node-children node) (node-ref node 'children))
(define (node-contributors node) (node-ref node 'contributors))
(define (node-manual-fulfillment node) (node-ref node 'manual-fulfillment))
(define (node-capacities node) (node-ref node 'capacities))
(define (node-capacity-shares node) (node-ref node 'capacity-shares))
(define (node-cache node) (node-ref node 'cache))

;; Zipper structure is [current-node context]
;; where context is either #f for root or [parent siblings ancestors]

;; Create a zipper at the root node
(define (make-zipper node)
  (vector node #f))

;; Accessor functions for zipper
(define (zipper-current zipper) (vector-ref zipper 0))
(define (zipper-context zipper) (vector-ref zipper 1))

;;-------------------------
;; Zipper Navigation
;;-------------------------

;; Enter a child node
(define (enter-child child-id zipper)
  (let* ((current (zipper-current zipper))
         (children (node-children current))
         (child (assoc child-id children string=?)))
    (if child
        (let* ((child-node (cadr child))
               (siblings (filter (lambda (c) (not (string=? (car c) child-id))) 
                                 children))
               (context (zipper-context zipper))
               (ancestors (if context (vector-ref context 2) '()))
               (new-context (vector current siblings ancestors)))
          (vector child-node new-context))
        #f)))

;; Exit to parent
(define (exit-to-parent zipper)
  (let ((context (zipper-context zipper)))
    (if (not context)
        #f  ; Can't go up from root
        (let* ((current (zipper-current zipper))
               (parent (vector-ref context 0))
               (siblings (vector-ref context 1))
               (ancestors (vector-ref context 2))
               (current-id (node-id current))
               ;; Add current node back to parent's children
               (new-children (cons (list current-id current) siblings))
               (new-parent (node-set! parent 'children new-children)))
          (vector new-parent (if (null? ancestors) #f (car ancestors)))))))

;; Go to root
(define (go-to-root zipper)
  (let ((parent (exit-to-parent zipper)))
    (if parent
        (go-to-root parent)
        zipper)))

;; Modify the current node
(define (modify-node f zipper)
  (vector (f (zipper-current zipper))
          (zipper-context zipper)))

;; Get child zippers
(define (children zipper)
  (map (lambda (child-pair)
         (enter-child (car child-pair) zipper))
       (node-children (zipper-current zipper))))

;; Fold over children
(define (fold-children f init zipper)
  (fold f init (children zipper)))

;; Map over children
(define (map-children f zipper)
  (map f (children zipper)))

;; Check if any child satisfies a predicate
(define (any-child p zipper)
  (any p (children zipper)))

;; Check if all children satisfy a predicate
(define (all-children p zipper)
  (every p (children zipper)))

;;----------------------
;; Caching System
;;----------------------

;; Cache keys
(define (weight-cache-key node-id)
  (string-append node-id "_weight"))

(define (fulfillment-cache-key node-id)
  (string-append node-id "_fulfillment"))

(define (mutual-cache-key node-id1 node-id2)
  (if (string<? node-id1 node-id2)
      (string-append node-id1 "_mutual_" node-id2)
      (string-append node-id2 "_mutual_" node-id1)))

(define (descendants-cache-key node-id)
  (string-append node-id "_descendants"))

(define (total-points-cache-key node-id)
  (string-append node-id "_total_points"))

(define (shares-cache-key node-id)
  (string-append node-id "_shares_of_fulfillment"))

;; Provider shares cache key
(define (provider-shares-cache-key node-id depth)
  (string-append node-id "_provider_shares_" (number->string depth)))

;; Cache lookup in association list
(define (cache-lookup key cache)
  (let ((entry (assoc key cache string=?)))
    (if entry
        (values (cdr entry) 
                (cons (cons 'hits (+ 1 (or (assq-ref cache 'hits) 0))) 
                      (alist-delete 'hits cache)))
        (values #f cache))))

;; Cache insert
(define (cache-insert key value cache)
  (cons (cons key value)
        (cons (cons 'misses (+ 1 (or (assq-ref cache 'misses) 0)))
              (alist-delete key (alist-delete 'misses cache)))))

;; Cache operation with computation
(define (with-cache key compute arg cache)
  (receive (value updated-cache) (cache-lookup key cache)
    (if value
        (values value updated-cache)
        (let ((result (compute arg)))
          (values result (cache-insert key result cache))))))

;; Simplified helper to perform node operations with cache
(define (with-node-cache computation zipper)
  (let* ((current (zipper-current zipper))
         (cache (node-cache current))
         (result (computation zipper cache))
         (updated-node (node-set! current 'cache (cdr result))))
    (car result)))

;; Invalidate cache on a node
(define (invalidate-cache zipper)
  (modify-node
   (lambda (node)
     (node-set! node 'cache '()))
   zipper))

;;----------------------------
;; Tree Modification API
;;----------------------------

;; Add a child to a node
(define (add-child name pts contribs manual zipper)
  (let* ((current (zipper-current zipper))
         (new-child (make-node name name pts contribs manual))
         (current-children (node-children current))
         ;; Explicitly invalidate cache when modifying tree structure
         (updated-node (node-set! current 'children 
                                 (cons (list name new-child) current-children)))
         (updated-node-with-empty-cache (node-set! updated-node 'cache '())))
    (vector updated-node-with-empty-cache (zipper-context zipper))))

;;-------------------------
;; Core Calculations
;;-------------------------

;; Calculate total points from all children
(define (total-child-points zipper)
  (with-node-cache total-child-points-m zipper))

(define (total-child-points-m zipper cache)
  (let* ((current (zipper-current zipper))
         (cache-key (total-points-cache-key (node-id current)))
         (compute-total (lambda (_)
                          (apply + (map (lambda (child) 
                                          (node-points (cadr child)))
                                        (node-children current))))))
    (with-cache cache-key compute-total '() cache)))

;; Calculate a node's weight with caching
(define (weight zipper)
  (with-node-cache weight-m zipper))

(define (weight-m zipper cache)
  (let* ((current (zipper-current zipper))
         (cache-key (weight-cache-key (node-id current))))
    (if (not (zipper-context zipper))
        (values 1.0 cache) ; Root node has weight 1.0
        (with-cache cache-key
                   (lambda (_) (compute-weight current zipper))
                   '()
                   cache))))

(define (compute-weight node zipper)
  (let* ((parent-zipper (exit-to-parent zipper)))
    (if (not parent-zipper)
        1.0  ; Should never happen but just in case
        (let* ((total (total-child-points parent-zipper))
               (current-points (node-points node))
               (parent-weight (weight parent-zipper)))
          (if (= total 0)
              0
              (* (/ current-points total) parent-weight))))))

;; Calculate share of parent
(define (share-of-parent zipper)
  (let ((parent (exit-to-parent zipper)))
    (if (not parent)
        1.0 ; Root node has 100% share
        (let* ((total (total-child-points parent))
               (current-points (node-points (zipper-current zipper))))
          (if (= total 0)
              0
              (/ current-points total))))))

;; Predicates for node types
(define (is-contribution? zipper)
  (and (not (null? (node-contributors (zipper-current zipper))))
       (zipper-context zipper)))

;; Check for direct contribution children
(define (has-direct-contribution-child? zipper)
  (any-child is-contribution? zipper))

;; Check for non-contribution children
(define (has-non-contribution-child? zipper)
  (not (all-children is-contribution? zipper)))

;; Contribution children weight
(define (contribution-children-weight zipper)
  (let ((result (fold-children accum-weight '(0 0) zipper)))
    (if (= (cadr result) 0)
        0
        (/ (car result) (cadr result)))))

(define (accum-weight acc child)
  (let ((w (weight child))
        (cw (car acc))
        (tw (cadr acc)))
    (if (is-contribution? child)
        (list (+ cw w) (+ tw w))
        (list cw (+ tw w)))))

;; Children fulfillment based on predicate
(define (children-fulfillment pred zipper)
  (apply + 
         (map (lambda (child)
                (* (fulfilled child) (share-of-parent child)))
              (filter pred (children zipper)))))

;; Contribution children fulfillment
(define (contribution-children-fulfillment zipper)
  (children-fulfillment is-contribution? zipper))

;; Non-contribution children fulfillment
(define (non-contribution-children-fulfillment zipper)
  (let* ((non-contrib-children (filter (lambda (c) (not (is-contribution? c))) 
                                       (children zipper)))
         (weights (map weight non-contrib-children))
         (fulfillments (map fulfilled non-contrib-children))
         (weighted-fulfillments (map * weights fulfillments))
         (total-weight (apply + weights)))
    (if (= total-weight 0)
        0
        (/ (apply + weighted-fulfillments) total-weight))))

;; Get all descendants (including self)
(define (get-all-descendants zipper)
  (cons zipper
        (append-map get-all-descendants (children zipper))))

;; Get all descendants with caching
(define (get-all-descendants-cached zipper)
  (cons zipper (with-node-cache get-all-descendants-cached-m zipper)))

(define (get-all-descendants-cached-m zipper cache)
  (let* ((current (zipper-current zipper))
         (current-id (node-id current))
         (cache-key (descendants-cache-key current-id)))
    (receive (val updated-cache) (cache-lookup cache-key cache)
      (if val
          ;; Fixed caching: we already have the descendant ids but need to re-build zippers
          (let* ((descendant-ids (if (pair? val) 
                                     (and (eq? (car val) 'string-list) (cadr val))
                                     '()))
                 (all-descendants (get-all-descendants zipper)))
            (values all-descendants updated-cache))
          ;; Cache miss - compute the descendants
          (let* ((all-descendants (get-all-descendants zipper))
                 ;; Store only the IDs in the cache, not the zippers
                 (descendant-ids (map (lambda (z) (node-id (zipper-current z))) 
                                    all-descendants)))
            (values all-descendants
                    (cache-insert cache-key 
                                 `(string-list ,descendant-ids)
                                 cache)))))))

;; Get descendants (excluding self)
(define (descendants zipper)
  (cdr (get-all-descendants-cached zipper)))

;; Calculate fulfillment with caching
(define (fulfilled zipper)
  (with-node-cache fulfilled-m zipper))

(define (fulfilled-m zipper cache)
  (let* ((current (zipper-current zipper))
         (cache-key (fulfillment-cache-key (node-id current))))
    (with-cache cache-key
                (lambda (_) (compute-fulfillment current zipper))
                '()
                cache)))

(define (compute-fulfillment node zipper)
  (cond
    ;; Leaf nodes
    ((null? (node-children node))
     (if (is-contribution? zipper) 1.0 0.0))
    
    ;; Manual fulfillment with contribution children
    ((and (node-manual-fulfillment node)
          (has-direct-contribution-child? zipper))
     (if (not (has-non-contribution-child? zipper))
         (node-manual-fulfillment node)
         (let ((contrib-weight (contribution-children-weight zipper))
               (non-contrib-fulfillment (non-contribution-children-fulfillment zipper)))
           (+ (* (node-manual-fulfillment node) contrib-weight)
              (* non-contrib-fulfillment (- 1.0 contrib-weight))))))
    
    ;; Default case - calculate from children
    (else
      (apply + (map (lambda (child)
                      (* (fulfilled child) (share-of-parent child)))
                    (children zipper))))))

;; Calculate desire (unfulfilled need)
(define (desire zipper)
  (- 1.0 (fulfilled zipper)))

;;---------------------------
;; Mutual Fulfillment
;;---------------------------

;; Calculate share of general fulfillment - the proportion of 
;; fulfillment a node attributes to each contributor
(define (share-of-general-fulfillment ci target contributor)
  (let ((target-node (zipper-current target))
        (contributor-id (node-id (zipper-current contributor))))
    ;; First check if the contributor exists in the forest
    (if (not (assoc-ref ci contributor-id))
        0
        (with-node-cache 
         (lambda (z cache)
           (let* ((current (zipper-current z))
                  (cache-key (shares-cache-key (node-id current))))
             ;; Try to get cached normalized shares
             (receive (shares updated-cache) (cache-lookup cache-key cache)
               (if (and shares (pair? shares) (eq? (car shares) 'share-map))
                   ;; Use cached normalized shares
                   (let ((share-map (normalize-proportions (cadr shares))))
                     (values (or (assoc-ref share-map contributor-id) 0) 
                             updated-cache))
                   ;; Calculate shares for this target using exact Haskell logic
                   (let* ((root-contributor (assoc-ref ci contributor-id))
                          (contrib-id (node-id (zipper-current root-contributor)))
                          ;; Find nodes that have this contributor
                          (contributing-nodes 
                           (filter (lambda (node)
                                     (and (member contrib-id 
                                                  (node-contributors (zipper-current node)))
                                          (is-contribution? node)))
                                   (cons z (descendants z))))
                          ;; Calculate weighted contribution
                          (node-weights (map weight contributing-nodes))
                          (node-fulfillments (map fulfilled contributing-nodes))
                          (contributor-counts 
                           (map (lambda (node) 
                                  (length (node-contributors (zipper-current node))))
                                contributing-nodes))
                          ;; Calculate weighted total exactly as in Haskell
                          (weighted-total 
                           (apply + 
                                  (map (lambda (w f c)
                                         (/ (* w f) c))
                                       node-weights
                                       node-fulfillments
                                       contributor-counts)))
                          ;; Build a map for all contributors
                          (all-shares (compute-all-contributor-shares z ci)))
                     
                     ;; Store complete share map for future lookups
                     (values weighted-total
                             (cache-insert cache-key 
                                          `(share-map ,all-shares)
                                          updated-cache)))))))
         target))))

;; Helper to compute shares for all contributors - used for caching
(define (compute-all-contributor-shares target ci)
  (let* ((all-contributors (map car ci))
         (shares 
          (map (lambda (contributor-id)
                 (let* ((contributor (assoc-ref ci contributor-id))
                        (share (if contributor
                                   (compute-single-contributor-share 
                                    target contributor contributor-id)
                                   0)))
                   (cons contributor-id share)))
               all-contributors)))
    ;; Return normalized shares
    (normalize-proportions shares)))

;; Helper to compute share for a single contributor without caching
(define (compute-single-contributor-share target contributor contributor-id)
  (let* ((contrib-id (node-id (zipper-current contributor)))
         ;; Find nodes that have this contributor
         (contributing-nodes 
          (filter (lambda (node)
                    (and (member contrib-id 
                                 (node-contributors (zipper-current node)))
                         (is-contribution? node)))
                  (cons target (descendants target))))
         ;; Calculate weighted contribution
         (node-weights (map weight contributing-nodes))
         (node-fulfillments (map fulfilled contributing-nodes))
         (contributor-counts 
          (map (lambda (node) 
                 (length (node-contributors (zipper-current node))))
               contributing-nodes))
         ;; Calculate weighted total exactly as in Haskell
         (weighted-total 
          (apply + 
                 (map (lambda (w f c)
                        (/ (* w f) c))
                      node-weights
                      node-fulfillments
                      contributor-counts))))
    weighted-total))

;; Calculate mutual fulfillment between two nodes with caching
(define (mutual-fulfillment ci a b)
  (with-node-cache 
   (lambda (z cache)
     (let* ((a-node (zipper-current z))
            (b-node (zipper-current b))
            (cache-key (mutual-cache-key (node-id a-node) (node-id b-node))))
       (receive (value updated-cache) (cache-lookup cache-key cache)
         (if value
             (values value updated-cache)
             (let ((a-to-b (share-of-general-fulfillment ci z b))
                   (b-to-a (share-of-general-fulfillment ci b z)))
               (let ((result (min a-to-b b-to-a)))
                 (values result (cache-insert cache-key result updated-cache))))))))
   a))

;;------------------------------
;; Mutual Fulfillment Utils
;;------------------------------

;; Get current path from root
(define (get-current-path zipper)
  (let loop ((z zipper)
             (path '()))
    (let ((parent (exit-to-parent z)))
      (if parent
          (loop parent (cons (node-id (zipper-current z)) path))
          path))))

;; Follow a path from a node
(define (follow-path path zipper)
  (if (null? path)
      zipper
      (let ((next-zipper (enter-child (car path) zipper)))
        (if next-zipper
            (follow-path (cdr path) next-zipper)
            #f))))

;; Score a path for mutual fulfillment
(define (score-path path root target ci)
  (let ((node (follow-path path root)))
    (if node
        (mutual-fulfillment ci node target)
        0)))

;; Find path to highest mutual fulfillment node
(define (find-highest-mutual-path ci root target)
  (let* ((all-paths (map get-current-path (cons root (descendants root))))
         (path-scores (map (lambda (path) 
                             (cons path (score-path path root target ci)))
                          all-paths))
         (sorted-paths (sort path-scores 
                            (lambda (a b) (> (cdr a) (cdr b))))))
    (if (null? sorted-paths)
        #f
        (car (car sorted-paths)))))

;; Get nodes with high mutual fulfillment
(define (get-high-mutual-nodes ci z threshold)
  (filter (lambda (node)
            (> (mutual-fulfillment ci z node) threshold))
          (cons z (descendants z))))

;;------------------------------------------
;; Unified Provider-centric share calculation
;;------------------------------------------

;; Calculate provider shares to any depth with caching
(define (provider-shares ci provider depth)
  (with-node-cache
   (lambda (z cache)
     (let* ((current (zipper-current z))
            (cache-key (provider-shares-cache-key (node-id current) depth)))
       (receive (shares updated-cache) (cache-lookup-normalized cache-key cache)
         (if shares
             (values shares updated-cache)
             (let ((computed-shares 
                    (if (<= depth 1)
                        ;; At depth=1, direct shares using mutual fulfillment
                        (normalize-proportions
                         (compute-direct-provider-shares z ci))
                        ;; For depth > 1, compute transitive shares
                        (normalize-proportions 
                         (compute-transitive-shares ci z depth)))))
               (values computed-shares
                       (cache-insert cache-key 
                                    `(share-map ,computed-shares) 
                                    updated-cache)))))))
   provider))

;; Calculate direct provider shares (depth=1)
(define (compute-direct-provider-shares provider ci)
  (let* ((contributors (node-contributors (zipper-current provider)))
         (valid-contributors 
          (filter-map (lambda (c) (assoc-ref ci c)) 
                     contributors))
         (mutual-values 
          (map (lambda (c)
                (cons (node-id (zipper-current c))
                      (mutual-fulfillment ci provider c)))
               valid-contributors)))
    mutual-values))

;; Compute transitive shares using cached results whenever possible
(define (compute-transitive-shares ci provider depth)
  (let ((direct-shares (provider-shares ci provider 1))
        (visited (list (node-id (zipper-current provider)))))
    
    (define (process-depth d shares)
      (if (>= d depth)
          shares
          ;; Process next level of depth
          (let* ((recipients (map car shares))
                 (unvisited-recipients (filter (lambda (r) (not (member r visited))) recipients))
                 (updated-shares (process-recipients unvisited-recipients shares)))
            (process-depth (+ d 1) updated-shares))))
    
    (define (process-recipients recipients shares-acc)
      (fold (lambda (recipient-id acc)
              (process-recipient recipient-id acc))
            shares-acc
            recipients))
    
    (define (process-recipient recipient-id shares)
      (let ((recipient-share (assoc-ref shares recipient-id))
            (recipient (assoc-ref ci recipient-id)))
        (if (or (not recipient-share) (not recipient))
            shares
            (begin
              ;; Mark as visited to prevent cycles
              (set! visited (cons recipient-id visited))
              ;; Get cached direct shares for this recipient
              (let ((recipient-direct-shares (provider-shares ci recipient 1)))
                ;; Add weighted shares to result
                (fold
                 (lambda (pair result)
                   (let* ((id (car pair))
                          (share (cdr pair))
                          (existing (assoc-ref result id)))
                     ;; Either update existing or add new entry
                     (if existing
                         (assoc-set! result id (+ existing (* recipient-share share)))
                         (cons (cons id (* recipient-share share)) result))))
                 ;; Start with current shares
                 shares
                 recipient-direct-shares))))))
    
    (process-depth 1 direct-shares)))

;; Helper function to get a specific share from provider
(define (get-share-from-provider ci provider recipient depth)
  (let ((shares (provider-shares ci provider depth)))
    (or (assoc-ref shares (node-id (zipper-current recipient))) 0)))

;; Forest management using association lists
(define (add-to-forest forest zipper)
  (cons (cons (node-id (zipper-current zipper)) zipper) forest))

(define (merge-contributors forests)
  (apply append forests))

;; Example of creating a forest
(define (create-example-forest)
  (let* ((alice (make-zipper (make-node "alice" "Alice" 100 '("bob" "charlie") 0.5)))
         (bob (make-zipper (make-node "bob" "Bob" 100 '("alice" "charlie") #f)))
         (charlie (make-zipper (make-node "charlie" "Charlie" 100 '("alice" "bob") #f)))
         
         ;; Add children
         (alice-with-child (add-child "alice-child" 30 '("bob" "charlie") #f alice))
         (bob-with-child (add-child "bob-child" 40 '("alice" "charlie") #f bob))
         (charlie-with-child (add-child "charlie-child" 50 '("alice" "bob") #f charlie))
         
         ;; Create forest and contributor index
         (forest (fold add-to-forest '() 
                       (list alice-with-child bob-with-child charlie-with-child))))
    
    forest))

;; Normalize a proportion map (association list)
(define (normalize-proportions proportions)
  (let ((total (apply + (map cdr proportions))))
    (if (or (= total 0) (= total 1.0))
        proportions
        (map (lambda (pair)
               (cons (car pair) (/ (cdr pair) total)))
             proportions))))

;; Cache lookup with normalization for proportion maps
(define (cache-lookup-normalized key cache)
  (receive (value updated-cache) (cache-lookup key cache)
    (if value
        (if (and (pair? value) (eq? (car value) 'share-map))
            (values (normalize-proportions (cadr value)) updated-cache)
            (values value updated-cache))
        (values #f updated-cache))))

;; Direct share and initial shares are now the same as provider-shares at depth=1
(define (direct-share ci provider recipient-id)
  (or (assoc-ref (provider-shares ci provider 1) recipient-id) 0))

;; Get a receiver's share from a specific capacity provider
(define (receiver-share-from ci receiver provider capacity max-depth)
  (let ((provider-share-map (provider-shares ci provider max-depth))
        (receiver-id (node-id (zipper-current receiver))))
    (or (assoc-ref provider-share-map receiver-id) 0)))