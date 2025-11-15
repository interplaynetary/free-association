(use-modules (srfi srfi-1)   ; List library
             (srfi srfi-9)   ; Record types
             (srfi srfi-43)  ; Vector library
             (ice-9 hash-table)
             (ice-9 match))  ; Pattern matching

;;----------------------
;; Caching System
;;----------------------

;; Define record types for our cache system
(define-record-type <cache>
  (make-cache cache-map cache-misses cache-hits)
  cache?
  (cache-map cache-map cache-map-set!)
  (cache-misses cache-misses cache-misses-set!)
  (cache-hits cache-hits cache-hits-set!))

;; Create an empty cache
(define (empty-cache)
  (make-cache (make-hash-table) 0 0))

;; We'll use tagged lists for cache values
(define (make-float-value val) (list 'float val))
(define (make-int-value val) (list 'int val))
(define (make-string-list-value val) (list 'string-list val))
(define (make-share-map-value val) (list 'share-map val))

;; Type predicates
(define (float-value? v) (and (pair? v) (eq? (car v) 'float)))
(define (int-value? v) (and (pair? v) (eq? (car v) 'int)))
(define (string-list-value? v) (and (pair? v) (eq? (car v) 'string-list)))
(define (share-map-value? v) (and (pair? v) (eq? (car v) 'share-map)))

;; Value extractors
(define (float-value-get v) (cadr v))
(define (int-value-get v) (cadr v))
(define (string-list-value-get v) (cadr v))
(define (share-map-value-get v) (cadr v))

;; Cache lookup function
(define (cache-lookup key cache)
  (let ((val (hash-ref (cache-map cache) key)))
    (if val
        (begin
          (cache-hits-set! cache (+ (cache-hits cache) 1))
          (values val cache))
        (values #f cache))))

;; Cache insert function
(define (cache-insert key value cache)
  (hash-set! (cache-map cache) key value)
  (cache-misses-set! cache (+ (cache-misses cache) 1))
  cache)

;; Monadic-style cache operations with continuation
(define (with-cache key compute arg cache)
  (let-values (((val updated-cache) (cache-lookup key cache)))
    (if val
        (values val updated-cache)
        (let ((result (compute arg)))
          (values result (cache-insert key result cache))))))

;;----------------------
;; Core Data Types
;;----------------------

;; Record for points
(define-record-type <points>
  (make-points value)
  points?
  (value points-value))

;; Define the Node type
(define-record-type <node>
  (make-node id name points children contributors manual-fulfillment capacities capacity-shares cache)
  node?
  (id node-id)
  (name node-name)
  (points node-points)
  (children node-children node-children-set!)
  (contributors node-contributors)
  (manual-fulfillment node-manual-fulfillment)
  (capacities node-capacities node-capacities-set!)
  (capacity-shares node-capacity-shares node-capacity-shares-set!)
  (cache node-cache node-cache-set!))

;; Define the Context type
(define-record-type <ctx>
  (make-ctx parent siblings ancestors)
  ctx?
  (parent ctx-parent)
  (siblings ctx-siblings)
  (ancestors ctx-ancestors))

;; Define the TreeZipper type
(define-record-type <tree-zipper>
  (make-tree-zipper current context)
  tree-zipper?
  (current zipper-current zipper-current-set!)
  (context zipper-context zipper-context-set!))

;; Cache key functions
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

;;-------------------------
;; Zipper Navigation
;;-------------------------

;; Enter a child node
(define (enter-child child-id zipper)
  (let* ((current (zipper-current zipper))
         (child (hash-ref (node-children current) child-id)))
    (if child
        (let* ((siblings (node-children current))
               (new-siblings (hash-table-copy siblings))
               (ctx (zipper-context zipper))
               (ancestors (if ctx (ctx-ancestors ctx) '()))
               (new-ctx (make-ctx current new-siblings ancestors)))
          (hash-remove! new-siblings child-id)
          (make-tree-zipper child (make-ctx current new-siblings ancestors)))
        #f)))

;; Exit to parent
(define (exit-to-parent zipper)
  (let ((ctx (zipper-context zipper)))
    (if (not ctx)
        #f
        (let* ((current (zipper-current zipper))
               (parent (ctx-parent ctx))
               (siblings (ctx-siblings ctx))
               (ancestors (ctx-ancestors ctx))
               (new-parent-children (node-children parent)))
          (hash-set! new-parent-children (node-id current) current)
          (make-tree-zipper parent (if (null? ancestors) #f (car ancestors)))))))

;; Navigate to a sibling
(define (enter-sibling name zipper)
  (let ((parent-zipper (exit-to-parent zipper)))
    (if parent-zipper
        (enter-child name parent-zipper)
        #f)))

;; Go to root of tree
(define (go-to-root zipper)
  (let ((parent (exit-to-parent zipper)))
    (if parent
        (go-to-root parent)
        zipper)))

;; Modify the current node
(define (modify-node f zipper)
  (make-tree-zipper 
    (f (zipper-current zipper))
    (zipper-context zipper)))

;; Get all siblings
(define (get-siblings zipper)
  (let ((parent (exit-to-parent zipper)))
    (if parent
        (hash-map->list (lambda (k v) k) (node-children (zipper-current parent)))
        '())))

;; Follow a path navigation
(define (follow-path path zipper)
  (if (null? path)
      zipper
      (let ((next-zipper (enter-child (car path) zipper)))
        (if next-zipper
            (follow-path (cdr path) next-zipper)
            #f))))

;; Get current path from root
(define (get-current-path zipper)
  (reverse (get-path zipper '())))

(define (get-path zipper acc)
  (let ((parent (exit-to-parent zipper)))
    (if parent
        (get-path parent (cons (node-id (zipper-current zipper)) acc))
        acc)))

;; Get child zippers
(define (children zipper)
  (filter-map 
    (lambda (cid) (enter-child cid zipper))
    (hash-map->list (lambda (k v) k) (node-children (zipper-current zipper)))))

;; Fold over children
(define (fold-children f z0 zipper)
  (fold f z0 (children zipper)))

;; Map over children
(define (map-children f zipper)
  (map f (children zipper)))

;; Check if any child satisfies a predicate
(define (any-child p zipper)
  (any p (children zipper)))

;; Check if all children satisfy a predicate
(define (all-children p zipper)
  (every p (children zipper)))

;; Get all descendants
(define (descendants zipper)
  (cdr (get-all-descendants-cached zipper)))

;;----------------------------
;; Tree Modification API
;;----------------------------

;; Create a root node
(define (create-root-node id name pts contribs manual)
  (make-node 
    id 
    name 
    (make-points pts)
    (make-hash-table) 
    (list->set contribs)
    (clamp-manual manual)
    (make-hash-table)
    (make-hash-table)
    (empty-cache)))

(define (clamp-manual manual)
  (and manual (max 0 (min 1 manual))))

;; Add a child
(define (add-child name pts contribs manual zipper)
  (let* ((current (zipper-current zipper))
         (new-child (create-root-node name name pts contribs manual))
         (children (node-children current)))
    (hash-set! children name new-child)
    (node-cache-set! current (empty-cache))
    zipper))

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
                          (apply + (map-children 
                                     (lambda (child) 
                                       (points-value (node-points (zipper-current child))))
                                     zipper)))))
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
                   (lambda (_) (compute-weight current))
                   '()
                   cache))))

(define (compute-weight node zipper)
  (let* ((parent-zipper (exit-to-parent zipper))
         (total (total-child-points parent-zipper))
         (current-points (points-value (node-points node)))
         (parent-weight (weight parent-zipper)))
    (if (= total 0)
        0
        (* (/ current-points total) parent-weight))))

;; Calculate share of parent
(define (share-of-parent zipper)
  (let ((parent (exit-to-parent zipper)))
    (if (not parent)
        1.0 ; Root node has 100% share
        (let* ((total (total-child-points parent))
               (current-points (points-value (node-points (zipper-current zipper)))))
          (if (= total 0)
              0
              (/ current-points total))))))

;; Predicates for node types
(define (is-contribution? zipper)
  (and (not (set-empty? (node-contributors (zipper-current zipper))))
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

;; Get all descendants with caching
(define (get-all-descendants-cached zipper)
  (cons zipper (with-node-cache get-all-descendants-cached-m zipper)))

(define (get-all-descendants-cached-m zipper cache)
  (let* ((current (zipper-current zipper))
         (current-id (node-id current))
         (cache-key (descendants-cache-key current-id)))
    (let-values (((val updated-cache) (cache-lookup cache-key cache)))
      (if (and val (string-list-value? val))
          (values (get-all-descendants zipper) updated-cache)
          (let ((descendants (get-all-descendants zipper))
                (descendant-ids (map (lambda (z) (node-id (zipper-current z))) descendants)))
            (values descendants
                    (cache-insert cache-key
                                 (make-string-list-value descendant-ids)
                                 cache)))))))

;; Get all descendants (including self)
(define (get-all-descendants zipper)
  (cons zipper
        (apply append
               (map get-all-descendants (children zipper)))))

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
    ((hash-table-empty? (node-children node))
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

;; Calculate share of general fulfillment
(define (share-of-general-fulfillment ci target contributor)
  (let* ((contrib-id (node-id (zipper-current contributor)))
         (root-contributor (hash-ref ci contrib-id)))
    (if (not root-contributor)
        0
        (let* ((contrib-id (node-id (zipper-current root-contributor)))
               (contributing-nodes 
                 (filter (lambda (node)
                           (and (set-contains? (node-contributors (zipper-current node)) contrib-id)
                                (is-contribution? node)))
                         (cons target (descendants target))))
               (total (apply + (map (lambda (node)
                                     (* (weight node) (fulfilled node)))
                                   contributing-nodes)))
               (contributor-counts (map (lambda (node) 
                                         (set-size (node-contributors (zipper-current node))))
                                       contributing-nodes))
               (weighted-total 
                 (apply + 
                        (map (lambda (w f c)
                               (/ (* w f) c))
                             (map weight contributing-nodes)
                             (map fulfilled contributing-nodes)
                             contributor-counts))))
          weighted-total))))

;; Calculate mutual fulfillment between two nodes with caching
(define (mutual-fulfillment ci a b)
  (with-node-cache (lambda (z cache) (mutual-fulfillment-m ci z b cache)) a))

(define (mutual-fulfillment-m ci a b cache)
  (let* ((a-node (zipper-current a))
         (b-node (zipper-current b))
         (cache-key (mutual-cache-key (node-id a-node) (node-id b-node))))
    (with-cache cache-key
               (lambda (_) (compute-mutual ci a b))
               '()
               cache)))

(define (compute-mutual ci a b)
  (let ((a-to-b (share-of-general-fulfillment ci a b))
        (b-to-a (share-of-general-fulfillment ci b a)))
    (min a-to-b b-to-a)))

;;------------------------------
;; Mutual Fulfillment Utils
;;------------------------------

;; Find path to highest mutual fulfillment node
(define (find-highest-mutual-path ci root target)
  (let* ((all-paths (map get-current-path (cons root (descendants root))))
         (path-scores (map (lambda (path) 
                            (cons path (score-path path)))
                          all-paths))
         (sort-paths (sort path-scores 
                          (lambda (a b) (> (cdr a) (cdr b))))))
    (if (null? sort-paths)
        #f
        (car (car sort-paths)))))

(define (score-path path root)
  (let ((node (follow-path path root)))
    (if node
        (mutual-fulfillment ci node target)
        0)))

;; Get nodes with high mutual fulfillment
(define (get-high-mutual-nodes ci z threshold)
  (filter (lambda (node)
            (> (mutual-fulfillment ci z node) threshold))
          (cons z (descendants z))))

;;------------------------------------------
;; Provider-centric share calculation
;;------------------------------------------

;; Calculate provider shares
(define (provider-shares ci provider depth)
  (if (<= depth 1)
      (initial-shares provider)
      (normalize-shares (process-providers ci (initial-shares provider) (make-hash-table) depth))))

(define (initial-shares provider)
  (let* ((contributors (node-contributors (zipper-current provider)))
         (valid-contributors (filter-map (lambda (c) (hash-ref ci c)) (set->list contributors)))
         (mutual-values (map (lambda (c)
                              (cons (node-id (zipper-current c))
                                    (mutual-fulfillment ci provider c)))
                            valid-contributors))
         (total (apply + (map cdr mutual-values))))
    (if (= total 0)
        (make-hash-table)
        (let ((shares (make-hash-table)))
          (for-each (lambda (pair)
                      (hash-set! shares (car pair) (/ (cdr pair) total)))
                    mutual-values)
          shares))))

(define (normalize-shares shares)
  (let ((total-share (apply + (hash-map->list (lambda (k v) v) shares))))
    (if (> total-share 0)
        (let ((normalized (hash-table-copy shares)))
          (hash-for-each 
            (lambda (k v) 
              (hash-set! normalized k (/ v total-share)))
            shares)
          normalized)
        shares)))

(define (process-providers ci shares visited depth)
  (if (< depth 2)
      shares
      (let ((final-shares (hash-table-copy shares)))
        (let loop ((depth 2))
          (if (> depth depth)
              final-shares
              (begin
                (process-depth ci final-shares visited)
                (loop (+ depth 1))))))))

(define (process-depth ci shares visited)
  (let ((recipients (hash-map->list (lambda (k v) k) shares))
        (unvisited-recipients (filter (lambda (r) (not (hash-ref visited r #f))) recipients)))
    (for-each (lambda (recipient-id)
                (process-recipient ci shares visited recipient-id))
              unvisited-recipients)
    shares))

(define (process-recipient ci shares visited recipient-id)
  (if (not (hash-ref shares recipient-id #f))
      shares  ; No share for this recipient
      (let ((recipient-share (hash-ref shares recipient-id))
            (recipient (hash-ref ci recipient-id #f)))
        (if (not recipient)
            shares  ; Recipient not found in contributor index
            (begin
              (hash-set! visited recipient-id #t)
              (let* ((connections (node-contributors (zipper-current recipient)))
                     (unvisited-connections 
                      (set-filter 
                        (lambda (c) (not (hash-ref visited c #f)))
                        connections))
                     (transitive-shares (provider-shares ci recipient 1)))
                ;; Add weighted transitive shares to the share map
                (hash-for-each
                  (lambda (k v)
                    (hash-set! shares k
                              (+ (hash-ref shares k 0) (* recipient-share v))))
                  transitive-shares)
                shares))))))

;; Cache-aware helper functions for node operations
(define (with-node-cache computation zipper)
  (let* ((current (zipper-current zipper))
         (cache (node-cache current))
         (result (computation zipper cache)))
    result))

;; Forest management
(define (add-to-forest forest zipper)
  (hash-set! forest (node-id (zipper-current zipper)) zipper)
  forest)

(define (merge-contributors forests)
  (let ((merged (make-hash-table)))
    (for-each (lambda (forest)
                (hash-for-each (lambda (k v) (hash-set! merged k v)) forest))
              forests)
    merged))

;; Utility functions for sets
(define (list->set lst)
  (let ((s (make-hash-table)))
    (for-each (lambda (item) (hash-set! s item #t)) lst)
    s))

(define (set-empty? s)
  (= (hash-count (const #t) s) 0))

(define (set-size s)
  (hash-count (const #t) s))

(define (set-contains? s item)
  (hash-ref s item #f))

(define (set->list s)
  (hash-map->list (lambda (k v) k) s))

(define (set-filter pred s)
  (let ((result (make-hash-table)))
    (hash-for-each 
      (lambda (k v) 
        (when (pred k) 
          (hash-set! result k #t)))
      s)
    result))
