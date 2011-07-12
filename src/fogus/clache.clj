;; clache.clj -- Cache implementations

;; by Michael Fogus - <http://fogus.me/fun/clache>
;; Jun. 2011

; Copyright (c) Michael Fogus, 2011. All rights reserved.  The use
; and distribution terms for this software are covered by the Eclipse
; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING the root of this
; distribution.  By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.  You must not
; remove this notice, or any other, from this software.

(ns fogus.clache
  "TODO"
  {:author "fogus"})

;; # Protocols and Types

(defprotocol CacheProtocol
  "This is the protocol describing the basic cache capability."
  (lookup  [cache e]
   "Retrieve the value associated with `e` if it exists")
  (has?    [cache e]
   "Checks if the cache contains a value associtaed with `e`")
  (hit     [cache e]
   "Is meant to be called if the cache is determined to contain a value
   associated with `e`")
  (miss    [cache e ret]
   "Is meant to be called if the cache is determined to **not** contain a
   value associated with `e`")
  (seed    [cache base]
   "Is used to signal that the cache should be created with a seed.
   The contract is that said cache should return an instance of its
   own type."))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result)))
  (seed [_ base]
    (BasicCache. base))
  Object
  (toString [_] (str cache)))

(deftype FIFOCache [cache q limit]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item]
    this)
  (miss [_ item result]
    (let [k (peek q)]
      (FIFOCache. (-> cache (dissoc k) (assoc item result))
                  (-> q pop (conj item))
                  limit)))
  (seed [_ base]
    (FIFOCache. base
                (into clojure.lang.PersistentQueue/EMPTY
                      (repeat limit :free))
                limit))
  Object
  (toString [_]
    (str cache \, \space (pr-str q))))

(defmethod print-method clojure.lang.PersistentQueue [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(deftype LRUCache [cache lru tick limit]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [_ item]
    (let [tick+ (inc tick)]
      (LRUCache. cache
                 (assoc lru item tick+)
                 tick+
                 limit)))
  (miss [_ item result]
    (let [tick+ (inc tick)
          k (apply min-key lru (keys lru))]
      (LRUCache. (-> cache (dissoc k) (assoc item result))
                 (-> lru (dissoc k) (assoc item tick+))
                 tick+
                 limit)))
  (seed [_ base]
    (LRUCache. base
               (into {} (for [x (range (- limit) 0)] [x x]))
               0
               limit))
  Object
  (toString [_]
    (str cache \, \space lru \, \space tick \, \space limit)))


;; # LIRS
 
;;  A
;;  [LIRS](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.116.2184)
;;  cache consists of two LRU lists, `S` and `Q`, and keeps more history
;;  than a LRU cache. Every cached item is either a LIR, HIR or
;;  non-resident HIR block. `Q` contains only HIR blocks, `S` contains
;;  LIR, HIR, non-resident HIR blocks. The total cache size is
;;  |`S`|+|`Q`|, |`S`| is typically 99% of the cache size.
 
;;  * LIR block:
;;    Low Inter-Reference block, a cached item with a short interval
;;    between accesses. A block `x`, `x` ∈ `S` ∧ `x` ∉ `Q` ∧ `x` ∈
;;    `cache`, is a LIR block.

;;  * HIR block:
;;    High Inter-Reference block, a cached item with rare accesses and
;;    long interval. A block `x`, `x` ∈ `Q` ∧ `x` ∈ `cache`, is a HIR block.

;;  * non-resident HIR block:
;;    only the key of the HIR block is cached, without the corresponding
;;    value a test (has?) for the corresponding key is always a
;;    miss. Used for additional history information. A block `x`, `x` ∈
;;    `S` ∧ `x` ∉ `Q` ∧ `x` ∉ `cache`, is a non-resident HIR block.
   
;; ## Outline of the implemented algorithm
 
;; `cache` is used to store the key value pairs.
;; `S` and `Q` maintain the relative order of accesses of the keys, like
;; a LRU list. 
   
;; Definition of `prune stack`:  
;;         
;;         repeatedly remove oldest item from S until an item k, k ∉ Q ∧
;;         k ∈ cache (a LIR block), is found  


;; In case of a miss for key `k` and value `v` (`k` ∉ cache) and
;;
;;  * (1.1) `S` is not filled, |`S`| < `limitS`       
;;         add k to S  
;;         add k to the cache  

;;  * (1.2) `k` ∉ `S`, never seen or not seen for a long, long time
;;             remove oldest item x from Q  
;;             remove x from cache  
;;             add k to S  
;;             add k to Q  
;;             add k to the cache  

;;  * (1.3) `k` ∈ `S`, this is a non-resident HIR block  
;;         remove oldest item x from Q  
;;         remove x from cache  
;;         add k to S  
;;         remove oldest item y from S
;;         add y to Q
;;         prune stack


;; In case of a hit for key `k` (`k` ∈ cache) and

;;  * (2.1) `k` ∈ `S` ∧ `k` ∉ `Q`, a LIR block
;;         add k to S / refresh
;;         prune stack if k was the oldest item in S

;;  * (2.2) `k` ∈ `S` ∧ `k` ∈ `Q`, a HIR block
;;         add k to S / refresh
;;         remove k from Q
;;         remove oldest item x from S
;;         add x to Q
;;         prune stack

;;  * (2.3) `k` ∉ `S` ∧ `k` ∈ `Q`, a HIR block, only older than the oldest item in S
;;         add k to S 
;;         add k to Q / refresh


(defn- prune-stack [lruS lruQ cache]
  (loop [s lruS q lruQ c cache]
    (let [k (apply min-key s (keys s))]
      (if (or (contains? q k)               ; HIR item
              (not (contains? c k)))        ; non-resident HIR item
        (recur (dissoc s k) q c)
        s))))

(deftype LIRSCache [cache lruS lruQ tick limitS limitQ]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [_ item]
    (let [tick+ (inc tick)]
      (if (not (contains? lruS item))
                                        ; (2.3) item ∉ S ∧ item ∈ Q
        (LIRSCache. cache (assoc lruS item tick+) (assoc lruQ item tick+) tick+ limitS limitQ)
        (let [k (apply min-key lruS (keys lruS))]
          (if (contains? lruQ item)
                                        ; (2.2) item ∈ S ∧ item ∈ Q
            (let [new-lruQ (-> lruQ (dissoc item) (assoc k tick+))] 
              (LIRSCache. cache
                          (-> lruS (dissoc k) (assoc item tick+) (prune-stack new-lruQ cache))
                          new-lruQ
                          tick+
                          limitS
                          limitQ))
                                        ; (2.1) item ∈ S ∧ item ∉ Q
            (LIRSCache. cache
                       (-> lruS (assoc item tick+) (prune-stack lruQ cache))
                       lruQ
                       tick+
                       limitS
                       limitQ))))))

  (miss [_ item result]
    (let [tick+ (inc tick)]
      (if (< (count cache) limitS)
                                        ; (1.1)
        (let [k (apply min-key lruS (keys lruS))]
          (LIRSCache. (assoc cache item result)
                      (-> lruS (dissoc k) (assoc item tick+))
                      lruQ
                      tick+
                      limitS
                      limitQ))
        (let [k (apply min-key lruQ (keys lruQ))
              new-lruQ (dissoc lruQ k)
              new-cache (-> cache  (dissoc k) (assoc item result))]
          (if (contains? lruS item)
                                        ; (1.3)
            (let [lastS (apply min-key lruS (keys lruS))] 
              (LIRSCache. new-cache
                          (-> lruS (dissoc lastS) (assoc item tick+) (prune-stack new-lruQ new-cache))
                          (assoc new-lruQ lastS tick+)
                          tick+
                          limitS
                          limitQ))
                                        ; (1.2)
            (LIRSCache. new-cache
                        (assoc lruS item tick+)
                        (assoc new-lruQ item tick+)
                        tick+
                        limitS
                        limitQ))))))
  (seed [_ base]
    (LIRSCache. base
                (into {} (for [x (range (- limitS) 0)] [x x]))
                (into {} (for [x (range (- limitQ) 0)] [x x]))
                0
                limitS
                limitQ))
  Object
  (toString [_]
    (str cache \, \space lruS \, \space lruQ \, \space tick \, \space limitS \, \space limitQ)))


(declare dissoc-dead)

(deftype TTLCache [cache ttl limit]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (when-let [t (get ttl item)]
      (< (- (System/currentTimeMillis)
            t)
         limit)))
  (hit [this item] this)
  (miss [this item result]
    (let [now  (System/currentTimeMillis)
          this (dissoc-dead this now)]
      (TTLCache. (assoc (:cache this) item result)
                 (assoc (:ttl this) item now)
                 limit)))
  (seed [_ base]
    (TTLCache. {} {} limit))
  
  Object
  (toString [_]
    (str cache \, \space ttl \, \space limit)))

(defn- dissoc-dead
  [state now]
  (let [ks (map key (filter #(> (- now (val %)) (:limit state))
                            (:ttl state)))
        dissoc-ks #(apply dissoc % ks)]
    (TTLCache. (dissoc-ks (:cache state))
               (dissoc-ks (:ttl state))
               (:limit state))))


(deftype LUCache [cache lu limit]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [_ item]
    (LUCache. cache (update-in lu [item] inc) limit))
  (miss [_ item result]
    (let [k (apply min-key lu (keys lu))]
      (LUCache. (-> cache (dissoc k) (assoc item result))
                (-> lu (dissoc k) (assoc item 0))
                limit)))
  (seed [_ base]
    (LUCache. base
              (into {} (for [x (range (- limit) 0)] [x x]))
              limit))
  
  Object
  (toString [_]
    (str cache \, \space lu \, \space limit)))

(deftype SoftCache [cache rq]
  CacheProtocol
  (lookup [_ item]
    (loop [r   (get cache item)
           val (.get @r)]
      (clojure.lang.Util/clearCache rq cache)
      (if (.isEnqueued @r)
        (recur r (.get @r))
        val)))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (SoftCache. (doto cache (.put item (reify
                                         clojure.lang.IDeref
                                         (deref [_]
                                           (java.lang.ref.SoftReference. result rq)))))
                rq))
  (seed [_ base]
    (SoftCache. base rq))
  Object
  (toString [_] (str cache)))

