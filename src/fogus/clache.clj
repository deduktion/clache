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

