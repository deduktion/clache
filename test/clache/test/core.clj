(ns clache.test.core
  (:use [fogus.clache] :reload-all)
  (:use [clojure.test])
  (:import [fogus.clache BasicCache LIRSCache]))

(deftest test-basic-cache-lookup
  (testing "that the BasicCache can lookup as expected"
    (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo))))))

(defn- lirs-map [lirs]
  {:cache (.cache lirs)
   :lruS (.lruS lirs)
   :lruQ (.lruQ lirs)
   :tick (.tick lirs)
   :limitS (.limitS lirs)
   :limitQ (.limitQ lirs)})

(deftest test-LIRSCache
  (testing "that the LIRSCache can lookup as expected"
    (is (= :robot (lookup (miss (seed (LIRSCache. {} {} {} 0 1 1) {}) '(servo) :robot) '(servo)))))
  
  (testing "a hit of a LIR block:

L LIR block
H HIR block
N non-resident HIR block


          +-----------------------------+   +----------------+
          |           HIT 4             |   |     HIT 8      |
          |                             v   |                |
          |                                 |                |
    H 5   |                           L 4   |                v
    H 3   |                           H 5   |
    N 2   |                           H 3   |              L 8
    L 1   |                           N 2   |              L 4
    N 6   |                           L 1   |              H 5
    N 9   |                           N 6   |              H 3
    L 4---+ 5                         N 9   | 5            N 2     5
    L 8     3                         L 8---+ 3            L 1     3

      S     Q                           S     Q              S     Q

"
    (let [lirs (LIRSCache. {:1 1 :3 3 :4 4 :5 5 :8 8}
                           {:5 7 :3 6 :2 5 :1 4 :6 3 :9 2 :4 1 :8 0}
                           {:5 1 :3 0} 7 3 2)]
      (testing "hit 4"
        (is (= (lirs-map (hit lirs :4))
               (lirs-map (LIRSCache. {:1 1 :3 3 :4 4 :5 5 :8 8}
                                    {:5 7 :3 6 :2 5 :1 4 :6 3 :9 2 :4 8 :8 0}
                                    {:5 1 :3 0} 8 3 2)))))
      (testing "hit 8 prunes the stack"
        (is (= (lirs-map (-> lirs (hit :4) (hit :8)))
              (lirs-map (LIRSCache. {:1 1 :3 3 :4 4 :5 5 :8 8}
                                    {:5 7 :3 6 :2 5 :1 4 :4 8 :8 9}
                                    {:5 1 :3 0} 9 3 2)))))))
  (testing "a hit of a HIR block:

L LIR block
H HIR block
N non-resident HIR block


                     HIT 3                                  HIT 5
         +-----------------------------+         +------------------+-----+
         |                             |         |                  |     |
    L 4  |+----------------------------+-----+   |                  v     |
    L 8  ||                            v     |   |                        |
    H 5  ||                                  v   |                H 5     v
    H 3-- |                          L 3         |                L 3
    N 2   | 5                        L 4     1   |                L 4     5
    L 1---+ 3                        L 8     5---+                L 8     1

      S     Q                          S     Q                      S     Q

"
    (let [lirs (LIRSCache. {:1 1 :3 3 :4 4 :5 5 :8 8}
                           {:4 9 :8 8 :5 7 :3 6 :2 5 :1 4 }
                           {:5 1 :3 0} 9 3 2 )]
      (testing "hit 3 prunes the stack and moves oldest block of lruS to lruQ"
        (is (= (lirs-map (hit lirs :3))
               {:cache {:1 1 :3 3 :4 4 :5 5 :8 8}
                :lruS {:3 10 :4 9 :8 8}
                :lruQ {:1 10 :5 1}
                :tick 10 :limitS 3 :limitQ 2})))
      (testing "hit 5 adds the block to lruS"
        (is (= (lirs-map (-> lirs (hit :3) (hit :5)))
               {:cache {:1 1 :3 3 :4 4 :5 5 :8 8}
                :lruS {:5 11 :3 10 :4 9 :8 8}
                :lruQ {:5 11 :1 10}
                :tick 11 :limitS 3 :limitQ 2})))))
  (testing "a miss:

L LIR block
H HIR block
N non-resident HIR block


                     MISS 7                          MISS 9                    MISS 5
             ---------------------+-----+    -----------------+-----+     +-------------------+
                                  |     |                     v     |     |                   |
                                  v     |                           |     |                   v
                                        |                   H 9  + -| - - +
                                H 7     |                   H 7  |  |                       L 5  +--+
    H 5                         H 5     v                   N 5- +  v                       H 9  |  v
    L 3                         L 3                         L 3                             N 7  |
    L 4     5                   L 4     7                   L 4     9                       L 3  |  8
    L 8     1                   L 8     5                   L 8--+  7                       L 4  |  9
                                                                 +-------------------------------+
      S     Q                     S     Q                     S     Q                         S     Q


"
    (let [lirs (LIRSCache. {:1 1 :3 3 :4 4 :5 5 :8 8}
                           {:5 11 :3 10 :4 9 :8 8}
                           {:5 11 :1 10} 11 3 2)]
      (testing "miss 7 adds the block to lruS and lruQ and removes the oldest block in lruQ"
        (is (= (lirs-map (miss lirs :7 7))
               {:cache {:3 3 :4 4 :5 5 :8 8 :7 7}
                :lruS {:7 12 :5 11 :3 10 :4 9 :8 8}
                :lruQ {:7 12 :5 11}
                :tick 12 :limitS 3 :limitQ 2})))
      (testing "miss 9 makes 5 a non-resident HIR block"
        (is (= (lirs-map (-> lirs (miss :7 7) (miss :9 9)))
               {:cache {:3 3 :4 4 :8 8 :7 7 :9 9}
                :lruS {:9 13 :7 12 :5 11 :3 10 :4 9 :8 8}
                :lruQ {:9 13 :7 12}
                :tick 13 :limitS 3 :limitQ 2})))
      (testing "miss 5, a non-resident HIR block becomes a new LIR block"
        (is (= (lirs-map (-> lirs (miss :7 7) (miss :9 9) (miss :5 5)))
               {:cache {:3 3 :4 4 :8 8 :9 9 :5 5}
                :lruS {:5 14 :9 13 :7 12 :3 10 :4 9}
                :lruQ {:8 14 :9 13}
                :tick 14 :limitS 3 :limitQ 2}))))))



