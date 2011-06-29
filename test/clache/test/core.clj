(ns clache.test.core
  (:use [fogus.clache] :reload-all)
  (:use [clojure.test])
  (:import [fogus.clache BasicCache]))

(deftest test-basic-cache-lookup
  (testing "that the BasicCache can lookup as expected"
    (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo))))))
