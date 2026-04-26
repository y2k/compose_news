(ns main-test
  (:require [main :as m]
            ["node:test" :as test]
            ["node:assert" :as assert]))

(test/before
 (fn []
   nil))

(test/after
 (fn []
   nil))

(test/test "stub"
           (fn []
             (let [ef (m/handle-fetch
                       (Request. "http://localhost/")
                       {}
                       {})]
               (eprintln
                (ef {})))

             (assert/ok true)))
