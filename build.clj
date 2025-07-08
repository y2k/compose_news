(ns _ (:require [".github/vendor/make/0.3.0/main" :as b]))

(b/generate
 [{:target "js"
   :root "src"
   :out-dir ".github/bin/src"}
  {:target "js"
   :root "test"
   :out-dir ".github/bin/test"}
  ;; {:lang "js"
  ;;  :src-dir "test"
  ;;  :target-dir ".github/bin/test"
  ;;  :items ["test" "test.main"]}
  ;; (b/vendor
  ;;  {:lang "js"
  ;;   :target-dir ".github/bin/vendor"
  ;;   :items [{:name "rec_json"     :version "0.1.0"}
  ;;           {:name "effects"      :version "0.1.0"}
  ;;           {:name "cf-xmlparser" :version "0.1.0"}]})
  ])
