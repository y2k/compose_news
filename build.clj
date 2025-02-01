(ns _ (:require ["./vendor/make/0.2.0/main" :as b]))

(b/generate
 [(b/module
   {:lang "js"
    :src-dir "src"
    :target-dir ".github/bin/src"
    :items ["event_source" "html" "main" "rss" "telegraph"]})
  (b/module
   {:lang "js"
    :src-dir "test"
    :target-dir ".github/bin/test"
    :items ["test" "test.main"]})
  (b/vendor
   {:lang "js"
    :target-dir ".github/bin/vendor"
    :items [{:name "rec_json"     :version "0.1.0"}
            {:name "effects"      :version "0.1.0"}
            {:name "cf-xmlparser" :version "0.1.0"}]})])
