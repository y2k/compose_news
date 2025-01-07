(ns _ (:require ["./vendor/build/0.1.0/main" :as b]))

(str
 (b/default)
 (b/build
  {:lang "js"
   :src-dir "src"
   :target-dir ".github/bin/src"
   :items ["event_source" "html" "main" "rss" "telegraph"]})
 (b/build
  {:lang "js"
   :src-dir "test"
   :target-dir ".github/bin/test"
   :items ["test" "test.main"]})
 (b/build
  {:lang "js"
   :src-dir "vendor/rec_json"
   :target-dir ".github/bin/vendor/rec_json"
   :items ["main"]})
 (b/build
  {:lang "js"
   :src-dir "vendor/effects/0.1.0"
   :target-dir ".github/bin/vendor/effects/0.1.0"
   :items ["main"]})
 (b/build
  {:lang "js"
   :src-dir "vendor/cf-xmlparser/0.1.0"
   :target-dir ".github/bin/vendor/cf-xmlparser"
   :items ["main"]}))
