(ns build (:require [make :as m]))

(deps {:make "0.5.0"})

(m/build-simple
 {:out ".github/bin"
  :target "js"
  :deps [["xml" "0.3.0"]
         ["effect" "0.1.0/js"]
         ["effect_fetch" "0.1.0/js"]]})
