(ns test (:require [js.wrangler :as w]
                   [js.fs.promises :as fs]))

(defn- read_log [dir]
  (.then
   (fs/readdir dir)
   (fn [xs]
     (->
      (.map xs (fn [fn]
                 (.then
                  (fs/readFile (str dir "/" fn) "utf-8")
                  (fn [content]
                    (if (.endsWith fn ".json")
                      (JSON.parse content)
                      {:out content})))))
      Promise.all))))

(defn- update_log [dir log]
  (let [log_path dir]
    (Promise.all
     (.map log (fn [x i]
                 (if (contains? x :in)
                   (fs/writeFile (str log_path "/" (.padStart (.toString (/ i 2)) 2 "0") ".json") (JSON.stringify x nil 2) "utf-8")
                   (fs/writeFile (str log_path "/" (.padStart (.toString (/ (- i 1) 2)) 2 "0") ".txt") (or (:out x) "") "utf-8")))))))

(def- DO_UPDATE false)

(defn main [name]
  (let [dir (str "../test/samples/" name)]
    (.then
     (read_log dir)
     (fn [log]
       (.then
        (w/unstable_dev "bin/test/test.main.js" {:experimental {:disableExperimentalWarning true}})
        (fn [worker]
          (->
           (.fetch worker "/" {:method :POST
                               :body (-> log
                                         (.filter (fn [x] (contains? x :out)))
                                         (.map (fn [x] (:out x)))
                                         (JSON.stringify))})
           (.then (fn [r] (if r.ok (.json r) (FIXME r.statusText))))
           (.then (fn [new_log]
                    (if (= (JSON.stringify log) (JSON.stringify new_log)) nil
                        (.then
                         (if DO_UPDATE
                           (update_log dir new_log)
                           (Promise.resolve nil))
                         (fn [] (FIXME "new_log is not equal to log"))))))
           (.finally (fn [] (.stop worker))))))))))

(->
 (Promise.resolve nil)
 (.then (fn [] (main "two_pages")))
 (.then (fn [] (main "no_updates")))
 (.then (fn [] (main "one_page")))
 (.then (fn [] (main "alread_updated"))))
