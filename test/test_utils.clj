(ns test-utils
  (:require [main :as m]
            [effect-fetch :as f]
            ["node:assert" :as assert]))

(defn assert-fetch-snapshot [request expected-base64]
  (let [effects (atom [])]
    (-> (run-with-mocks effects request {:TG_TOKEN "test-token" :TELEGRAM_CHAT_ID "test-chat"})
        (.then (fn [response] (.text response)))
        (.then
         (fn [body]
           (assert-json-snapshot
            {:effects (deref effects)
             :response body}
            expected-base64))))))

(defn- run-with-mocks [effects request env]
  ((f/with-fetch
     (fn [url props]
       (swap! effects (fn [items] (conj items {:type "fetch" :url url :props props})))
       (Promise/resolve (Response. "{}")))
     (m/handle-fetch request env {}))
   {}))

(defn- base64-encode [text]
  (-> (.from js/Buffer text)
      (.toString "base64")))

(defn- base64-decode [text]
  (-> (.from js/Buffer text "base64")
      (.toString "utf8")))

(defn- assert-json-snapshot [actual expected-base64]
  (let [actual-json (.stringify js/JSON actual)
        actual-base64 (base64-encode actual-json)]
    (assert/deepStrictEqual
     (JSON/parse actual-json)
     (JSON/parse (base64-decode expected-base64))
     actual-base64)))
