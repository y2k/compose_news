(ns main-test
  (:require [main :as m]
            [effect-fetch :as f]
            ["node:test" :as test]
            ["node:assert" :as assert]))

(defn- run-fetch [request env]
  ((m/handle-fetch request env {}) {}))

(defn- run-fetch-with-mock [request env fetch-fn]
  ((f/with-fetch fetch-fn
     (m/handle-fetch request env {}))
   {}))

(defn- assert-html-ok [response]
  (assert/strictEqual response.status 200)
  (assert/strictEqual
   (.get response.headers "Content-Type")
   "text/html")
  (.text response))

(test/test "GET /"
           (fn []
             (-> (run-fetch (Request. "http://localhost/") {})
                 (.then assert-html-ok)
                 (.then
                  (fn [response]
                    (assert/ok (.includes response "<form")))))))

(test/test "POST /submit"
           (fn []
             (let [link "https://example.com/news"
                   fetch-call (atom nil)
                   request (Request. "http://localhost/submit"
                                     {:method "POST"
                                      :headers {"Content-Type" "application/x-www-form-urlencoded"}
                                      :body (str "link_to_event=" (encodeURIComponent link))})]
               (-> (run-fetch-with-mock
                    request
                    {:TG_TOKEN "test-token"
                     :TELEGRAM_CHAT_ID "test-chat"}
                    (fn [url props]
                      (reset! fetch-call {:url url :props props})
                      (Promise/resolve (Response. "{}"))))
                   (.then assert-html-ok)
                   (.then
                    (fn [body]
                      (let [{url :url props :props} (deref fetch-call)
                            message (JSON.parse (:body props))]
                        (assert/strictEqual
                         url
                         "https://api.telegram.org/bottest-token/sendMessage")
                        (assert/strictEqual (:method props) "POST")
                        (assert/strictEqual (.-chat_id message) "test-chat")
                        (assert/strictEqual
                         (.-text message)
                         (str "Новая рекомендация (compose news): " link))
                        (assert/ok (.includes body "Спасибо!"))
                        (assert/ok (.includes body link)))))))))
