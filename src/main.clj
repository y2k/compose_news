(ns main (:require ["../vendor/effects/main" :as e]
                   ["../vendor/cf-xmlparser/main" :as hrw]
                   ["./telegraph" :as tg]
                   ["./html" :as h]
                   ["./rss" :as cr]
                   ["./event_source" :as es]))

(defn- fetchX      [url props] (fn [w] ((:fetch w)       {:url url :props props})))
(defn- db_read     [key]       (fn [w] ((:db_read w)     {:key key})))
(defn- db_write    [key value] (fn [w] ((:db_write w)    {:key key :value value})))
(defn- resolve_env [key]       (fn [w] ((:resolve_env w) key)))

(defn- send_text_message [content]
  (e/then
   (resolve_env :TARGET_CHAT)
   (fn [target_chat]
     (fetchX "https://api.telegram.org/bot~TG_TOKEN~/sendMessage"
             {:method "POST"
              :body (JSON.stringify {:chat_id target_chat :text content})
              :headers {"Content-Type" "application/json"}}))))

(defn- create_telegraph_page [results]
  (e/then
   (resolve_env :TELEGRAPH_TOKEN)
   (fn [telegraph_token]
     (let [content (tg/create_page telegraph_token results)]
       (e/then
        (fetchX "https://api.telegra.ph/createPage"
                {:method :POST
                 :headers {"content-type" "application/json"}
                 :body content})
        (fn [r] (e/pure (:url (:result (JSON.parse r))))))))))

(defn- chunk_array [array size]
  (if (<= (.-length array) size)
    [array]
    (concat [(.slice array 0 size)] (chunk_array (.slice array size) size))))

(defn- create_telegraph_page_batched [results]
  (->
   (chunk_array results 10)
   (.map (fn [xs] (create_telegraph_page xs)))
   (e/batch)
   (e/then
    (fn [xs]
      (-> xs
          (.map (fn [r] (send_text_message r)))
          (e/batch))))))

;; (defn- create_telegraph_page_batched [results]
;;   (->
;;    (chunk_array results 10)
;;    (.map (fn [xs] (create_telegraph_page xs)))
;;    (e/batch)
;;    (e/then
;;     (fn [xs]
;;       (if (empty? xs)
;;         (e/pure nil)
;;         (reduce
;;          (fn [acc x] (str acc "\n" x))
;;          "Обновление Jepack Compose:"
;;          xs))))))

(def- LAST_ID_KEY "last_id")

(defn main []
  (->
   (e/batch [(fetchX "https://developer.android.com/feeds/androidx-release-notes.xml"
                     {:decoder {:type :htmlrewriter :config cr/configure_rewriter}})
             (db_read LAST_ID_KEY)])
   (e/then (fn [[{body :items id :id} last_id]]
             (if (= last_id id)
               (e/pure nil)
               (e/batch [(db_write LAST_ID_KEY id)
                         (->
                          body
                          (.filter (fn [{url :url}] (.includes url "compose")))
                          (.map (fn [{url :url}]
                                  (fetchX url
                                          {:decoder {:type :htmlrewriter
                                                     :config (fn [rw] (h/configure_rewriter
                                                                       {:id (-> url (.split "#") (get 1))
                                                                        :url url} rw))}})))
                          (e/batch)
                          (e/then (fn [htmls]
                                    (if (= 0 (.-length htmls))
                                      (e/pure nil)
                                      (create_telegraph_page_batched htmls)))))]))))))

;; Infrastructure

(defn create_env [env]
  {:bindings env
   :resolve_env (fn [key] (Promise.resolve (get env key)))
   :db_read (fn [{key :key}]
              (.get env.COMPOSE_NEWS_KV key))
   :db_write (fn [{key :key value :value}]
               (.put env.COMPOSE_NEWS_KV key value))
   :raw-fetch (fn [{url :url props :props}]
                (->
                 (fetch (.replaceAll url "~TG_TOKEN~" env.TG_TOKEN) props)
                 (.then (fn [r] (.text r)))))
   :fetch (fn [{url :url props :props}]
            (let [decoder (:decoder props)]
              (->
               (fetch (.replaceAll url "~TG_TOKEN~" env.TG_TOKEN) props)
               (.then (fn [response]
                        (if (and (some? decoder) (= (:type decoder) :htmlrewriter))
                          (hrw/parse response (:config decoder))
                          (.text response)))))))})

(export-default
 {:scheduled (fn [event env ctx] (.waitUntil ctx (.finally
                                                  ((main) (es/decorate (create_env env)))
                                                  (fn [] (es/reset_cache env)))))})
