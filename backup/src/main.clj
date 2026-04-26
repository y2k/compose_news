(ns main
  (:require ["../vendor/effects/main" :as e]
             ["../vendor/cf-xmlparser/main" :as hrw]
             ["./views" :as views]
             ["./telegraph" :as tg]
             ["./html" :as h]
             ["./rss" :as cr]
             ["./event_source" :as es]))

;; (defn- fetchX      [url props] (fn [w] ((:fetch w)       {:url url :props props})))
;; (defn- db_read     [key]       (fn [w] ((:db_read w)     {:key key})))
;; (defn- db_write    [key value] (fn [w] ((:db_write w)    {:key key :value value})))
;; (defn- resolve_env [key]       (fn [w] ((:resolve_env w) key)))
;; (defn- request_text []         (fn [w] ((:request_text w))))

;; (defn- attempt [fx]
;;   (fn [w]
;;     (.then
;;      (fx w)
;;      (fn [value] {:ok true :value value})
;;      (fn [error] {:ok false :error error}))))

;; (defn- escape_html [value]
;;   (->
;;    value
;;    (str)
;;    (.replaceAll "&" "&amp;")
;;    (.replaceAll "<" "&lt;")
;;    (.replaceAll ">" "&gt;")
;;    (.replaceAll "\"" "&quot;")))

;; (declare render_html)

;; (defn- render_attrs [attrs]
;;   (apply str
;;          (map (fn [[k v]]
;;                 (str " " (name k) "=\"" (escape_html v) "\""))
;;               attrs)))

;; (defn- render_html [node]
;;   (cond
;;     (nil? node) ""
;;     (string? node) (escape_html node)
;;     (number? node) (str node)
;;     (sequential? node)
;;     (if (keyword? (first node))
;;       (let [[tag maybe-attrs & rest] node
;;             [attrs children] (if (map? maybe-attrs)
;;                                [maybe-attrs rest]
;;                                [{} (cons maybe-attrs rest)])]
;;         (str "<" (name tag) (render_attrs attrs) ">"
;;              (apply str (map render_html children))
;;              "</" (name tag) ">"))
;;       (apply str (map render_html node)))
;;     :else
;;     (escape_html node)))

;; (defn- html_response [view]
;;   (Response. (str "<!DOCTYPE html>" (render_html view))
;;              {:headers {"Content-Type" "text/html; charset=UTF-8"}}))

;; (defn- parse_form_data [body]
;;   (let [params (js/URLSearchParams. body)]
;;     {:link_to_event (.get params "link_to_event")}))

;; (defn- execute_request_effect [request env fx]
;;   (fx (assoc (create_env env)
;;              :request_text (fn [] (.text request)))))

;; (defn- send_text_message [content options]
;;   (e/then
;;    (resolve_env :TARGET_CHAT)
;;    (fn [target_chat]
;;      (fetchX "https://api.telegram.org/bot~TG_TOKEN~/sendMessage"
;;              {:method "POST"
;;               :body (JSON.stringify
;;                      (merge
;;                       {:chat_id target_chat :text content}
;;                       (if (some? options) options {})))
;;               :headers {"Content-Type" "application/json"}}))))

;; (defn- create_telegraph_page [results]
;;   (e/then
;;    (resolve_env :TELEGRAPH_TOKEN)
;;    (fn [telegraph_token]
;;      (let [content (tg/create_page telegraph_token results)]
;;        (e/then
;;         (fetchX "https://api.telegra.ph/createPage"
;;                 {:method :POST
;;                  :headers {"content-type" "application/json"}
;;                  :body content})
;;         (fn [r] (e/pure (:url (:result (JSON.parse r))))))))))

;; (defn- chunk_array [array size]
;;   (if (<= (.-length array) size)
;;     [array]
;;     (concat [(.slice array 0 size)] (chunk_array (.slice array size) size))))

;; (defn- create_telegraph_page_batched [results]
;;   (->
;;    (chunk_array results 10)
;;    (.map (fn [xs] (create_telegraph_page xs)))
;;    (e/batch)
;;    (e/then
;;     (fn [xs]
;;       (if (empty? xs)
;;         (e/pure nil)
;;         (send_text_message
;;          (.reduce
;;           xs
;;           (fn [acc x] (str acc "\n- " x))
;;           "Обновления Jepack Compose:\n")
;;          {:link_preview_options
;;           {:show_above_text true
;;            :url "https://developer.android.com/static/codelabs/jetpack-compose-animation/img/jetpack_compose_logo_with_rocket_1920.png"}}))))))

;; (def- LAST_ID_KEY "last_id")

;; (defn main []
;;   (->
;;    (e/batch [(fetchX "https://developer.android.com/feeds/androidx-release-notes.xml"
;;                      {:decoder {:type :htmlrewriter :config cr/configure_rewriter}})
;;              (db_read LAST_ID_KEY)])
;;    (e/then (fn [[{body :items id :id} last_id]]
;;              (if (= last_id id)
;;                (e/pure nil)
;;                (e/batch [(db_write LAST_ID_KEY id)
;;                          (->
;;                           body
;;                           (.filter (fn [{url :url}] (.includes url "compose")))
;;                           (.map (fn [{url :url}]
;;                                   (fetchX url
;;                                           {:decoder {:type :htmlrewriter
;;                                                      :config (fn [rw] (h/configure_rewriter
;;                                                                        {:id (-> url (.split "#") (get 1))
;;                                                                         :url url} rw))}})))
;;                           (e/batch)
;;                           (e/then (fn [htmls]
;;                                     (if (= 0 (.-length htmls))
;;                                       (e/pure nil)
;;                                       (create_telegraph_page_batched htmls)))))]))))))

;; Infrastructure

;; (defn create_env [env]
;;   {:bindings env
;;    :resolve_env (fn [key] (Promise.resolve (get env key)))
;;    :db_read (fn [{key :key}]
;;               (.get env.COMPOSE_NEWS_KV key))
;;    :db_write (fn [{key :key value :value}]
;;                (.put env.COMPOSE_NEWS_KV key value))
;;    :raw-fetch (fn [{url :url props :props}]
;;                 (->
;;                  (fetch (.replaceAll url "~TG_TOKEN~" env.TG_TOKEN) props)
;;                  (.then (fn [r] (.text r)))))
;;    :fetch (fn [{url :url props :props}]
;;             (let [decoder (:decoder props)]
;;               (->
;;                (fetch (.replaceAll url "~TG_TOKEN~" env.TG_TOKEN) props)
;;                (.then (fn [response]
;;                         (if (and (some? decoder) (= (:type decoder) :htmlrewriter))
;;                           (hrw/parse response (:config decoder))
;;                           (.text response)))))))})

;; HTTP server

(defn handle-fetch [request env ctx]
  (let [url (js/URL. request.url)
         path url.pathname
         method request.method]
    (cond
      (and (= path "/") (= method "GET"))
      (execute_request_effect request env (e/pure (html_response (views/home-page))))

      (and (= path "/submit") (= method "POST"))
      (execute_request_effect
       request
       env
       (->
        (request_text)
        (e/then
         (fn [body]
           (let [form-data (parse_form_data body)
                 link (:link_to_event form-data)
                 response (html_response (views/submit-result link))]
             (->
              (attempt (send_text_message (str "Новая рекомендация: " link) nil))
              (e/then
               (fn [result]
                 (when-not (:ok result)
                   (eprintln (:error result)))
                 (e/pure response)))))))))

      :else
      (Response. "Not Found" {:status 404}))))

;;  :scheduled (fn [event env ctx] (.waitUntil ctx (.finally
;;                                                   ((main) (es/decorate (create_env env)))
;;                                                   (fn [] (es/reset_cache env)))))

(export-default {:fetch handle-fetch})
