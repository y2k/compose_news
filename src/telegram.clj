(ns telegram
  (:require [effect-fetch :as f]))

(defn send-message [config chat_id text]
  (f/fetch
   (str "https://api.telegram.org/bot" (:token config) "/sendMessage")
   {:method "POST"
    :headers {"Content-Type" "application/json"}
    :decoder :json
    :body (.stringify js/JSON
                      {:chat_id chat_id
                       :text text})}))
