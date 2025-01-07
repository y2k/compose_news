(ns telegraph (:require ["./html" :as h]))

(defn- add_contents [nodes]
  (.concat
   [(h/tag "h3" {} "Оглавление")]
   [(h/tag "ul" {}
           (->
            nodes
            (.filter (fn [{tag :tag}] (= tag "a")))
            (.map (fn [{children :children}] (get (:children (get children 0)) 0)))
            (.map (fn [title]
                    (h/tag "li" {} (h/tag "a" {:href (str "#" (.replaceAll title " " "-"))} title))))
            spread))]
   nodes))

(defn- create_body [access_token title content]
  {:title title
   :access_token access_token
   :author_name "Compose News"
   :author_url "https://github.com/y2k/compose_news"
   :content content})

(defn create_page [token results]
  (let [doc (->
             (.flatMap
              results
              (fn [nodes]
                (concat
                 [(h/tag "hr")]
                 (.flatMap nodes (fn [x] x)))))
             add_contents)]
    (JSON.stringify (create_body token "Compose updates" doc))))
