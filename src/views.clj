(ns views)

(defn- layout [title content]
  [:html {}
   [:head {}
    [:meta {:charset "UTF-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:title {} title]
    [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"}]]
   [:body {}
    [:main {:class "container"}
     content]]])

(defn home-page []
  (layout "Рекомендовать новость"
          [:form {:method "POST" :action "/submit"}
           [:fieldset {}
            [:label {} "Ссылка на новость, библиотеку и тд."
             [:input {:name "link_to_event"
                      :placeholder "https://"
                      :type "url"
                      :required true}]]]
           [:input {:type "submit" :value "Предложить"}]
           [:p {} "Для канала: " [:a {:href "https://t.me/jetpack_compose"} "@jetpack_compose"]]]))

(defn submit-result [link]
  (layout "Рекомендовать новость"
          [:article {}
           [:p {} "Спасибо! Ссылка получена: " link]
           [:a {:href "/"} "Вернуться"]]))
