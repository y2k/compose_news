(defn configure_rewriter [rewriter]
  (let [items []
        id (atom 0)
        text_buffer (atom "")]
    (->
     rewriter
     (.on
      "entry:first-of-type > id"
      {:text
       (fn [t] (reset! text_buffer (str (deref text_buffer) t.text)))
       :element
       (fn [element]
         (reset! text_buffer "")
         (let [link (.getAttribute element "href")]
           (.onEndTag
            element
            (fn [endTag]
              (reset! id (deref text_buffer))
              (reset! text_buffer "")))))})
     (.on
      "entry:first-of-type > content > li > a"
      {:text
       (fn [t] (reset! text_buffer (str (deref text_buffer) t.text)))
       :element
       (fn [element]
         (reset! text_buffer "")
         (let [link (.getAttribute element "href")]
           (.onEndTag
            element
            (fn [endTag]
              (.push items {:title (deref text_buffer) :url link})
              (reset! text_buffer "")))))}))
    {:decode (fn []
              ;;  (eprintln "RESULT:" items id)
               {:items items :id (deref id)})}))
