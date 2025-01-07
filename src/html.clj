(defn tag [name attrs & children]
  {:tag name :attrs (or attrs null) :children children})

(defn configure_rewriter [dsl rewriter]
  (let [items []
        last_ul []
        text_buffer (atom "")
        enabled (atom false)
        text_save_disabled (atom false)]
    (->
     rewriter
     (.on
      "head > title"
      {:text (fn [t] (reset! text_buffer (str (deref text_buffer) t.text)))
       :element (fn [element]
                  ;; (eprintln "TITLE:" element)
                  (reset! text_buffer "")
                  (.onEndTag
                   element
                   (fn [endTag]
                     (.push items
                            (tag "a" {:href dsl.url}
                                 (tag :h3 null (str (.replaceAll (deref text_buffer) (RegExp. "&nbsp;.+" "g") "") dsl.id))))
                     (reset! text_buffer ""))))})
     (.on
      "div.devsite-article-body > h3"
      {:element
       (fn [element]
         (reset! enabled (= dsl.id (.getAttribute element "id"))))})
     (.on
      "div.devsite-article-body > ul"
      {:text
       (fn [t] (reset! text_buffer (str (deref text_buffer) t.text)))
       :element
       (fn [element]
         (if (deref enabled)
           (do
             (.splice last_ul 0 last_ul.length)
             (.onEndTag
              element
              (fn [endTag]
                (let [ns_or_stub (if (empty? last_ul) [(tag :li null "Нет информации")] last_ul)]
                  (.push items (tag :ul null (spread ns_or_stub)))))))
           null))})
     (.on
      "div.devsite-article-body > ul > li"
      {:text
       (fn [t]
         (if text_save_disabled
           null
           (reset! text_buffer (str (deref text_buffer) t.text))))
       :element
       (fn [element]
         (if (deref enabled)
           (do
             (reset! text_buffer "")
             (.onEndTag
              element
              (fn [endTag]
                (.push last_ul (tag :li null
                                    (->
                                     (deref text_buffer)
                                     (.replaceAll "&#39;" "'")
                                     (.replaceAll "&lt;" "<")
                                     (.replaceAll "&gt;" ">"))))
                (reset! text_buffer ""))))
           null))})
     (.on
      "div.devsite-article-body > ul > li > *"
      {:element
       (fn [element]
         (reset! text_save_disabled true)
         (.onEndTag
          element
          (fn []
            (reset! text_save_disabled false))))})
     (.on
      "div.devsite-article-body > p > strong"
      {:text
       (fn [t] (reset! text_buffer (str (deref text_buffer) t.text)))
       :element
       (fn [element]
         (if (deref enabled)
           (do
             (reset! text_buffer "")
             (.onEndTag
              element
              (fn [endTag]
                (.push items (tag :h4 null (deref text_buffer)))
                (reset! text_buffer ""))))
           null))}))
    {:decode (fn []
              ;;  (eprintln "RESULT:" items)
               items)}))
