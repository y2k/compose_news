
(defn split-error-to-log [x ok]
  (if (is-success x)
    (dispatch ok (get-success x))
    (dispatch :logged (get-error x))))

(link
 :scheduled
 handle_scheduled
 (split-error-to-log
  (link0
   handle_xml_downloaded
   (split-error-to-log
    (link0
     handle_html_downloaded
     (split-error-to-log
      :FIXME))))))

;; Link(
;;    Scheduled,
;;    handle_sheduled,
;;    SplitResult(
;;        Link0(
;;            handle_xml_downloaded,
;;            SplitResult(
;;                Link0(
;;                    handle_html_downloaded,
;;                    SplitResult(???, Logged)
;;                ),
;;                Logged
;;            )
;;        ),
;;        Logged
;;    )
;; )
