(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)
(defvar bts-test:case-entry-amount 0)

(expectations

  (desc "summary-entry init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:scenario-ticket-fetcher
          (lambda (p q)
            (let ((amount (case (plist-get q :state)
                            ((open close) 1))))
              (bts:ticket-fetch-complete q (-slice bts-test:scenario-tickets 0 amount)))))
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (switch-to-buffer bts-test:case-summary-buffer)
    (setq bts-test:case-entry-amount (count-lines (point-min) (point-max)))
    (buffer-name))

  (desc "summary-entry reflection of query update")
  (expect t
    (bts-test:with-yaxception "main"
      (bts:query-update (bts:query-get-config-by-id 1))
      (bts-test:widget-edit 'state 'update (case (bts:widget-get-value 'state)
                                             (all 'open)
                                             (t   'all)))
      (call-interactively 'bts:widget-submit)
      (switch-to-buffer bts-test:case-summary-buffer)
      (call-interactively 'bts:summary-reload-all)
      (not (= (count-lines (point-min) (point-max))
              bts-test:case-entry-amount))))

  )

