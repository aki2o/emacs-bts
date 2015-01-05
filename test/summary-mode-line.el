(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)
(defvar bts-test:case-project-name "")
(defvar bts-test:case-query-name "")
(defvar bts-test:case-query-description "")

(expectations

  (desc "summary-mode-line init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:case-summary-buffer
          (bts-test:scenario-summary-open))
    (setq bts-test:case-project-name
          (bts:project-get-config-name (bts:query-get-project (bts:query-get-config-by-id 1))))
    (setq bts-test:case-query-name
          (bts:query-get-config-name (bts:query-get-config-by-id 1)))
    (setq bts-test:case-query-description
          (bts:query-get-description (bts:query-get-config-by-id 1) :short t))
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  (desc "summary-mode-line no warn")
  (expect ""
    (nth 4 mode-line-format))
  
  (desc "summary-mode-line finished")
  (expect ""
    (nth 5 mode-line-format))
  
  (desc "summary-mode-line no marked")
  (expect (format "0/%i" (length bts-test:scenario-tickets))
    (nth 7 mode-line-format))

  (desc "summary-mode-line 1 query")
  (expect bts-test:case-query-description
    (nth 9 mode-line-format))
  
  (desc "summary-mode-line 1 marked")
  (expect (format "1/%i" (length bts-test:scenario-tickets))
    (bts:summary-mark-ticket)
    (nth 7 mode-line-format))

  (desc "summary-mode-line all marked")
  (expect (format "%i/%i" (length bts-test:scenario-tickets) (length bts-test:scenario-tickets))
    (bts:summary-mark-all-tickets)
    (nth 7 mode-line-format))

  (desc "summary-mode-line 1 unmarked")
  (expect (format "%i/%i" (1- (length bts-test:scenario-tickets)) (length bts-test:scenario-tickets))
    (goto-char (point-min))
    (bts:summary-unmark-ticket)
    (nth 7 mode-line-format))

  (desc "summary-mode-line all marked")
  (expect (format "0/%i" (length bts-test:scenario-tickets))
    (bts:summary-unmark-all-tickets)
    (nth 7 mode-line-format))

  (desc "summary-mode-line reflection of project update")
  (expect (replace-regexp-in-string bts-test:case-project-name
                                    "*upd-proj*"
                                    bts-test:case-query-description)
    (bts-test:with-yaxception "main"
      (bts:project-update (bts:project-get-config-by-id 1))
      (bts-test:widget-edit 'bts-name 'update "*upd-proj*")
      (call-interactively 'bts:widget-submit)
      (switch-to-buffer bts-test:case-summary-buffer)
      (call-interactively 'bts:summary-reload-all)
      (setq bts-test:case-query-description (nth 9 mode-line-format))))

  (desc "summary-mode-line reflection of query update")
  (expect (replace-regexp-in-string bts-test:case-query-name
                                    "*upd-query*"
                                    bts-test:case-query-description)
    (bts-test:with-yaxception "main"
      (bts:query-update (bts:query-get-config-by-id 1))
      (bts-test:widget-edit 'bts-name 'update "*upd-query*")
      (call-interactively 'bts:widget-submit)
      (switch-to-buffer bts-test:case-summary-buffer)
      (call-interactively 'bts:summary-reload-all)
      (nth 9 mode-line-format)))

  (desc "summary-mode-line warn")
  (expect " *Warning exists*"
    (bts-test:with-yaxception "main"
      (bts-test:scenario-system-update
       :ticket-register (lambda (p tic diff)
                          (error "Something is bad!")))
      (goto-char (point-min))
      (call-interactively 'bts:summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)
      (switch-to-buffer bts-test:case-summary-buffer)
      (nth 4 mode-line-format)))

  (desc "summary-mode-line warn cleared by regist")
  (expect ""
    (bts-test:with-yaxception "main"
      (bts-test:scenario-system-update)
      (goto-char (point-min))
      (call-interactively 'bts:summary-view-ticket)
    (bts-test:start-logging)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)
      (switch-to-buffer bts-test:case-summary-buffer)
      (nth 4 mode-line-format)))

  (desc "summary-mode-line warn")
  (expect " *Warning exists*"
    (bts-test:with-yaxception "main"
      (bts-test:scenario-system-update
       :ticket-register (lambda (p tic diff)
                          (error "Something is bad!")))
      (goto-char (point-min))
      (call-interactively 'bts:summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)
      (switch-to-buffer bts-test:case-summary-buffer)
      (nth 4 mode-line-format)))

  (desc "summary-mode-line warn cleared by reload")
  (expect ""
    (bts-test:with-yaxception "main"
      (bts-test:scenario-system-update
       :ticket-latest bts-test:default-scenario-ticket-latest)
      (goto-char (point-min))
      (call-interactively 'bts:summary-reload-ticket)
      (nth 4 mode-line-format)))

  )

