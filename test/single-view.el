(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)
(defvar bts-test:case-ticket nil)
(defvar bts-test:case-register-called 0)

(expectations

  (desc "single-view init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:scenario-ticket-register
          (lambda (p tic diff)
            (incf bts-test:case-register-called)
            tic))
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (setq bts-test:case-register-called 0)
    (bts-test:scenario-ticket-regist)
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  (desc "single-view open view")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (goto-char (point-min))
      (setq bts-test:case-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-view-ticket)
      (buffer-name)))
  
  (desc "single-view ticket opened")
  (expect (plist-get bts-test:case-ticket :name)
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "single-view quit with edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-cancel)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "single-view summary not changed after quit")
  (expect (plist-get bts-test:case-ticket :name)
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (goto-char (point-min))
      (bts-test:summary-get-column-value 'name)))

  (desc "single-view submit with no edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (call-interactively 'bts:widget-submit)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "single-view register not called")
  (expect 0
    bts-test:case-register-called)

  (desc "single-view summary not changed after skip")
  (expect (plist-get bts-test:case-ticket :name)
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (goto-char (point-min))
      (bts-test:summary-get-column-value 'name)))

  (desc "single-view submit with edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "single-view register called")
  (expect 1
    bts-test:case-register-called)

  (desc "single-view summary not changed after skip")
  (expect (concat "*upd*" (plist-get bts-test:case-ticket :name))
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (goto-char (point-min))
      (bts-test:summary-get-column-value 'name)))

  )

