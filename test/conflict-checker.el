(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)

(defun bts-test:case-do-summary-view-ticket ()
  (when (not (equal (current-buffer) bts-test:case-summary-buffer))
    (switch-to-buffer bts-test:case-summary-buffer))
  (goto-char (point-min))
  (call-interactively 'bts:summary-view-ticket))

(expectations

  (desc "conflict-checker init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:scenario-ticket-latest
          (lambda (p tic)
            (let ((ctic (copy-sequence tic)))
              (plist-put ctic :name "zzzz")
              (plist-put ctic :company "Microsoft")
              ctic)))
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (bts-test:scenario-ticket-regist)
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  ;; In default, call `bts::ticket-chk-diff-conflicts' to check conflict.
  ;; The function checks about only user update properties.
  (desc "conflict-checker value about diff")
  (expect (mock (bts:ticket-resolve-conflict * '((name . ("*upd*aaaa" . "zzzz")))))
    (setq bts-test:scenario-resolve-conflict-answers '("q"))
    (bts-test:with-yaxception "main"
      (bts-test:case-do-summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)))

  ;; If conflict-checker option is 'any', call `bts::ticket-chk-any-conflicts' to check conflict.
  ;; The function checks about all properties.
  (desc "conflict-checker value about any")
  (expect (mock (bts:ticket-resolve-conflict * '((name . ("*upd*aaaa" . "zzzz"))
                                                 (company . ("Google" . "Microsoft")))))
    (bts-test:scenario-system-update :conflict-checker 'any)
    (setq bts-test:scenario-resolve-conflict-answers '("q"))
    (bts-test:with-yaxception "main"
      (bts-test:case-do-summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)))

  (desc "conflict-checker called for diff")
  (expect (mock (bts::ticket-chk-diff-conflicts *))
    (bts-test:scenario-system-update)
    (setq bts-test:scenario-resolve-conflict-answers '("q"))
    (bts-test:with-yaxception "main"
      (bts-test:case-do-summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)))

  (desc "conflict-checker called for any")
  (expect (mock (bts::ticket-chk-any-conflicts *))
    (bts-test:scenario-system-update :conflict-checker 'any)
    (setq bts-test:scenario-resolve-conflict-answers '("q"))
    (bts-test:with-yaxception "main"
      (bts-test:case-do-summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)))

  (desc "conflict-checker called for custom")
  (expect t
    (let ((custom-called nil))
      (bts-test:scenario-system-update :conflict-checker (lambda (p btic tic)
                                                           (setq custom-called t)))
      (setq bts-test:scenario-resolve-conflict-answers '("q"))
      (bts-test:with-yaxception "main"
        (bts-test:case-do-summary-view-ticket)
        (bts-test:widget-edit 'name 'insert "*upd*")
        (call-interactively 'bts:widget-submit))
      custom-called))

)
