(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)

(defun bts-test:case-do-summary-view-ticket ()
  (when (not (equal (current-buffer) bts-test:case-summary-buffer))
    (switch-to-buffer bts-test:case-summary-buffer))
  (goto-char (point-min))
  (call-interactively 'bts:summary-view-ticket))

(expectations

  (desc "resolve-conflict init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (bts-test:scenario-ticket-regist)
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  (desc "resolve-conflict called")
  (expect (mock (bts:ticket-resolve-conflict *))
    (bts-test:scenario-system-update
     :ticket-latest (lambda (p tic)
                      (let ((ctic (copy-sequence tic)))
                        (plist-put ctic :name "zzzz")
                        ctic)))
    (setq bts-test:scenario-resolve-conflict-answers '("q"))
    (bts-test:with-yaxception "main"
      (bts-test:case-do-summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd*")
      (call-interactively 'bts:widget-submit)))
  
  (desc "resolve-conflict quit")
  (expect t
    ;; Setting for conflict happens in two properties and then quit to regist
    (bts-test:scenario-system-update
     :ticket-latest (lambda (p tic)
                      (let ((ctic (copy-sequence tic)))
                        (plist-put ctic :name "zzzz")
                        (plist-put ctic :company "Microsoft")
                        ctic)))
    (setq bts-test:scenario-resolve-conflict-answers '("y" "q"))
    ;; Start
    (let ((before-name "")
          (before-company ""))
      (bts-test:with-yaxception "main"
        ;; Open ticket
        (bts-test:case-do-summary-view-ticket)
        ;; Get current value
        (setq before-name (bts:widget-get-value 'name))
        (setq before-company (bts:widget-get-value 'company))
        ;; Update and try to regist ticket
        (bts-test:widget-edit 'name 'insert "*upd*")
        (bts-test:widget-edit 'company 'insert "*upd*")
        (call-interactively 'bts:widget-submit)
        ;; After resolve conflict, ticket should be not changed
        (bts-test:case-do-summary-view-ticket)
        (or (and (string= (bts:widget-get-value 'name) before-name)
                 (string= (bts:widget-get-value 'company) before-company))
            (format "name[%s] company[%s]"
                    (bts:widget-get-value 'name)
                    (bts:widget-get-value 'company))))))

  (desc "resolve-conflict skip from all use latest")
  (expect t
    ;; Setting for conflict happens in two properties and then use latest value for all
    (bts-test:scenario-system-update
     :ticket-latest (lambda (p tic)
                      (let ((ctic (copy-sequence tic)))
                        (plist-put ctic :name "zzzz")
                        (plist-put ctic :company "Microsoft")
                        ctic)))
    (setq bts-test:scenario-resolve-conflict-answers '("n" "n"))
    ;; Start
    (let ((before-name "")
          (before-company ""))
      (bts-test:with-yaxception "main"
        ;; Open ticket
        (bts-test:case-do-summary-view-ticket)
        ;; Get current value
        (setq before-name (bts:widget-get-value 'name))
        (setq before-company (bts:widget-get-value 'company))
        ;; Update and try to regist ticket
        (bts-test:widget-edit 'name 'insert "*upd*")
        (bts-test:widget-edit 'company 'insert "*upd*")
        (call-interactively 'bts:widget-submit)
        ;; After resolve conflict, ticket should be not changed
        (bts-test:case-do-summary-view-ticket)
        (or (and (string= (bts:widget-get-value 'name) before-name)
                 (string= (bts:widget-get-value 'company) before-company))
            (format "name[%s] company[%s]"
                    (bts:widget-get-value 'name)
                    (bts:widget-get-value 'company))))))
  
  (desc "resolve-conflict regist only use local property")
  (expect t
    ;; Setting for conflict happens in two properties and then use local for one
    (bts-test:scenario-system-update
     :ticket-latest (lambda (p tic)
                      (let ((ctic (copy-sequence tic)))
                        (plist-put ctic :name "zzzz")
                        (plist-put ctic :company "Microsoft")
                        ctic)))
    (setq bts-test:scenario-resolve-conflict-answers '("y" "n"))
    ;; Start
    (let ((before-name "")
          (before-company ""))
      (bts-test:with-yaxception "main"
        ;; Open ticket
        (bts-test:case-do-summary-view-ticket)
        ;; Get current value
        (setq before-name (bts:widget-get-value 'name))
        (setq before-company (bts:widget-get-value 'company))
        ;; Update and try to regist ticket
        (bts-test:widget-edit 'name 'insert "*upd*")
        (bts-test:widget-edit 'company 'insert "*upd*")
        (call-interactively 'bts:widget-submit)
        ;; After resolve conflict, ticket should be changed
        (bts-test:case-do-summary-view-ticket)
        (or (and (string= (bts:widget-get-value 'name) (concat "*upd*" before-name))
                 (string= (bts:widget-get-value 'company) "Microsoft"))
            (format "name[%s] company[%s]"
                    (bts:widget-get-value 'name)
                    (bts:widget-get-value 'company))))))
  
  )

