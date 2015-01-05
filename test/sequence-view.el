(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)
(defvar bts-test:case-first-ticket nil)
(defvar bts-test:case-second-ticket nil)
(defvar bts-test:case-third-ticket nil)
(defvar bts-test:case-register-called 0)

(expectations

  (desc "sequence-view init scenario")
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

  (desc "sequence-view called")
  (expect (mock (bts::ticket-open-sequence-view *))
    (bts-test:with-yaxception "main"
      (goto-char (point-min))
      (setq bts-test:case-first-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-mark-ticket)
      (setq bts-test:case-second-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-mark-ticket)
      (setq bts-test:case-third-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-mark-ticket)
      (call-interactively 'bts:summary-view-ticket)))

  (desc "sequence-view open view")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (buffer-name)))

  (desc "sequence-view first ticket opened")
  (expect (plist-get bts-test:case-first-ticket :name)
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view submit on first ticket with edit")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (bts-test:widget-edit 'name 'insert "*upd1*")
      (call-interactively 'bts:widget-submit)
      (buffer-name)))

  (desc "sequence-view second ticket opened")
  (expect (plist-get bts-test:case-second-ticket :name)
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view submit on second ticket with edit")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (bts-test:widget-edit 'name 'insert "*upd2*")
      (call-interactively 'bts:widget-submit)
      (buffer-name)))

  (desc "sequence-view third ticket opened")
  (expect (plist-get bts-test:case-third-ticket :name)
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view cancel on third ticket with edit")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (bts-test:widget-edit 'name 'insert "*upd3*")
      (call-interactively 'bts:widget-cancel)
      (buffer-name)))

  (desc "sequence-view second ticket opened edited")
  (expect (concat "*upd2*" (plist-get bts-test:case-second-ticket :name))
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view cancel on second ticket")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:widget-cancel)
      (buffer-name)))

  (desc "sequence-view first ticket opened edited")
  (expect (concat "*upd1*" (plist-get bts-test:case-first-ticket :name))
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view re submit on first ticket")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:widget-submit)
      (buffer-name)))

  (desc "sequence-view second ticket opened")
  (expect (concat "*upd2*" (plist-get bts-test:case-second-ticket :name))
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view quit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:widget-cancel)
      (call-interactively 'bts:widget-cancel)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "sequence-view summary mark remained after quit")
  (expect t
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (and (progn (goto-char (point-min))
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at)))))

  (desc "sequence-view summary not changed")
  (expect t
    (bts-test:with-yaxception "main"
      (let* ((name1 (progn (goto-char (point-min))
                           (bts-test:summary-get-column-value 'name)))
             (name2 (progn (forward-line 1)
                           (bts-test:summary-get-column-value 'name)))
             (name3 (progn (forward-line 1)
                           (bts-test:summary-get-column-value 'name))))
        (or (and (string= name1 (plist-get bts-test:case-first-ticket :name))
                 (string= name2 (plist-get bts-test:case-second-ticket :name))
                 (string= name3 (plist-get bts-test:case-third-ticket :name)))
            (format "1[%s] 2[%s] 3[%s]" name1 name2 name3)))))

  (desc "sequence-view re open view")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (buffer-name)))

  (desc "sequence-view first ticket opened not updated")
  (expect (plist-get bts-test:case-first-ticket :name)
    (bts-test:with-yaxception "main"
      (bts:widget-get-value 'name)))

  (desc "sequence-view submit to second ticket not updated")
  (expect (plist-get bts-test:case-second-ticket :name)
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:widget-submit)
      (bts:widget-get-value 'name)))

  (desc "sequence-view submit to third ticket not updated")
  (expect (plist-get bts-test:case-third-ticket :name)
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:widget-submit)
      (bts:widget-get-value 'name)))

  (desc "sequence-view submit finally with no edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:widget-submit)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "sequence-view register not called")
  (expect 0
    bts-test:case-register-called)

  (desc "sequence-view summary mark remained after skip")
  (expect t
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (and (progn (goto-char (point-min))
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at)))))

  (desc "sequence-view submit finally with edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (bts-test:widget-edit 'name 'insert "*upd1*")
      (call-interactively 'bts:widget-submit)
      (bts-test:widget-edit 'name 'insert "*upd2*")
      (call-interactively 'bts:widget-submit)
      (bts-test:widget-edit 'name 'insert "*upd3*")
      (call-interactively 'bts:widget-submit)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "sequence-view register called")
  (expect 3
    bts-test:case-register-called)

  (desc "sequence-view summary mark remained after regist")
  (expect t
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (and (progn (goto-char (point-min))
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at)))))

  (desc "sequence-view summary changed")
  (expect t
    (bts-test:with-yaxception "main"
      (let* ((name1 (progn (goto-char (point-min))
                           (bts-test:summary-get-column-value 'name)))
             (name2 (progn (forward-line 1)
                           (bts-test:summary-get-column-value 'name)))
             (name3 (progn (forward-line 1)
                           (bts-test:summary-get-column-value 'name))))
        (or (and (string= name1 (concat "*upd1*" (plist-get bts-test:case-first-ticket :name)))
                 (string= name2 (concat "*upd2*" (plist-get bts-test:case-second-ticket :name)))
                 (string= name3 (concat "*upd3*" (plist-get bts-test:case-third-ticket :name))))
            (format "1[%s] 2[%s] 3[%s]" name1 name2 name3)))))

  )

