(require 'bts-test)

(defvar bts-test:case-summary-buffer nil)
(defvar bts-test:case-first-ticket nil)
(defvar bts-test:case-second-ticket nil)
(defvar bts-test:case-third-ticket nil)
(defvar bts-test:case-register-called 0)

(expectations

  (desc "multi-view init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:scenario-ticket-multi-view
          bts-test:default-scenario-ticket-multi-view)
    (setq bts-test:scenario-ticket-register
          (lambda (p tic diff)
            (incf bts-test:case-register-called)
            tic))
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (setq bts-test:case-register-called 0)
    (bts-test:scenario-ticket-regist)
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  (desc "multi-view called")
  (expect (mock (bts::ticket-open-system-multi-view *))
    (bts-test:with-yaxception "main"
      (goto-char (point-min))
      (setq bts-test:case-first-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-mark-ticket)
      (setq bts-test:case-second-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-mark-ticket)
      (setq bts-test:case-third-ticket (copy-sequence (bts::summary-get-ticket-at)))
      (call-interactively 'bts:summary-view-ticket)))

  (desc "multi-view open view")
  (expect bts::default-ticket-view-buffer-name
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (buffer-name)))

  (desc "multi-view quit")
  (expect nil
    (bts-test:with-yaxception "main"
      (bts-test:widget-edit 'name 'update "zzzz")
      (call-interactively 'bts:widget-cancel)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "multi-view summary mark remained after quit")
  (expect t
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (and (progn (goto-char (point-min))
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at)))))
  
  (desc "multi-view summary not changed")
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

  (desc "multi-view submit with no edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (call-interactively 'bts:widget-submit)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "multi-view register not called")
  (expect 0
    bts-test:case-register-called)

  (desc "multi-view summary mark remained after skip")
  (expect t
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (and (progn (goto-char (point-min))
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at)))))

  (desc "multi-view submit with edit")
  (expect nil
    (bts-test:with-yaxception "main"
      (call-interactively 'bts:summary-view-ticket)
      (bts-test:widget-edit 'name 'update "zzzz")
      (call-interactively 'bts:widget-submit)
      (get-buffer bts::default-ticket-view-buffer-name)))

  (desc "multi-view register called")
  (expect 2
    bts-test:case-register-called)

  (desc "multi-view summary mark remained after regist")
  (expect t
    (bts-test:with-yaxception "main"
      (switch-to-buffer bts-test:case-summary-buffer)
      (and (progn (goto-char (point-min))
                  (bts::summary-marked-at))
           (progn (forward-line 1)
                  (bts::summary-marked-at)))))

  (desc "multi-view summary changed")
  (expect t
    (bts-test:with-yaxception "main"
      (let* ((name1 (progn (goto-char (point-min))
                           (bts-test:summary-get-column-value 'name)))
             (name2 (progn (forward-line 1)
                           (bts-test:summary-get-column-value 'name)))
             (name3 (progn (forward-line 1)
                           (bts-test:summary-get-column-value 'name))))
        (or (and (string= name1 "zzzz")
                 (string= name2 "zzzz")
                 (string= name3 (plist-get bts-test:case-third-ticket :name)))
            (format "1[%s] 2[%s] 3[%s]" name1 name2 name3)))))

  )
