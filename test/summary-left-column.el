(require 'bts-test)

(defvar bts-test:case-summary-labels nil)

(expectations
  
  (desc "summary-left-column init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (setq bts-test:case-summary-labels
          (loop for e in bts-test:scenario-summary-format-result
                collect (plist-get e :label)))
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  (desc "summary-left-column move to last column from point-at-eol")
  (expect (nth (1- (length bts-test:case-summary-labels)) bts-test:case-summary-labels)
    (bts-test:with-yaxception "main"
      (goto-char (point-min))
      (goto-char (point-at-eol))
      (bts:summary-left-column)
      (bts::summary-get-column-label-at)))

  (desc "summary-left-column move to previous column from a column")
  (expect (nth (- (length bts-test:case-summary-labels) 2) bts-test:case-summary-labels)
    (bts-test:with-yaxception "main"
      (bts:summary-left-column)
      (bts::summary-get-column-label-at)))

  (desc "summary-left-column check prepare first column name")
  (expect (nth 0 bts-test:case-summary-labels)
    (bts-test:with-yaxception "main"
      (goto-char (point-at-eol))
      (loop for e in bts-test:case-summary-labels
            do (bts:summary-left-column))
      (bts::summary-get-column-label-at)))

  (desc "summary-left-column check prepare first column point")
  (expect t
    (> (point) (point-at-bol)))
  
  (desc "summary-left-column stay at point from first column")
  (expect t
    (bts-test:with-yaxception "main"
      (= (point)
         (progn (bts:summary-left-column)
                (point)))))

  )

