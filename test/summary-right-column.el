(require 'bts-test)

(defvar bts-test:case-summary-labels nil)

(expectations
  
  (desc "summary-right-column init scenario")
  (expect bts::default-summary-buffer-name
    (bts-test:scenario-clear)
    (setq bts-test:case-summary-buffer (bts-test:scenario-summary-open))
    (setq bts-test:case-summary-labels
          (loop for e in bts-test:scenario-summary-format-result
                collect (plist-get e :label)))
    (switch-to-buffer bts-test:case-summary-buffer)
    (buffer-name))

  (desc "summary-right-column move to first column from point-at-bol")
  (expect (nth 0 bts-test:case-summary-labels)
    (bts-test:with-yaxception "main"
      (goto-char (point-min))
      (bts:summary-right-column)
      (bts::summary-get-column-label-at)))

  (desc "summary-right-column move to next column from a column")
  (expect (nth 1 bts-test:case-summary-labels)
    (bts-test:with-yaxception "main"
      (bts:summary-right-column)
      (bts::summary-get-column-label-at)))

  (desc "summary-right-column check prepare last column name")
  (expect (nth (1- (length bts-test:case-summary-labels)) bts-test:case-summary-labels)
    (bts-test:with-yaxception "main"
      (goto-char (point-at-bol))
      (loop for e in bts-test:case-summary-labels
            do (bts:summary-right-column))
      (bts::summary-get-column-label-at)))

  (desc "summary-right-column check prepare last column point")
  (expect t
    (< (point) (point-at-eol)))
  
  (desc "summary-right-column move to point-at-eol from last column")
  (expect (point-at-eol)
    (bts-test:with-yaxception "main"
      (bts:summary-right-column)
      (point)))

  )

