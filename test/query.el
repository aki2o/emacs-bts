(require 'bts-test)

(expectations
  
  (desc "query no project")
  (expect "Any project is nothing"
    (bts-test:scenario-clear)
    (yaxception:$
      (yaxception:try
        (call-interactively 'bts:query-new)
        nil)
      (yaxception:catch 'error e
        (yaxception:get-text e))))
  
  (desc "query open new view")
  (expect bts::default-query-view-buffer-name
    (bts-test:scenario-project-setup)
    (call-interactively 'bts:query-new)
    (buffer-name))
  (expect ""
    (bts:widget-get-value 'bts-query))
  (expect nil
    (bts:widget-get-value 'state))

  (desc "query quit new view")
  (expect nil
    (bts-test:widget-edit 'bts-query 'insert "*new-query*")
    (bts-test:widget-edit 'state 'update 'open)
    (call-interactively 'bts:widget-cancel)
    (get-buffer bts::default-query-view-buffer-name))
  (expect 0
    (length (bts:query-get-config-all)))

  (desc "query submit new view")
  (expect nil
    (call-interactively 'bts:query-new)
    (bts-test:widget-edit 'bts-query 'insert "*new-query*")
    (bts-test:widget-edit 'state 'update 'open)
    (call-interactively 'bts:widget-submit)
    (get-buffer bts::default-query-view-buffer-name))
  (expect 1
    (length (bts:query-get-config-all)))

  (desc "query open update view")
  (expect bts::default-query-view-buffer-name
    (setq bts-test:dummy-result-query-select (bts:query-get-config-by-id 1))
    (call-interactively 'bts:query-update)
    (buffer-name))
  (expect "*new-query*"
    (bts:widget-get-value 'bts-query))
  (expect 'open
    (bts:widget-get-value 'state))
  
  (desc "query quit update view")
  (expect nil
    (bts-test:widget-edit 'state 'update 'closed)
    (call-interactively 'bts:widget-cancel)
    (get-buffer bts::default-query-view-buffer-name))
  (expect 'open
    (bts:query-update (bts:query-get-config-by-id 1))
    (bts:widget-get-value 'state))

  (desc "query submit update view")
  (expect nil
    (bts-test:widget-edit 'state 'update 'closed)
    (call-interactively 'bts:widget-submit)
    (get-buffer bts::default-query-view-buffer-name))
  (expect 'closed
    (bts:query-update (bts:query-get-config-by-id 1))
    (bts:widget-get-value 'state))

  (desc "query remove")
  (expect 0
    (call-interactively 'bts:widget-cancel)
    (bts:query-remove (bts:query-get-config-by-id 1))
    (length (bts:query-get-config-all)))

  (desc "query update in no query")
  (expect "Any query is nothing"
    (yaxception:$
      (yaxception:try
        (setq bts-test:dummy-result-query-select nil)
        (call-interactively 'bts:query-update)
        nil)
      (yaxception:catch 'error e
        (yaxception:get-text e))))

  (desc "query scenario setup")
  (expect t
    (bts-test:scenario-var-init)
    (bts-test:scenario-query-setup)
    (> (length (bts:query-get-config-all)) 1))

  (desc "query remove all")
  (expect 0
    (call-interactively 'bts:query-remove-all)
    (length (bts:query-get-config-all)))

  (desc "query reflection of project remove")
  (expect t
    (bts-test:scenario-var-init)
    (bts-test:scenario-query-setup)
    (let (belonged-query-ids etc-query-ids)
      (loop for q in (bts:query-get-config-all)
            if (string= (plist-get q :bts-project) "1")
            collect (plist-get q :bts-id) into belonged-query-ids
            else
            collect (plist-get q :bts-id) into etc-query-ids)
      (bts:project-remove (bts:project-get-config-by-id 1))
      (and (loop for id in belonged-query-ids
                 never (bts:query-get-config-by-id id))
           (loop for id in etc-query-ids
                 always (bts:query-get-config-by-id id)))))
  
  )

