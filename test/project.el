(require 'bts-test)

(expectations

  (desc "project no system")
  (expect "Any system type is nothing"
    (bts-test:scenario-clear)
    (yaxception:$
      (yaxception:try
        (call-interactively 'bts:project-new)
        nil)
      (yaxception:catch 'error e
        (yaxception:get-text e))))
  
  (desc "project open new view")
  (expect bts::default-project-view-buffer-name
    (bts-test:scenario-system-setup)
    (call-interactively 'bts:project-new)
    (buffer-name))
  (expect ""
    (bts:widget-get-value 'bts-project))
  (expect ""
    (bts:widget-get-value 'username))

  (desc "project quit new view")
  (expect nil
    (bts-test:widget-edit 'bts-project 'insert "*new-proj*")
    (bts-test:widget-edit 'username 'insert "Ichiro")
    (call-interactively 'bts:widget-cancel)
    (get-buffer bts::default-project-view-buffer-name))
  (expect 0
    (length (bts:project-get-config-all)))

  (desc "project submit new view")
  (expect nil
    (call-interactively 'bts:project-new)
    (bts-test:widget-edit 'bts-project 'insert "*new-proj*")
    (bts-test:widget-edit 'username 'insert "Ichiro")
    (call-interactively 'bts:widget-submit)
    (get-buffer bts::default-project-view-buffer-name))
  (expect 1
    (length (bts:project-get-config-all)))

  (desc "project open update view")
  (expect bts::default-project-view-buffer-name
    (setq bts-test:dummy-result-project-select (bts:project-get-config-by-id 1))
    (call-interactively 'bts:project-update)
    (buffer-name))
  (expect "*new-proj*"
    (bts:widget-get-value 'bts-project))
  (expect "Ichiro"
    (bts:widget-get-value 'username))
  
  (desc "project quit update view")
  (expect nil
    (bts-test:widget-edit 'username 'update "Jiro")
    (call-interactively 'bts:widget-cancel)
    (get-buffer bts::default-project-view-buffer-name))
  (expect "Ichiro"
    (bts:project-update (bts:project-get-config-by-id 1))
    (bts:widget-get-value 'username))

  (desc "project submit update view")
  (expect nil
    (bts-test:widget-edit 'username 'update "Jiro")
    (call-interactively 'bts:widget-submit)
    (get-buffer bts::default-project-view-buffer-name))
  (expect "Jiro"
    (bts:project-update (bts:project-get-config-by-id 1))
    (bts:widget-get-value 'username))

  (desc "project remove")
  (expect 0
    (call-interactively 'bts:widget-cancel)
    (bts:project-remove (bts:project-get-config-by-id 1))
    (length (bts:project-get-config-all)))

  (desc "project update in no project")
  (expect "Any project is nothing"
    (yaxception:$
      (yaxception:try
        (setq bts-test:dummy-result-project-select nil)
        (call-interactively 'bts:project-update)
        nil)
      (yaxception:catch 'error e
        (yaxception:get-text e))))

  (desc "project scenario setup")
  (expect t
    (bts-test:scenario-var-init)
    (bts-test:scenario-project-setup)
    (> (length (bts:project-get-config-all)) 1))
  
  (desc "project remove all")
  (expect 0
    (call-interactively 'bts:project-remove-all)
    (length (bts:project-get-config-all)))

  )

