(require 'bts)
(require 'tenv)
(require 'el-expectations)

;;;;;;;;;;;;;;;;;;;;;;
;; Test Environment

(defvar bts-test::root-directory (tenv-get-tmp-directory "bts" t t))
(setq current-language-environment "English")
(setq bts:project-cache-file (concat bts-test::root-directory "/.bts-project"))
(setq bts:query-cache-file (concat bts-test::root-directory "/.bts-query"))

(defvar bts-test:quiet nil)

(defadvice expectations-execute (before bts-test:message-disable activate)
  (setq bts-test:quiet t))

(defadvice message (before bts-test:quiet activate)
  (when bts-test:quiet
    (ad-set-args 0 '(""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scenario Environment

(defmacro bts-test:scenario-var-init ()
  `(progn

     (makunbound 'bts-test:scenario-projects)
     (defvar bts-test:scenario-projects
       '(("myproj" . (:username "Taro"))
         ("joined" . (:username "Hanako"))))

     (makunbound 'bts-test:scenario-queries)
     (defvar bts-test:scenario-queries
       '(("myproj" . (("all"  . (:state all))))
         ("joined" . (("all"  . (:state all))))
         ("myproj" . (("main" . (:state open))))
         ("myproj" . (("old"  . (:state closed))))))

     (makunbound 'bts-test:scenario-tickets)
     (defvar bts-test:scenario-tickets
       '((:id "1111" :name "aaaa" :company "Google" :countries (japan america))
         (:id "2222" :name "bbbb" :company "Baidu"  :countries (china))
         (:id "3333" :name "cccc" :company "Yahoo"  :countries (japan))
         (:id "4444" :name "dddd" :company "Google" :countries (japan))
         (:id "5555" :name "eeee" :company "Yahoo"  :countries (america))))

     (makunbound 'bts-test:scenario-project-view)
     (defvar bts-test:scenario-project-view
       (lambda (p)
         `(:layout ((:type text :name username :label "UserName"))
                   :defaults ,(when p (bts:make-alist-from-plist p)))))

     (makunbound 'bts-test:scenario-query-view)
     (defvar bts-test:scenario-query-view
       (lambda (p q)
         `(:layout ((:type select :name state :label "Status" :options (all open closed)))
                   :defaults ,(when q (bts:make-alist-from-plist q)))))

     (makunbound 'bts-test:scenario-ticket-single-view)
     (defvar bts-test:scenario-ticket-single-view
       (lambda (p tic)
         `(:layout ((:type const :name id :label "ID")
                    BR (:type text :name name :label "Name" :size 30 :require t)
                    BR (:type text :name company :label "Company" :size 30)
                    BR (:type select :name countries :label "Countries" :require t :multiple t
                              :options (japan america china)))
                   :defaults ,(if tic
                                  (bts:make-alist-from-plist tic)
                                '((id        . "*new*")
                                  (countries . (japan)))))))

     (makunbound 'bts-test:scenario-ticket-multi-view)
     (defvar bts-test:scenario-ticket-multi-view
       nil)

     (makunbound 'bts-test:default-scenario-ticket-multi-view)
     (defvar bts-test:default-scenario-ticket-multi-view
       (lambda (p tic)
         `(:layout ((:type text :name name :label "Name" :size 30)
                    BR (:type text :name company :label "Company" :size 30)))))

     (makunbound 'bts-test:scenario-ticket-fetcher)
     (defvar bts-test:scenario-ticket-fetcher
       (lambda (p q)
         (bts:ticket-fetch-complete q bts-test:scenario-tickets)))

     (makunbound 'bts-test:scenario-ticket-register)
     (defvar bts-test:scenario-ticket-register
       (lambda (p tic diff)
         tic))

     (makunbound 'bts-test:scenario-summary-format-result)
     (defvar bts-test:scenario-summary-format-result
       '((:name id :label "ID" :size 5 :sort t)
         (:name name :label "Name" :size 20 :sort t)
         (:name company :label "Company" :size 20 :sort t)
         (:name countries :label "Countries" :size 30)))

     (makunbound 'bts-test:scenario-ticket-unique-string)
     (defvar bts-test:scenario-ticket-unique-string
       (lambda (tic)
         (plist-get tic :id)))

     (makunbound 'bts-test:scenario-ticket-latest)
     (defvar bts-test:scenario-ticket-latest
       nil)

     (makunbound 'bts-test:default-scenario-ticket-latest)
     (defvar bts-test:default-scenario-ticket-latest
       (lambda (p tic)
         (-find (lambda (e)
                  (string= (plist-get e :id) (or (plist-get tic :id) "")))
                bts-test:scenario-tickets)))

     (makunbound 'bts-test:scenario-conflict-checker)
     (defvar bts-test:scenario-conflict-checker
       nil)

     (makunbound 'bts-test:scenario-conflict-resolver)
     (defvar bts-test:scenario-conflict-resolver
       nil)

     (makunbound 'bts-test:scenario-resolve-conflict-answers)
     (defvar bts-test:scenario-resolve-conflict-answers
       nil)

     ))
(bts-test:scenario-var-init)


;;;;;;;;;;;;;
;; Utility

(defun bts-test:start-logging ()
  (bts--log-clear-log)
  (bts--log-enable-logging)
  (bts--log-set-level 'trace)
  (bts--log-enable-messaging)
  (setq bts-test:quiet nil))

(defun bts-test:print-log ()
  (princ (with-current-buffer (bts--log-open-log)
           (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro bts-test:with-yaxception (name &rest body)
  (declare (indent 1))
  `(yaxception:$
     (yaxception:try
       (bts--log-enable-logging)
       (bts--log-set-level 'trace)
       ,@body)
     (yaxception:catch 'error e
       (bts--error ,(concat "failed " name " : %s\n%s")
                   (yaxception:get-text e)
                   (yaxception:get-stack-trace-string e))
       (bts-test:print-log)
       (yaxception:throw e))))

(defmacro bts-test::def-non-interactive (func)
  "Replace user interactive function.
You are able to get result without interactive process.
The result is variable named \"bts-test:\" + non prefix part of the function + \"-result\"."
  (let* ((non-prefix-part (replace-regexp-in-string "\\`bts:+" "" (symbol-name func)))
         (varnm (concat "bts-test:dummy-result-" non-prefix-part)))
    `(progn
       (defvar ,(intern varnm) nil)
       (defadvice ,func (around bts-test:non-interactive activate)
         (if (not ,(intern varnm))
             ad-do-it
           (bts--trace ,(concat "test advice " (symbol-name func) " non-interactive set return %s")
                       ,(intern varnm))
           (setq ad-return-value ,(intern varnm)))))))

(defmacro bts-test::def-yaxception-function (func args &rest body)
  (declare (indent 2))
  `(defun ,(intern (concat "bts-test:" (symbol-name func))) ,args
     (bts-test:with-yaxception ,(concat "bts-test:" (symbol-name func))
       ,@body)))

(defun bts-test:widget-edit (name type value)
  "Edit NAME widget in current buffer
TYPE is one of insert, update"
  (bts:widget-goto name)
  (let* ((w (widget-at))
         (v (widget-value w))
         (wtype (widget-type w)))
    (case wtype
      ((editable-field text)
       (case type
         (insert (insert value))
         (update (delete-region (widget-get w :from) (widget-get w :to))
                 (insert value))
         (t      (yaxception:throw 'bts:invalid-argument))))
      ((menu-choice radio-button-choice)
       (case type
         (update (widget-value-set w value))
         (t      (yaxception:throw 'bts:invalid-argument))))
      (t
       (yaxception:throw 'bts:invalid-argument)))))

(defun bts-test:summary-get-column-value (name)
  (let* ((label (loop for e in bts-test:scenario-summary-format-result
                      if (eq (plist-get e :name) name)
                      return (plist-get e :label)))
         (begin (bts::summary-get-column-start-by-label label))
         (end (progn (goto-char begin)
                     (bts:summary-right-column)
                     (point))))
    (s-trim (buffer-substring-no-properties begin end))))


;;;;;;;;;;;;;;;;;;;;
;; Setup Scenario

(defvar bts-test::scenario-p nil)

(bts-test::def-non-interactive y-or-n-p)
(bts-test::def-non-interactive completing-read)
(bts-test::def-non-interactive read-string)

;; System
(bts-test::def-non-interactive bts::system-select)

(defun bts-test:scenario-system-clear ()
  (setq bts::system-hash (make-hash-table))
  (setq bts-test:dummy-result-y-or-n-p nil)
  (setq bts-test:dummy-result-system-select nil))


(defun* bts-test:scenario-system-update (&key project-view query-view
                                              ticket-single-view ticket-multi-view
                                              ticket-fetcher ticket-register
                                              summary-format ticket-unique-string
                                              ticket-latest conflict-checker conflict-resolver)
  (bts:system-regist
   (make-bts:system
    :name                 'test
    :project-view         (or project-view         bts-test:scenario-project-view)
    :query-view           (or query-view           bts-test:scenario-query-view)
    :ticket-single-view   (or ticket-single-view   bts-test:scenario-ticket-single-view)
    :ticket-multi-view    (or ticket-multi-view    bts-test:scenario-ticket-multi-view)
    :ticket-fetcher       (or ticket-fetcher       bts-test:scenario-ticket-fetcher)
    :ticket-register      (or ticket-register      bts-test:scenario-ticket-register)
    :ticket-unique-string (or ticket-unique-string bts-test:scenario-ticket-unique-string)
    :ticket-latest        (or ticket-latest        bts-test:scenario-ticket-latest)
    :summary-format       (or summary-format       (lambda (p q) bts-test:scenario-summary-format-result))
    :conflict-checker     (or conflict-checker     bts-test:scenario-conflict-checker)
    :conflict-resolver    (or conflict-resolver    bts-test:scenario-conflict-resolver))))

(bts-test::def-yaxception-function scenario-system-setup ()
  (setq bts-test::scenario-p t)
  (bts-test:scenario-system-clear)
  (bts-test:scenario-system-update)
  (setq bts-test:dummy-result-y-or-n-p t)
  (setq bts-test:dummy-result-system-select (bts:system-get 'test)))

;; Project
(bts-test::def-non-interactive bts::project-select)

(defun bts-test:scenario-project-clear ()
  (bts::plstore-clear 'bts::project-store)
  (when (file-exists-p bts:project-cache-file)
    (delete-file bts:project-cache-file))
  (setq bts-test:dummy-result-project-select nil))

(bts-test::def-yaxception-function scenario-project-setup ()
  (bts-test:scenario-system-setup)
  (bts-test:scenario-project-clear)
  (loop for e in bts-test:scenario-projects
        for name = (car e)
        for prop = (cdr e)
        do (bts:project-store-config nil name 'test prop))
  (setq bts-test:dummy-result-project-select (bts:project-get-config-by-id 1)))

;; Query
(bts-test::def-non-interactive bts::query-select)

(defun bts-test:scenario-query-clear ()
  (bts::plstore-clear 'bts::query-store)
  (when (file-exists-p bts:query-cache-file)
    (delete-file bts:query-cache-file))
  (setq bts-test:dummy-result-query-select nil))

(bts-test::def-yaxception-function scenario-query-setup ()
  (bts-test:scenario-project-setup)
  (bts-test:scenario-query-clear)
  (loop for e in bts-test:scenario-queries
        for projnm = (car e)
        for qpairs = (cdr e)
        do (loop for pair in qpairs
                 for querynm = (car pair)
                 for prop = (cdr pair)
                 for proj = (bts:project-get-config-by-name projnm 'test)
                 do (bts:query-store-config nil proj querynm prop)))
  (setq bts-test:dummy-result-query-select (bts:query-get-config-by-id 1)))

;; Summary
(defun bts-test::ticket-fetch (queries callback)
  (bts::ticket-fetch-init-result)
  (dolist (q (if (bts:query-p queries)
                 (list queries)
               queries))
    (let* ((q (copy-sequence q))
           (proj (copy-sequence (bts:query-get-project q)))
           (sys (bts:project-get-system proj))
           (fetchkey (bts::ticket-fetch-make-fetch-key q)))
      (puthash fetchkey 'running bts::ticket-fetch-result-hash)
      (funcall (bts:system-ticket-fetcher sys) proj q)))
  (let ((tickets (loop for v being the hash-values in bts::ticket-fetch-result-hash append v)))
    (funcall callback tickets t)))

(defadvice bts:ticket-fetch (around bts-test:synchronously activate)
  (if (not bts-test::scenario-p)
      ad-do-it
    (bts-test::ticket-fetch (ad-get-arg 0) (ad-get-arg 1))))

(defun bts-test:scenario-summary-clear ()
  (mapc 'kill-buffer (bts::summary-buffers)))

(bts-test::def-yaxception-function scenario-summary-open (&optional queries)
  (bts-test:scenario-query-setup)
  (bts:summary-open (or queries
                        (list (bts:query-get-config-by-id 1))))
  (current-buffer))

;; Ticket
(defun bts-test:scenario-ticket-clear ()
  (setq bts-test:dummy-result-read-string nil)
  (setq bts-test:scenario-resolve-conflict-answers nil))

(bts-test::def-yaxception-function scenario-ticket-regist ()
  (setq bts-test:dummy-result-read-string "y")
  (defadvice read-string (around bts-test:resolve-conflict-answers activate)
    (bts--trace "test advice read-string resolve-conflict-answers current answers : %s"
                bts-test:scenario-resolve-conflict-answers)
    (let ((bts-test:dummy-result-read-string (if bts-test:scenario-resolve-conflict-answers
                                                 (pop bts-test:scenario-resolve-conflict-answers)
                                               bts-test:dummy-result-read-string)))
      ad-do-it)))


(defun bts-test:scenario-clear ()
  (bts-test:scenario-ticket-clear)
  (bts-test:scenario-summary-clear)
  (bts-test:scenario-query-clear)
  (bts-test:scenario-project-clear)
  (bts-test:scenario-system-clear)
  (setq bts-test::scenario-p nil)
  (bts-test:scenario-var-init))


(provide 'bts-test)
;;; bts-test.el ends here
