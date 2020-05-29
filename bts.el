;;; bts.el --- A unified UI for various bug tracking systems -*- coding: utf-8; -*-

;; Copyright (C) 2015  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aki2o/emacs-bts
;; Version: 0.1.0
;; Package-Requires: ((widget-mvc "0.0.2") (log4e "0.3.0") (yaxception "0.3.3") (dash "2.9.0") (s "1.9.0") (pos-tip "0.4.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension features are
;; - Easily and quickly edit ticket of various bug tracking systems using a unified widget interface
;; - List up ticket summaries with the combination of multiple conditions at one time
;; - Edit the listed multiple tickets at one time
;; 
;; You are able to create/update the tickets of a bug tracking system by the following steps.
;; 1. create projects for the system
;; 2. create queries belongs to the project
;; 3. List up the summaries of the fetched tickets from the queries
;; 4. Open details of the tickets, change the properties, and submit them.
;; 
;; * Project means a access configuration for the system stores target tickets data
;; * Query means a configuration detects the fetched tickets in the tickets belongs to the project
;; * For handling project, there are `bts:project-new'/`bts:project-update'
;; * For handling query, there are `bts:query-new'/`bts:query-update'
;; * For handling ticket, there are `bts:ticket-new'/`bts:summary-open'
;; * For key binding in widget buffer, see `bts:widget-common-keymap'
;; * For checking other functions, see API section below
;; * For use of this extension, it needs to install the system package (eg. bts-github is for GitHub).
;; 
;; For more infomation, see <https://github.com/aki2o/emacs-bts/blob/master/README.md>

;;; Dependencies:
;; 
;; - widget-mvc.el ( see <https://github.com/kiwanami/emacs-widget-mvc> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - dash.el ( see <https://github.com/magnars/dash.el> )
;; - s.el ( see <https://github.com/magnars/s.el> )
;; - pos-tip.el

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'bts)

;;; Configuration:
;;
;; ;; Key Binding
;; (global-unset-key (kbd "M-b"))
;; (global-set-key (kbd "M-b n")   'bts:ticket-new)
;; (global-set-key (kbd "M-b s")   'bts:summary-open)
;; (global-set-key (kbd "M-b p n") 'bts:project-new)
;; (global-set-key (kbd "M-b p u") 'bts:project-update)
;; (global-set-key (kbd "M-b p d") 'bts:project-remove)
;; (global-set-key (kbd "M-b p D") 'bts:project-remove-all)
;; (global-set-key (kbd "M-b q n") 'bts:query-new)
;; (global-set-key (kbd "M-b q u") 'bts:query-update)
;; (global-set-key (kbd "M-b q d") 'bts:query-remove)
;; (global-set-key (kbd "M-b q D") 'bts:query-remove-all)
;; 
;; ;; About other config item, see Customization or eval the following sexp.
;; ;; (customize-group "bts")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "bts:[^:]" :docstring t)
;; `bts:project-cache-file'
;; Filepath stores user project configurations.
;; `bts:query-cache-file'
;; Filepath stores user query configurations.
;; `bts:preferred-selection-method'
;; Symbol for a preferred feature used for various selection flow.
;; `bts:widget-menu-minibuffer-flag'
;; Value for `widget-menu-minibuffer-flag' in BTS widget buffer.
;; `bts:widget-label-format'
;; Format of label part in BTS widget buffer.
;; `bts:widget-label-prefix'
;; Prefix of label part in BTS widget buffer.
;; `bts:widget-label-suffix'
;; Suffix of label part in BTS widget buffer.
;; `bts:widget-require-mark'
;; String as a mark of requirement in BTS widget buffer.
;; `bts:ticket-fetch-check-interval'
;; Seconds as interval to check the finish of fetching ticket.
;; `bts:ticket-multi-view-preferred'
;; Whether to open a multi view if marked entry is multiple in summary buffer.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "bts:[^:]" :docstring t)
;; `bts:show-message'
;; Show message. Arguments are passed to `message'.
;; `bts:make-message'
;; Return a message string. Arguments are passed to `format'.
;; `bts:get-message'
;; Return a message string of MSG-ID registed by `bts:regist-message'.
;; `bts:regist-message'
;; Regist message.
;; `bts:internal-symbol-p'
;; Whether SYM_OR_NAME is a dedicated symbol for BTS.
;; `bts:merge-alist'
;; Merge with ALIST2 overwrite ALIST1.
;; `bts:conv-symbol-to-keyword'
;; Return a keyword of SYMBOL. eg. 'hoge -> :hoge
;; `bts:conv-keyward-to-symbol'
;; Return a symbol of KEYWORD. eg. :hoge -> 'hoge
;; `bts:make-seconds'
;; Return a float number of seconds since the epoch
;; `bts:make-alist-from-plist'
;; Return the alist made from PLIST using include/exclude option.
;; `bts:make-plist-from-alist'
;; Return the plist made from ALIST using include/exclude option.
;; `bts:widget-get-local-attribute'
;; Return a local attribute identified by NAME.
;; `bts:widget-get-local-attributes'
;; Return all local attributes.
;; `bts:widget-set-local-attribute'
;; Set VALUE to a local attribute identified by NAME.
;; `bts:widget-goto'
;; Move to a widget identified by NAME.
;; `bts:widget-get-by-name'
;; Return a widget identified by NAME.
;; `bts:widget-get-name-at'
;; Return name of a widget at POS.
;; `bts:widget-get-index-at'
;; Return index of a widget at POS.
;; `bts:widget-get-name-by-index'
;; Return name of a widget identified by INDEX.
;; `bts:widget-exist-p'
;; Whether a widget exists identified by NAME.
;; `bts:widget-exist-flex-p'
;; Whether a flex widget exists identified by NAME in flex INDEX.
;; `bts:widget-get-model'
;; Return current model.
;; `bts:widget-get-flex-models'
;; Return current flex models.
;; `bts:widget-get-value'
;; Return value of a widget identified by NAME.
;; `bts:widget-get-init-value'
;; Return initialized value of a widget identified by NAME.
;; `bts:widget-get-flex-value'
;; Return value of a flex widget identified by NAME in flex INDEX.
;; `bts:widget-get-flex-init-value'
;; Return initialized value of a flex widget identified by NAME in flex INDEX.
;; `bts:widget-get-flex-last-index'
;; Return current max value of flex index.
;; `bts:widget-get-flex-current-index'
;; Return current value of flex index.
;; `bts:widget-expand-flex-layout'
;; Compile flex layout and return the result.
;; `bts:widget-build-buffer'
;; Setup a widget buffer.
;; `bts:widget-update-buffer'
;; Re build a widget buffer of BUFFER-OR-NAME.
;; `bts:widget-validation-uniq'
;; Validate widget value is uniq.
;; `bts:system-regist'
;; Regist SYSTEM as a bug tracking system.
;; `bts:system-get'
;; Return a structure of `bts:system' from SYSNM.
;; `bts:project-p'
;; Whether PROJECT is a project configuration.
;; `bts:project-get-config-all'
;; Return all project configurations.
;; `bts:project-get-config-by-id'
;; Return a project configuration of PROJID.
;; `bts:project-get-config-by-name'
;; Return a project configuration of PROJNM/SYSNM.
;; `bts:project-get-config-name'
;; Return a name of PROJECT.
;; `bts:project-get-system'
;; Return a `bts:system' object of PROJECT.
;; `bts:project-get-unique-string'
;; Return a unique string of PROJECT.
;; `bts:project-store-config'
;; Store a project configuration as PROP of PROJID into `bts:project-cache-file'.
;; `bts:project-delete-config'
;; Delete a project configuration of PROJNM/SYSNM in `bts:project-cache-file'.
;; `bts:query-p'
;; Whether QUERY is a query configuration.
;; `bts:query-get-config-all'
;; Return all query configurations belongs to all projects or PROJECT.
;; `bts:query-get-config-by-id'
;; Return a query configuration of QUERYID.
;; `bts:query-get-config-by-name'
;; Return a query configuration of QUERYNM belongs to PROJECT.
;; `bts:query-get-config-name'
;; Return a name of QUERY.
;; `bts:query-get-project'
;; Return a project configuration of QUERY.
;; `bts:query-get-unique-string'
;; Return a unique string of QUERY.
;; `bts:query-get-description'
;; Return a description string of QUERY.
;; `bts:query-store-config'
;; Store a query configuration as QUERY of QUERYID belongs to PROJECT into `bts:query-cache-file'.
;; `bts:query-delete-config'
;; Delete a query configuration of QUERYNM belongs to PROJECT in `bts:query-cache-file'.
;; `bts:query-delete-config-all'
;; Delete all query configurations belongs to PROJECT in `bts:query-cache-file'.
;; `bts:ticket-p'
;; Whether TICKET is a ticket data.
;; `bts:ticket-get-project'
;; Return the project configuration TICKET belongs to.
;; `bts:ticket-get-system'
;; Return a `bts:system' object TICKET belongs to.
;; `bts:ticket-get-unique-string'
;; Return a unique string of TICKET.
;; `bts:ticket-belong-to-same-project-p'
;; Whether all of TICKETS belongs to same project.
;; `bts:ticket-get-latest'
;; Return a latest data of TICKET.
;; `bts:ticket-simple-update'
;; Update TICKET about all non-nil properties in MODEL.
;; `bts:ticket-multi-update'
;; Update TICKET about non-nil and not empty properties in MODEL.
;; `bts:ticket-setup-conflict-buffer'
;; Setup a conflict display buffer.
;; `bts:ticket-resolve-conflict'
;; Update TICKET by asking user how to handle the property in CONFLICTS.
;; `bts:ticket-open-add-view'
;; Popup a widget buffer for creating a new ticket of PROJECT.
;; `bts:ticket-open-update-view'
;; Popup a widget buffer for updating TICKETS.
;; `bts:ticket-fetch'
;; Start to fetch ticket using QUERIES and the task checkes the finish.
;; `bts:ticket-fetch-failed'
;; Inform the fail of fetching ticket using QUERY to the task checkes the finish.
;; `bts:ticket-fetch-complete'
;; Inform the complete of fetching ticket using QUERY to the task checkes the finish.
;; `bts:complex-condition-compile'
;; Return a `bts:complex-condition' object or a string made from V.
;; `bts:complex-condition-match-to-list'
;; Whether to match any entry of CHECK-VALUES to CONDITION.
;; `bts:complex-condition-match-to-string'
;; Whether to match CHECK-VALUE to CONDITION.
;; `bts:complex-condition-validation'
;; Validate widget value is a complex condition.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "bts:[^:]" :docstring t)
;; `bts:widget-forward'
;; Move to a forward widget using `pophint:do' or `widget-forward'.
;; `bts:widget-backward'
;; Move to a backward widget using `pophint:do' or `widget-backward'.
;; `bts:widget-jump'
;; Jump to any widget using `pophint:do'.
;; `bts:widget-submit'
;; Press a submit button.
;; `bts:widget-cancel'
;; Press a cancel button.
;; `bts:project-new'
;; Popup a widget buffer for creating a new project configuration for SYSTEM.
;; `bts:project-update'
;; Popup a widget buffer for updating PROJECT.
;; `bts:project-remove'
;; Remove PROJECT with confirm.
;; `bts:project-remove-all'
;; Remove all project configurations with confirm.
;; `bts:query-new'
;; Popup a widget buffer for creating a new query configuration belongs to PROJECT.
;; `bts:query-update'
;; Popup a widget buffer for updating QUERY.
;; `bts:query-remove'
;; Remove QUERY with confirm.
;; `bts:query-remove-all'
;; Remove all query configurations with confirm.
;; `bts:ticket-fetch-quit'
;; Quit the running tasks fetches ticket.
;; `bts:summary-right-column'
;; Move to right column.
;; `bts:summary-left-column'
;; Move to left column.
;; `bts:summary-reload-all'
;; Re fetch and setup current buffer.
;; `bts:summary-reload-ticket'
;; Update marked entries or current entry to the latest data.
;; `bts:summary-view-ticket'
;; Popup a widget buffer for updating marked entries or current entry.
;; `bts:summary-mark-ticket'
;; Put a mark to current entry.
;; `bts:summary-unmark-ticket'
;; Remove a mark of current entry.
;; `bts:summary-toggle-ticket-marking'
;; Switch a mark situation of current entry.
;; `bts:summary-mark-all-tickets'
;; Put marks to all entries.
;; `bts:summary-unmark-all-tickets'
;; Remove the marks of all entries.
;; `bts:summary-toggle-all-tickets-marking'
;; Switch the mark situations of all entries.
;; `bts:summary-open'
;; Setup a summary buffer for QUERIES and switch current buffer to that.
;; `bts:ticket-new'
;; Regist a new ticket of PROJECT.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2014-02-22 on chindi10, modified by Debian
;; - widget-mvc.el ... Version 0.0.2
;; - log4e.el ... Version 0.3.0
;; - yaxception.el ... Version 0.3.3
;; - dash.el ... Version 2.9.0
;; - s.el ... Version 1.9.0
;; - pos-tip.el ... Version 0.4.5


;; Enjoy!!!


;;; Code:
(require 'cl-lib)
(require 'plstore)
(require 'wid-edit)
(require 'tabulated-list)
(require 'hl-line)
(require 'widget-mvc)
(require 'log4e)
(require 'yaxception)
(require 'dash)
(require 's)
(require 'pos-tip)
(require 'pophint nil t)

(defgroup bts nil
  "A unified UI for various bug tracking systems."
  :group 'convenience
  :prefix "bts:")

(defcustom bts:project-cache-file (concat user-emacs-directory ".bts-project")
  "Filepath stores user project configurations."
  :type 'string
  :group 'bts)

(defcustom bts:query-cache-file (concat user-emacs-directory ".bts-query")
  "Filepath stores user query configurations."
  :type 'string
  :group 'bts)

(defcustom bts:preferred-selection-method 'helm
  "Symbol for a preferred feature used for various selection flow."
  :type '(choice (const helm)
                 (const anything)
                 (const default)
                 (const nil))
  :group 'bts)

(defcustom bts:widget-menu-minibuffer-flag t
  "Value for `widget-menu-minibuffer-flag' in BTS widget buffer."
  :type 'boolean
  :group 'bts)

(defcustom bts:widget-label-format " %s "
  "Format of label part in BTS widget buffer."
  :type 'string
  :group 'bts)

(defcustom bts:widget-label-prefix " "
  "Prefix of label part in BTS widget buffer."
  :type 'string
  :group 'bts)

(defcustom bts:widget-label-suffix " "
  "Suffix of label part in BTS widget buffer."
  :type 'string
  :group 'bts)

(defcustom bts:widget-require-mark "*"
  "String as a mark of requirement in BTS widget buffer."
  :type 'string
  :group 'bts)

(defcustom bts:ticket-fetch-check-interval 3
  "Seconds as interval to check the finish of fetching ticket."
  :type 'number
  :group 'bts)

(defcustom bts:ticket-multi-view-preferred t
  "Whether to open a multi view if marked entry is multiple in summary buffer.

View means a widget buffer for create/update of ticket.
Multi view is able to update multiple tickets by only one submit.
If this value is nil or multi view is not defined in `bts:system',
instead open views for each of selected tickets."
  :type 'boolean
  :group 'bts)

(defface bts:widget-button-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for `widget-button-face' in BTS widget buffer."
  :group 'bts)

(defface bts:widget-button-pressed-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style pressed-button)
     :background "lightgrey" :foreground "black")
    (t
     :inverse-video t))
  "Face for `widget-button-pressed-face' in BTS widget buffer."
  :group 'bts)

(defface bts:widget-mouse-face
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "grey90" :foreground "black")
    (t
     :inverse-video t))
  "Face for `widget-mouse-face' in BTS widget buffer."
  :group 'bts)

(defface bts:widget-link-face
  '((t :inherit link))
  "Face for a link part in BTS widget buffer."
  :group 'bts)

(defface bts:widget-documentation-face
  '((t nil))
  "Face for `widget-documentation-face' in BTS widget buffer."
  :group 'bts)

(defface bts:widget-label-face
  '((((class color) (background dark))  (:background "azure" :foreground "black" :box "sky blue"))
    (((class color) (background light)) (:background "gray10" :foreground "white" :box "white"))
    (t                                  (:background "azure" :foreground "black" :box "sky blue")))
  "Face for a label part in BTS widget buffer."
  :group 'bts)

(defface bts:widget-require-face
  '((t (:foreground "red")))
  "Face for `bts:widget-require-mark' in BTS widget buffer."
  :group 'bts)

(defface bts:widget-const-face
  '((t nil))
  "Face for a const widget in BTS widget buffer."
  :group 'bts)

(defface bts:widget-tip-face
  '((t (:foreground "black" :background "khaki1")))
  "Face for a popup tip in BTS widget buffer."
  :group 'bts)

(defface bts:summary-condition-warn-part-face
  '((t (:foreground "red" :bold t)))
  "Face for a warning notice in mode line of summary buffer."
  :group 'bts)

(defface bts:summary-condition-fetch-part-face
  '((t (:foreground "tomato" :bold t)))
  "Face for a fetching notice in mode line of summary buffer."
  :group 'bts)

(defface bts:summary-condition-grep-part-face
  '((t (:foreground "black" :background "sky blue")))
  "Face for a grepping notice in mode line of summary buffer."
  :group 'bts)

(defface bts:summary-condition-marked-part-face
  '((t (:foreground "black" :background "misty rose")))
  "Face for a marking notice in mode line of summary buffer."
  :group 'bts)

(defface bts:summary-condition-query-part-face
  '((t (:foreground "black" :background "misty rose")))
  "Face for a queries notice in mode line of summary buffer."
  :group 'bts)

(defface bts:summary-mark-face
  '((t (:background "forest green")))
  "Face for a mark in summary buffer."
  :group 'bts)

(defface bts:summary-closed-ticket-face
  '((t (:inherit font-lock-comment-face)))
  "Face for a closed ticket entry in summary buffer."
  :group 'bts)

(defface bts:summary-ignored-ticket-face
  '((t (:inherit dired-ignored)))
  "Face for a ignored ticket entry in summary buffer."
  :group 'bts)

(defface bts:ticket-regist-message-succeed-face
  '((t (:foreground "lime green" :bold t)))
  "Face for a succeed counts in a finish notice of all registrations."
  :group 'bts)

(defface bts:ticket-regist-message-skipped-face
  '((t (:foreground "khaki" :bold t)))
  "Face for a skipped counts in a finish notice of all registrations."
  :group 'bts)

(defface bts:ticket-regist-message-failed-face
  '((t (:foreground "red" :bold t)))
  "Face for a failed counts in a finish notice of all registrations."
  :group 'bts)


(log4e:deflogger "bts" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                 (error . "error")
                                                 (warn  . "warn")
                                                 (info  . "info")
                                                 (debug . "debug")
                                                 (trace . "trace")))
;; WARN: trace level might happen high memory consumption
(bts--log-set-level 'info)


;;;;;;;;;;;;;
;; Utility

(defvar bts::max-specpdl-size-bkup nil)

(defmacro bts::set-alist-value (alist key value)
  (declare (indent 0))
  `(setq ,alist (loop for (k . v) in ,alist
                      if (eq k ,key) collect (cons k ,value)
                      else           collect (cons k v))))

(defmacro bts:awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defmacro bts:aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))


(defun bts:show-message (msg &rest args)
  "Show message. Arguments are passed to `message'."
  (apply 'message (concat "[BTS] " msg) args)
  nil)

(defun bts:make-message (msg &rest args)
  "Return a message string. Arguments are passed to `format'."
  (apply 'format (concat "[BTS] " msg) args))

(defun bts:get-message (msg-id &rest args)
  "Return a message string of MSG-ID registed by `bts:regist-message'."
  (apply 'wmvc:get-text nil msg-id args))

(defun bts:regist-message (msg-id &rest lang-values)
  "Regist message.

MSG-ID is a symbol which is passed to `bts:get-message'.

LANG-VALUES is a pair list of lang-id/lang-message.
 - lang-id is a symbol to pass to `wmvc:lang-register-messages'.
 - lang-message is a string as a shown value.

Example:
 (bts:regist-message 'hello t \"Hello!\" 'Japanese \"こんにちは\" ...)"
  (declare (indent 1))
  ;; No check msg-id exists
  ;; (when (assq msg-id (assq t wmvc:lang-messages))
  ;;   (error "failed regist messages : id is already registed"))
  (loop for (lang-id value) on lang-values by 'cddr
        do (wmvc:lang-register-messages lang-id (list msg-id value))))


(yaxception:deferror 'bts:error nil "%s" 'msg)


(yaxception:deferror 'bts:invalid-argument 'bts:error 'bts::invalid-argument-msg)

(bts:regist-message 'bts-invalid-argument
  t         "Invalid argument. For check the reason, enable and open log"
  'Japanese "不正な引数が渡されました。詳細はログを有効にして確認して下さい")

(defun bts::invalid-argument-msg ()
  (bts:get-message 'bts-invalid-argument))


(bts:regist-message 'bts-no-candidates
  t         "Any %s is nothing"
  'Japanese "%sが一つもありません")

(defun* bts::select-something (&key description candidates method multiple require default no-error)
  (bts--debug* "start select something. description[%s] method[%s] multiple[%s] require[%s] default[%s] no-error[%s]"
               description method multiple require default no-error)
  (when (and (not no-error)
             (not candidates))
    (yaxception:throw
      'bts:error
      :msg (bts:get-message 'bts-no-candidates (downcase description))))
  (let* ((pref (or method bts:preferred-selection-method))
         (mtd (or (when (and (eq pref 'helm) (featurep 'helm))         'helm)
                  (when (and (eq pref 'anything) (featurep 'anything)) 'anything)
                  (when (eq pref 'default)                             'completing-read)
                  (when (featurep 'helm)                               'helm)
                  (when (featurep 'anything)                           'anything)
                  'completing-read))
         (src `((name . ,description)
                (candidates . ,candidates)
                (candidate-number-limit . 999)
                (action . (lambda (cand)
                            (or (when (not ,multiple)      cand)
                                (when (eq ',mtd 'helm)     (helm-marked-candidates))
                                (when (eq ',mtd 'anything) (anything-marked-candidates))
                                (list cand))))))
         (args (case mtd
                 (helm            `(:sources ,src))
                 (anything        `(:sources ,src))
                 (completing-read `(,(concat description ": ") ,candidates nil t nil '() ,default))))
         (selected (loop while t
                         for selected = (apply mtd args)
                         if selected return selected
                         if (not require) return selected
                         do (progn (bts:show-message "Have to select something")
                                   (sleep-for 2)))))
    (bts--info "finished select something : %s" selected)
    (if (and multiple
             (not (listp selected)))
        (progn (bts--debug "convert result of select something to list")
               (list selected))
      selected)))

(defvar bts::regexp-secret-property "\\`:secret-")

(defun bts::has-secret-property (plist)
  (loop for (k v) on plist by 'cddr
        for s = (symbol-name k)
        if (string-match bts::regexp-secret-property s)
        return t))

(defun bts::pick-up-normal-properties (plist)
  (loop for (k v) on plist by 'cddr
        for s = (symbol-name k)
        if (not (string-match bts::regexp-secret-property s))
        append (progn (bts--trace "picked up normal property : %s" k)
                      (list k v))))

(defun bts::pick-up-secret-properties (plist)
  (loop for (k v) on plist by 'cddr
        for s = (symbol-name k)
        for real-name = (replace-regexp-in-string bts::regexp-secret-property "" s)
        if (not (string= s real-name))
        append (progn (bts--trace "picked up secret property : %s" k)
                      `(,(intern (concat ":" real-name)) ,v))))

(defsubst bts::plstore-load (plstore-sym store-file &optional force)
  (when (or force
            (not (symbol-value plstore-sym))
            (not (buffer-live-p (plstore--get-buffer (symbol-value plstore-sym)))))
    (yaxception:$
      (yaxception:try
        (bts--debug "try to load plstore from %s into %s" store-file plstore-sym)
        (bts:aif (symbol-value plstore-sym)
            (progn (bts--info "revert current plstore : %s" plstore-sym)
                   (plstore-revert it))
          (bts--info "open plstore from %s" store-file)
          (set plstore-sym (plstore-open (expand-file-name store-file)))
          (bts--debug "loaded plstore from %s into %s" store-file plstore-sym)))
      (yaxception:catch 'end-of-buffer e
        (bts--warn "try to init %s because it's broken" store-file)
        (bts:awhen (symbol-value plstore-sym)
          (plstore-close it)
          (set plstore-sym nil))
        (delete-file store-file)
        (set plstore-sym (plstore-open (expand-file-name store-file)))
        (bts--debug "loaded inited plstore from %s into %s" store-file plstore-sym)))))

(defsubst bts::plstore-save (plstore key value)
  (bts--debug "start plstore save value of %s into %s"
              key (ignore-errors (plstore-get-file plstore)))
  (let* ((normal-value (bts::pick-up-normal-properties value))
         (secret-value (bts::pick-up-secret-properties value)))
    (plstore-put plstore key normal-value secret-value)
    (plstore-save plstore)
    (bts--info "finished plstore save value of %s into %s"
               key (plstore-get-file plstore))))

(defsubst bts::plstore-remove (plstore key)
  (bts--debug "start plstore remove value of %s from %s"
              key (ignore-errors (plstore-get-file plstore)))
  (plstore-delete plstore key)
  (plstore-save plstore)
  (bts--info "finished plstore remove value of %s from %s"
             key (plstore-get-file plstore)))

(defsubst bts::plstore-clear (plstore-sym)
  (bts--debug "start plstore clear and delete %s"
              (ignore-errors (plstore-get-file (symbol-value plstore-sym))))
  (when (symbol-value plstore-sym)
    (let ((file (ignore-errors
                  (expand-file-name (plstore-get-file (symbol-value plstore-sym))))))
      (plstore-close (symbol-value plstore-sym))
      (set plstore-sym nil)
      (when (file-exists-p file)
        (bts--debug "try to delete %s" file)
        (delete-file file))
      (bts--info "finished plstore clear and delete %s" file))))


(defun bts:internal-symbol-p (sym_or_name)
  "Whether SYM_OR_NAME is a dedicated symbol for BTS."
  (string-match "\\`bts-" (if (symbolp sym_or_name)
                              (symbol-name sym_or_name)
                            sym_or_name)))

(defun bts:merge-alist (alist1 alist2)
  "Merge with ALIST2 overwrite ALIST1."
  (append (loop for e in alist1
                for k = (car e)
                if (not (assq k alist2)) collect e)
          alist2))

(defun bts:conv-symbol-to-keyword (symbol)
  "Return a keyword of SYMBOL. eg. 'hoge -> :hoge"
  (intern (concat ":" (symbol-name symbol))))

(defun bts:conv-keyward-to-symbol (keyword)
  "Return a symbol of KEYWORD. eg. :hoge -> 'hoge"
  (intern (replace-regexp-in-string "\\`:" "" (symbol-name keyword))))


(defun bts:make-seconds (decoded-time duration &optional backward)
  "Return a float number of seconds since the epoch
as the time away DURATION from DECODED-TIME.

DECODED-TIME is a list as argument for `encode-time'. If nil, means `current-time'.
DURATION is a string as argument for `timer-duration'. If nil, means '0sec'.
If BACKWARD is non-nil, time calculation is done to backward."
  (bts--trace "start make seconds. decoded-time[%s] duration[%s] backward[%s]"
              decoded-time duration backward)
  (let* ((seconds (float-time (bts:awhen decoded-time
                                (apply 'encode-time it))))
         (duration-seconds (if duration
                               (timer-duration duration)
                             0))
         (seconds-calculator (if backward '- '+))
         (ret (funcall seconds-calculator seconds duration-seconds)))
    (bts--trace "made seconds : %s" ret)
    ret))

(defun* bts:make-alist-from-plist (plist &key
                                         includes include-regexp include-function
                                         excludes exclude-regexp exclude-function)
  "Return the alist made from PLIST using include/exclude option.

PLIST is a plist like project/query/ticket.

For including, 
INCLUDES is a list of symbols as property.
INCLUDE-REGEXP is the regexp matches property name.
INCLUDE-FUNCTION is the function receives a symbol as property and should return non-nil if it's a included one.

For excluding ( prior than include options ),
EXCLUDES is a list of symbols as property.
EXCLUDE-REGEXP is the regexp matches property name.
EXCLUDE-FUNCTION is the function receives a symbol as property and should return non-nil if it's a excluded one.

If any options is nothing, all properties is included."
  (loop for k in (loop for (p v) on plist by 'cddr
                       collect (bts:conv-keyward-to-symbol p))
        for s = (symbol-name k)
        for p = (bts:conv-symbol-to-keyword k)
        for exclude-p = (or (bts:internal-symbol-p s)
                            (and excludes         (memq k excludes))
                            (and exclude-regexp   (string-match exclude-regexp s))
                            (and exclude-function (funcall exclude-function k)))
        for include-p = (or (and includes         (memq k includes))
                            (and include-regexp   (string-match include-regexp s))
                            (and include-function (funcall include-function k))
                            (and (not includes)
                                 (not include-regexp)
                                 (not include-function)))
        if (and (not exclude-p) include-p)
        collect (cons k (plist-get plist p))))

(defun* bts:make-plist-from-alist (alist &key
                                         includes include-regexp include-function
                                         excludes exclude-regexp exclude-function)
  "Return the plist made from ALIST using include/exclude option.

ALIST is a alist like model.

For including, 
INCLUDES is a list of symbols as key.
INCLUDE-REGEXP is the regexp matches key name.
INCLUDE-FUNCTION is the function receives a symbol as key and should return non-nil if it's a included one.

For excluding ( prior than include options ),
EXCLUDES is a list of symbols as key.
EXCLUDE-REGEXP is the regexp matches key name.
EXCLUDE-FUNCTION is the function receives a symbol as key and should return non-nil if it's a excluded one.

If any options is nothing, all keys is included."
  (loop for k in (mapcar 'car alist)
        for s = (if k (symbol-name k) "")
        for p = (bts:conv-symbol-to-keyword k)
        for exclude-p = (or (string= s "")
                            (bts:internal-symbol-p s)
                            (and excludes         (memq k excludes))
                            (and exclude-regexp   (string-match exclude-regexp s))
                            (and exclude-function (funcall exclude-function k)))
        for include-p = (or (and includes         (memq k includes))
                            (and include-regexp   (string-match include-regexp s))
                            (and include-function (funcall include-function k))
                            (and (not includes)
                                 (not include-regexp)
                                 (not include-function)))
        if (and (not exclude-p) include-p)
        append (list p (assoc-default k alist))))


;;;;;;;;;;;;
;; Widget

(defvar bts:widget-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n")   'bts:widget-forward)
    (define-key map (kbd "C-p")   'bts:widget-backward)
    (define-key map (kbd "C-M-j") 'bts:widget-jump)
    (define-key map (kbd "M-RET") 'bts:widget-submit)
    (define-key map (kbd "M-DEL") 'bts:widget-cancel)
    map)
  "Keymap as base for any widget keymaps.")

(defvar bts:widget-keymap nil)
(defvar bts:widget-field-keymap nil)
(defvar bts:widget-text-keymap nil)
(defvar bts:widget-date-keymap nil)
(defun bts::wid-setup-keymap ()
  (loop for (kmap . parentmap) in '((bts:widget-keymap       . widget-keymap)
                                    (bts:widget-field-keymap . widget-field-keymap)
                                    (bts:widget-text-keymap  . widget-text-keymap)
                                    (bts:widget-date-keymap  . date-field-keymap))
        if (and (not (keymapp (symbol-value kmap)))
                (boundp parentmap)
                (keymapp (symbol-value parentmap)))
        do (let ((map (copy-keymap bts:widget-common-keymap)))
             (bts--debug "start setup keymap %s from %s" kmap parentmap)
             (set-keymap-parent map (symbol-value parentmap))
             (set kmap map))))

(defun bts::wid-show-help (value)
  (yaxception:$
    (yaxception:try
      (bts--trace "start wid show help.\nvalue... %s" value)
      (cond ((bufferp value)
             (display-buffer value))
            ((stringp value)
             (pos-tip-show value 'bts:widget-tip-face nil nil 300))))
    (yaxception:catch 'error e
      (bts--error "failed wid show help : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed to show widget help : %s" (yaxception:get-text e)))))

(defsubst bts::wid-parts-p (parts)
  (and (listp parts)
       (bts:awhen (plist-get parts :type)
         (symbolp it))))

(defsubst bts::wid-named-parts-p (parts)
  (and (bts::wid-parts-p parts)
       (bts:awhen (plist-get parts :name)
         (symbolp it))))

(defsubst bts::wid-wmvc-widget-item-p (e)
  (and (consp e)
       (bts:awhen (ignore-errors (plist-get (cdr e) :name))
         (symbolp it))))

(defsubst bts::wid-get-wmvc-widget-property (e)
  (cdr e))

(defun bts::wid-get-label-max-width (clayout)
  (loop with maxwidth = 0
        for e in clayout
        for width = (cond ((not (bts::wid-parts-p e))
                           0)
                          ((plist-get e :unarrayed)
                           0)
                          ((plist-get e :label-width)
                           (plist-get e :label-width))
                          ((eq (plist-get e :type) 'label)
                           (string-width (or (plist-get e :value) "")))
                          ((not (plist-get e :label))
                           0)
                          (t
                           (string-width (plist-get e :label))))
        if (> width maxwidth) do (setq maxwidth width)
        finally return maxwidth))

(defun bts::wid-format-label (parts label fmtwidth)
  (let* ((prefix (or (plist-get parts :label-prefix) bts:widget-label-prefix))
         (suffix (or (plist-get parts :label-suffix) bts:widget-label-suffix))
         (face (or (plist-get parts :label-face) 'bts:widget-label-face))
         (fmt (or (plist-get parts :label-format) bts:widget-label-format))
         (array-fmt (concat "%" (if fmtwidth (number-to-string fmtwidth) "") "s"))
         (arrayed-label (format array-fmt label))
         (fmt-label (propertize (format fmt arrayed-label) 'face face))
         (required (plist-get parts :require))
         (req-mark (or (plist-get parts :require-mark) bts:widget-require-mark))
         (req-face (or (plist-get parts :require-face) 'bts:widget-require-face))
         (fmt-req (if required
                      (propertize req-mark 'face req-face)
                    ;; put a space padding for the width of require mark
                    (let* ((req-width (string-width req-mark))
                           (padd-fmt (concat "%" (number-to-string req-width) "s")))
                      (format padd-fmt "")))))
    (concat prefix fmt-label fmt-req suffix)))

(defun bts::wid-get-wmvc-common-arguments (parts &rest excludes)
  (loop for k in '(:name :action :help)
        for v = (plist-get parts k)
        for k = (case k
                  (:help :help-echo)
                  (t     k))
        if (and v
                (not (memq (bts:conv-keyward-to-symbol k) excludes)))
        append (list k v)))

(defun bts::wid-make-label (parts fmtwidth)
  (let ((label (or (plist-get parts :value) ""))
        (fmtwidth (when (not (plist-get parts :unarrayed)) fmtwidth)))
    `(,(bts::wid-format-label parts label fmtwidth))))

(defun bts::wid-make-wmvc-widget-with-label (parts fmtwidth &rest defs)
  (let ((label (plist-get parts :label))
        (fmtwidth (when (not (plist-get parts :unarrayed)) fmtwidth)))
    (if label
        `(,(bts::wid-format-label parts label fmtwidth) ,@defs)
      defs)))

(defun bts::wid-make-wmvc-const (parts fmtwidth)
  (let* ((face (or (plist-get parts :face) 'bts:widget-const-face))
         (value (plist-get parts :value))
         (def `(input :type const ,@(bts::wid-get-wmvc-common-arguments parts)
                      :face ,face :value ,value)))
    (bts::wid-make-wmvc-widget-with-label parts fmtwidth def)))

(defun bts::wid-make-wmvc-textbox (parts fmtwidth)
  (let* ((secret (plist-get parts :secret))
         (area (plist-get parts :area))
         (size (when (not area)
                 (or (plist-get parts :size)
                     (if secret 20 30))))
         (prefix (when (and (plist-get parts :label)
                            area)
                   'BR))
         (keymap (if area bts:widget-text-keymap bts:widget-field-keymap))
         (def `(input :type text ,@(bts::wid-get-wmvc-common-arguments parts) :secret ,secret
                      :size ,size :area ,area :keymap ,keymap)))
    (bts::wid-make-wmvc-widget-with-label parts fmtwidth prefix def)))

(defun bts::wid-make-wmvc-checkbox (parts fmtwidth)
  (let* ((prefix (plist-get parts :prefix))
         (suffix (or (plist-get parts :suffix)
                     (when (not prefix) "On")))
         (tick (when (plist-get parts :tick) t))
         (def `(input :type checkbox ,@(bts::wid-get-wmvc-common-arguments parts) :value ,tick)))
    (bts::wid-make-wmvc-widget-with-label parts fmtwidth prefix def suffix)))

(defun bts::wid-make-wmvc-selectlist (parts fmtwidth)
  (let* ((name (plist-get parts :name))
         (opts (plist-get parts :options))
         (cargs (bts::wid-get-wmvc-common-arguments parts))
         (multiple (plist-get parts :multiple))
         (horizontal (plist-get parts :horizontal))
         (radio (plist-get parts :radio))
         (format (cond (radio    "%b%v")
                       (multiple "%b%v")
                       (t        "%[ %v %]")))
         (type (if radio 'radio 'select))
         (def `(input :type ,type ,@cargs :options ,opts :multiple ,multiple
                      :format ,format :horizontal ,horizontal)))
    (bts::wid-make-wmvc-widget-with-label parts fmtwidth def)))

(defun bts::wid-make-wmvc-link (parts fmtwidth)
  (lexical-let* ((cargs (bts::wid-get-wmvc-common-arguments parts 'action))
                 (url (plist-get parts :url))
                 (info (plist-get parts :info))
                 (file (plist-get parts :file))
                 (tip (plist-get parts :tip))
                 (value (plist-get parts :value))
                 (title (or (plist-get parts :title)
                            (when tip "?")))
                 (action (if tip
                             (lambda (mdl)
                               (let ((v (cond ((functionp tip) (funcall tip mdl))
                                              ((boundp tip)    (symbol-value tip))
                                              (t               tip))))
                                 (bts::wid-show-help v)))
                           (plist-get parts :action)))
                 (def `(input :type link :action ,action ,@cargs
                              :url ,url :info ,info :file ,file :value ,value
                              :title ,title :face bts:widget-link-face)))
    (when (not value)
      (plist-put parts :value (or url info file)))
    (bts::wid-make-wmvc-widget-with-label parts fmtwidth def)))

(defun bts::wid-make-wmvc-date (parts fmtwidth)
  (let* ((cargs (bts::wid-get-wmvc-common-arguments parts))
         (separator (or (plist-get parts :separator) "/"))
         (def `(input :type date ,@cargs :separator ,separator :keymap ,bts:widget-date-keymap)))
    (bts::wid-make-wmvc-widget-with-label parts fmtwidth def)))

(defun bts::wid-make-wmvc-button (parts fmtwidth)
  (let* ((btntitle (plist-get parts :title))
         (title (or btntitle (plist-get parts :label)))
         (cargs (bts::wid-get-wmvc-common-arguments parts)))
    (plist-put parts :value title)
    (if btntitle
        (bts::wid-make-wmvc-widget-with-label parts fmtwidth `(button :title ,title ,@cargs))
      `((button :title ,title ,@cargs)))))

(defun* bts::wid-make-wmvc-template (clayout &key label-unarrayed-p)
  (bts--debug "start wid make wmvc template. label-unarrayed-p[%s]" label-unarrayed-p)
  (bts--trace* "compiled layout is\n%s" clayout)
  (bts::wid-setup-keymap)
  (loop with fmtwidth = (when (not label-unarrayed-p)
                          (bts::wid-get-label-max-width clayout))
        for e in clayout
        if (stringp e)
        collect e
        else if (symbolp e)
        collect e
        else if (bts::wid-parts-p e)
        append (if (plist-get e :const)
                   (bts::wid-make-wmvc-const e fmtwidth)
                 (case (plist-get e :type)
                   (const    (bts::wid-make-wmvc-const      e fmtwidth))
                   (text     (bts::wid-make-wmvc-textbox    e fmtwidth))
                   (checkbox (bts::wid-make-wmvc-checkbox   e fmtwidth))
                   (select   (bts::wid-make-wmvc-selectlist e fmtwidth))
                   (link     (bts::wid-make-wmvc-link       e fmtwidth))
                   (date     (bts::wid-make-wmvc-date       e fmtwidth))
                   (button   (bts::wid-make-wmvc-button     e fmtwidth))
                   (label    (bts::wid-make-label           e fmtwidth))))
        else
        do (bts--error "found wid unknown layout parts : %s" e)))

(defun bts::wid-grep-named-parts-layout (clayout)
  (loop for e in clayout
        if (bts::wid-named-parts-p e)
        collect e))

(defun bts::wid-make-wmvc-model (clayout defaults)
  (bts--debug "start wid make wmvc model.")
  (bts--trace* "compiled layout is\n%s" clayout)
  (bts--trace* "defaults is\n%s" defaults)
  (let ((ret (loop for e in (bts::wid-grep-named-parts-layout clayout)
                   for name = (plist-get e :name)
                   for value = (or (assoc-default name defaults)
                                   (plist-get e :value))
                   collect (cons name value))))
    (bts--debug "made wmvc model : %s" ret)
    ret))

(yaxception:deferror 'bts:validate-failed 'bts:error 'bts::validate-failed-msg 'name 'msg)

(bts:regist-message 'bts-validate-failed
  t         "Failed validate for %s : %s"
  'Japanese "%sの検証に失敗しました : %s")

(defun bts::validate-failed-msg ()
  (bts:get-message 'bts-validate-failed))

(defun bts::wid-make-wmvc-validations (clayout)
  (bts--debug "start wid make wmvc validations.")
  (bts--trace* "compiled layout is\n%s" clayout)
  (loop for e in (bts::wid-grep-named-parts-layout clayout)
        for name = (plist-get e :name)
        for req = (plist-get e :require)
        for validater = (plist-get e :validate)
        for vfunc = (when (or req (functionp validater))
                      (bts--debug "found widget should be validated : %s" name)
                      ;; lambda exp is required more one brase because apply is used in wmvc:validate-fields
                      `((lambda (ctx value &rest args)
                          (yaxception:$
                            (yaxception:try
                              (or (when ,req
                                    (bts--debug "start wmvc:validation-not-empty for %s" ',name)
                                    (let ((vstr (cond ((stringp value) value)
                                                      (value           (format "%s" value)))))
                                      (apply 'wmvc:validation-not-empty ctx vstr args)))
                                  (when (functionp ',validater)
                                    (bts--debug "start custome validate for %s" ',name)
                                    (apply ',validater ctx value args))))
                            (yaxception:catch 'error e
                              (bts--error "failed validate for %s : %s\n%s"
                                          ',name
                                          (yaxception:get-text e)
                                          (yaxception:get-stack-trace-string e))
                              (yaxception:throw 'bts:validate-failed
                                                :name ',name
                                                :msg (yaxception:get-text e)))))))
        if vfunc
        collect (cons name vfunc)))

(defun bts::wid-make-leave-map (clayout)
  (bts--debug "start wid make leave map.")
  (bts--trace* "compiled layout is\n%s" clayout)
  (let ((ret (loop for e in (bts::wid-grep-named-parts-layout clayout)
                   for name = (plist-get e :name)
                   for leave = (plist-get e :leave)
                   if leave
                   collect (cons name leave))))
    (bts--debug "made leave map : %s" ret)
    ret))

(defun bts::wid-compile-layout (layout)
  (bts--debug "start wid compile layout.")
  (bts--trace* "layout is\n%s" layout)
  (loop with expander = '(lambda (e f)
                           (bts--debug "compile parts by %s : %s" f e)
                           (let ((lo (funcall f e)))
                             (bts::wid-compile-layout
                              (cond ((bts::wid-parts-p lo) (list lo))
                                    ((listp lo)            lo)
                                    (t                     (list lo))))))
        for e in layout
        if (functionp e)
        append (funcall expander e 'funcall)
        else if (and (symbolp e)
                     (boundp e))
        append (funcall expander e 'symbol-value)
        else if (and (listp e)
                     (not (bts::wid-parts-p e)))
        append (funcall expander e 'eval)
        else
        collect e))

(defsubst bts::wid-flex-get-index (flex-name)
  (let ((s (if (symbolp flex-name)
               (symbol-name flex-name)
             "")))
    (when (string-match "\\`bts-flex-\\([0-9]+\\)-" s)
      (string-to-number (match-string-no-properties 1 s)))))

(defsubst bts::wid-flex-name-p (name)
  (when (bts::wid-flex-get-index name) t))

(defsubst bts::wid-flex-make-name-from (original-name flex-index)
  (when (bts::wid-flex-name-p original-name)
    (error "Called bts::wid-flex-make-name-from to flex name : %s" original-name))
  (intern (format "bts-flex-%s-%s" flex-index (symbol-name original-name))))

(defsubst bts::wid-flex-reverse-name-from (flex-name)
  (intern (replace-regexp-in-string "\\`bts-flex-[0-9]+-" "" (symbol-name flex-name))))

(defsubst bts::wid-flex-update-name (flex-name new-index)
  (let ((orgnm (bts::wid-flex-reverse-name-from flex-name)))
    (bts::wid-flex-make-name-from orgnm new-index)))

(defsubst bts::wid-flex-get-propertized-index (obj)
  (and (stringp obj)
       (get-text-property 0 'bts-flex-index obj)))

(defsubst bts::wid-flex-wmvc-template-index-shift (tmpl from-index func)
  (loop with curridx = -1
        for e in tmpl
        for pidx = (bts:awhen (bts::wid-flex-get-propertized-index e)
                     (setq curridx it))
        if (and pidx
                (>= pidx from-index))
        do (setq e (propertize e 'bts-flex-index (funcall func pidx)))
        if (and (>= curridx from-index)
                (bts::wid-wmvc-widget-item-p e))
        do (let* ((prop (bts::wid-get-wmvc-widget-property e))
                  (name (plist-get prop :name))
                  (newname (bts::wid-flex-update-name name (funcall func curridx))))
             (plist-put prop :name newname))
        collect e))

(defsubst bts::wid-flex-wmvc-alist-index-shift (alist from-index func)
  (loop for (name . value) in alist
        for curridx = (bts::wid-flex-get-index name)
        if (and curridx
                (>= curridx from-index))
        collect `(,(bts::wid-flex-update-name name (funcall func curridx)) . ,value)
        else
        collect (cons name value)))

(defun bts::wid-flex-wmvc-context-index-shift (ctx from-index func)
  (let ((tmpl (wmvc:context-template ctx))
        (mdl (wmvc:context-model ctx))
        (valid (wmvc:context-validations ctx)))
    (setf (wmvc:context-template ctx)    (bts::wid-flex-wmvc-template-index-shift tmpl from-index func))
    (setf (wmvc:context-model ctx)       (bts::wid-flex-wmvc-alist-index-shift mdl from-index func))
    (setf (wmvc:context-validations ctx) (bts::wid-flex-wmvc-alist-index-shift valid from-index func))))

(defun bts::wid-flex-wmvc-context-index++ (ctx from-index)
  (bts::wid-flex-wmvc-context-index-shift ctx from-index '1+))

(defun bts::wid-flex-wmvc-context-index-- (ctx from-index)
  (bts::wid-flex-wmvc-context-index-shift ctx from-index '1-))

(defun bts::wid-flex-get-index-of-wmvc-template (tmpl flex-index)
  (bts--debug "start wid flex get index of wmvc template. flex-index[%s]" flex-index)
  (bts--trace* "template is\n%s" tmpl)
  (loop with idx = 0
        with lastidx = -1
        for e in tmpl
        for foundidx = (bts::wid-flex-get-propertized-index e)
        for p = (when (bts::wid-wmvc-widget-item-p e)
                  (bts::wid-get-wmvc-widget-property e))
        for currnm = (when (and p (bts::wid-flex-name-p (plist-get p :name)))
                       (bts::wid-flex-reverse-name-from (plist-get p :name)))
        do (bts--trace "%s element : orgnm[%s] flexidx[%s] ... %s" idx currnm foundidx e)
        if (and foundidx (= foundidx flex-index))
        return (progn (bts--info "got flex-index header's index : %s" idx)
                      idx)
        if (eq currnm 'bts-add)
        do (progn (bts--debug "found flex add button's index : %s" idx)
                  ;; Last element is at two after add button
                  (setq lastidx (+ idx 2)))
        do (incf idx)
        finally return (when (>= lastidx 0)
                         (bts--info "found flex last element's index : %s" lastidx)
                         (1+ lastidx))))

(defun bts::wid-flex-delete-alist-member (alist flex-index)
  ;; Pick up the elements which don't have flex-index
  (loop for e in alist
        for idx = (bts::wid-flex-get-index (car e))
        if (or (not idx)
               (not (= idx flex-index)))
        collect e))

(defun bts::wid-flex-compile-layout (flex-layout flex-index)
  (bts--debug "start wid flex compile layout. flex-index[%s]" flex-index)
  (bts--trace* "layout is\n%s" flex-layout)
  (let ((layout `(,@(bts::wid-compile-layout flex-layout)
                  (:type button :name bts-del :label "Del" :action bts::wid-flex-delete)
                  (:type button :name bts-add :label "Add" :action bts::wid-flex-append)))
        (uprop '(:label-format "%s" :label-prefix "" :label-suffix ":" :require-mark "")))
    (append `(,(propertize "-" 'bts-flex-index flex-index) " ")
            (loop for e in layout
                  if (bts::wid-named-parts-p e)
                  ;; update name to notice this parts is a flex parts.
                  append (let* ((newe (copy-sequence e))
                                (name (plist-get newe :name))
                                (newname (bts::wid-flex-make-name-from name flex-index)))
                           (plist-put newe :name newname)
                           (loop for (k v) on uprop  by 'cddr
                                 if (not (plist-get newe k))
                                 do (plist-put newe k v))
                           (list newe " "))
                  else
                  collect e)
            '(BR))))

(defun bts::wid-flex-make-wmvc-model (clayout flex-defaults)
  (bts--debug "start wid make flex wmvc model.")
  (bts--trace* "compiled layout is\n%s" clayout)
  (bts--trace* "defaults is\n%s" flex-defaults)
  (let ((ret (loop for e in (bts::wid-grep-named-parts-layout clayout)
                   for name = (plist-get e :name)
                   for orgnm = (when (bts::wid-flex-name-p name)
                                 (bts::wid-flex-reverse-name-from name))
                   if orgnm
                   collect (let* ((idx (bts::wid-flex-get-index name))
                                  (defs (nth idx flex-defaults)))
                             (cons name (or (assoc-default orgnm defs)
                                            (plist-get e :value)))))))
    (bts--trace* "made flex wmvc model : %s" ret)
    ret))

(defun bts::wid-flex-append (mdl)
  (yaxception:$
    (yaxception:try
      (bts--debug "start wid flex append.")
      (bts--trace* "model is\n%s" mdl)
      (let* ((ctx wmvc:context)
             (flex-layout (bts:widget-get-local-attribute 'bts-flex-layout))
             (currtmpl (wmvc:context-template ctx))
             (curridx (bts::wid-flex-get-index (bts:widget-get-name-at)))
             (newidx (1+ curridx))
             (clayout (bts::wid-flex-compile-layout flex-layout newidx))
             (newtmpl (bts::wid-make-wmvc-template clayout :label-unarrayed-p t))
             (newmdl (bts::wid-flex-make-wmvc-model clayout nil))
             (newvalid (bts::wid-make-wmvc-validations clayout))
             (newleave (bts::wid-make-leave-map clayout))
             (insertidx (bts::wid-flex-get-index-of-wmvc-template currtmpl newidx))
             (updated-tmpl (progn
                             ;; Reflect latest model into wmvc:context
                             (setf (wmvc:context-model ctx) mdl)
                             ;; Shift index of flex name in wmvc:context
                             (bts::wid-flex-wmvc-context-index++ ctx newidx)
                             (wmvc:context-template ctx)))
             (updated-mdl (wmvc:context-model ctx))
             (updated-valid (wmvc:context-validations ctx))
             (first-name (loop for e in flex-layout
                               if (bts::wid-named-parts-p e)
                               return (bts::wid-flex-make-name-from (plist-get e :name) newidx))))
        ;; ReSetup
        (bts:widget-set-local-attribute 'bts-leave
                                        (append (bts:widget-get-local-attribute 'bts-leave)
                                                newleave))
        (bts::wid-reload-buffer
         ;; Insert new template into wmvc:context template
         :tmpl (append (-slice updated-tmpl 0 insertidx)
                       newtmpl
                       (-slice updated-tmpl insertidx))
         ;; Merge new model, validation into wmvc:context
         :model (append updated-mdl newmdl)
         :validations (append updated-valid newvalid)
         :focus-to first-name)))
    (yaxception:catch 'error e
      (bts--error "failed bts::wid-flex-append : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed append flex widgets : %s" (yaxception:get-text e)))))

(defun bts::wid-flex-delete (mdl)
  (yaxception:$
    (yaxception:try
      (bts--debug "start wid flex delete.")
      (bts--trace* "model is\n%s" mdl)
      (let* ((ctx wmvc:context)
             (currtmpl (wmvc:context-template ctx))
             (currvalid (wmvc:context-validations ctx))
             (curridx (bts::wid-flex-get-index (bts:widget-get-name-at)))
             (nextidx (1+ curridx))
             (remove-startidx (bts::wid-flex-get-index-of-wmvc-template currtmpl curridx))
             (remove-endidx (bts::wid-flex-get-index-of-wmvc-template currtmpl nextidx)))
        ;; Remove template, model, validation of selected flex layout from wmvc:context
        (setf (wmvc:context-template ctx) (append (-slice currtmpl 0 remove-startidx)
                                                  (-slice currtmpl remove-endidx)))
        (setf (wmvc:context-model ctx) (bts::wid-flex-delete-alist-member mdl curridx))
        (setf (wmvc:context-validations ctx) (bts::wid-flex-delete-alist-member currvalid curridx))
        (bts:widget-set-local-attribute 'bts-leave
                                        (bts::wid-flex-delete-alist-member
                                         (bts:widget-get-local-attribute 'bts-leave)
                                         curridx))
        ;; Shift index of flex name in wmvc:context
        (bts::wid-flex-wmvc-context-index-- ctx nextidx)
        ;; ReSetup
        (bts::wid-reload-buffer
         :focus-to (list (bts::wid-flex-make-name-from 'bts-del curridx)
                         (bts::wid-flex-make-name-from 'bts-del 0)))))
    (yaxception:catch 'error e
      (bts--error "failed bts::wid-flex-delete : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed delete flex widgets : %s" (yaxception:get-text e)))))

(defun bts::wid-make-full-wmvc-template (attrs clayout)
  (let* ((title        (assoc-default 'bts-title        attrs))
         (title-face   (assoc-default 'bts-title-face   attrs))
         (submit-title (assoc-default 'bts-submit-title attrs))
         (submit-title (format " %s " (or submit-title "OK")))
         (cancel-title (assoc-default 'bts-cancel-title attrs))
         (cancel-title (format " %s " (or cancel-title "Cancel"))))
    `(,(propertize (or title "*NoTitle*") 'face (or title-face 'info-title-1)) BR BR
      ;; (message :name error :face compilation-error) BR
      ,@(bts::wid-make-wmvc-template clayout)
      BR BR
      "    " (button :title ,submit-title :action on-submit :name bts-submit :validation t)
      "  "   (button :title ,cancel-title :action on-cancel :name bts-cancel))))

(defun bts::wid-edit-handler (widget)
  (bts--trace* "start wid edit handler.\nwidget... %s" widget)
  )

(defun bts::wid-submit (mdl)
  (yaxception:$
    (yaxception:try
      (bts--debug "start widget submit.\nmodel... %s" mdl)
      (bts:aif (bts:widget-get-local-attribute 'bts-submit-action)
          (let ((buf (current-buffer)))
            (funcall it mdl)
            (kill-buffer buf))
        (bts:show-message "No submit action")))
    (yaxception:catch 'error e
      (bts--fatal "failed widget submit : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed widget submit : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-wid-cancel-confirm
  t         "Buffer has been changed. Quit without submit?"
  'Japanese "内容が変更されていますが、登録せずに閉じてよろしいですか？")

(defun bts::wid-cancel (mdl)
  (yaxception:$
    (yaxception:try
      (bts--debug "start widget cancel.\nmodel... %s" mdl)
      (bts:aif (bts:widget-get-local-attribute 'bts-cancel-action)
          (funcall it mdl)
        (let* ((initmdl (bts:widget-get-local-attribute 'bts-init-model))
               (conv-empty-nil (lambda (v)
                                 ;; consider empty as nil
                                 (if (and (stringp v) (string= v ""))
                                     nil
                                   v)))
               (delete-p (loop for (k . v) in initmdl
                               for pair = (assq k mdl)
                               for v = (funcall conv-empty-nil v)
                               if (and (not pair) v)
                               return (progn
                                        (bts--debug "found deleted : name[%s] init[%s]" k v)
                                        t)))
               (update-p (loop for (k . v1) in initmdl
                               for pair = (assq k mdl)
                               for v1 = (funcall conv-empty-nil v1)
                               for v2 = (funcall conv-empty-nil (cdr-safe pair))
                               if (and pair (not (equal v1 v2)))
                               return (progn
                                        (bts--debug "found updated : name[%s] init[%s] curr[%s]" k v1 v2)
                                        t)))
               (add-p (loop for (k . v) in mdl
                            for pair = (assq k initmdl)
                            for v = (funcall conv-empty-nil v)
                            if (and (not pair) v)
                            return (progn
                                     (bts--debug "found added : name[%s] curr[%s]" k v)
                                     t))))
          (when (or (and (not delete-p)
                         (not update-p)
                         (not add-p))
                    (y-or-n-p (bts:get-message 'bts-wid-cancel-confirm)))
            (kill-this-buffer)))))
    (yaxception:catch 'error e
      (bts--fatal "failed widget cancel : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed widget cancel : %s" (yaxception:get-text e)))))

(defun bts::wid-update (mdl)
  (yaxception:$
    (yaxception:try
      (bts--debug "start widget update.\nmodel... %s" mdl)
      (bts:widget-update-buffer :showp t))
    (yaxception:catch 'error e
      (bts--fatal "failed widget update : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed widget update : %s" (yaxception:get-text e)))))

(defvar bts::wid-buffer-p nil)

(defun* bts::wid-build-buffer (&key buf
                                    tmpl
                                    model
                                    validations
                                    submit-action
                                    cancel-action
                                    attributes)
  (bts--debug "start wid build buffer.")
  (bts--trace* "buf... %s
tmpl... %s
model... %s
validations... %s
submit-action... %s
cancel-action... %s
attributes... %s"
               buf tmpl model validations submit-action cancel-action attributes)
  (let ((acts `((on-submit . bts::wid-submit)
                (on-cancel . bts::wid-cancel)
                (on-update . bts::wid-update)))
        (attributes (bts:merge-alist attributes
                                     `((bts-submit-action . ,submit-action)
                                       (bts-cancel-action . ,cancel-action)
                                       (bts-init-model    . ,(copy-alist model)))))
        (bts::wid-buffer-p t))
    (wmvc:build-buffer :buffer buf
                       :tmpl tmpl
                       :model (copy-alist model)
                       :validations validations
                       :actions acts
                       :attributes attributes)))

(defun* bts::wid-rebuild-buffer (&key buf tmpl model validations)
  (bts--debug "start wid rebuild buffer.")
  (bts--trace* "buf... %s\ntmpl... %s\nmodel... %s\nvalidations... %s"
               buf tmpl model validations)
  (let* ((buf (or buf (current-buffer)))
         (ctx (buffer-local-value 'wmvc:context buf))
         (new-buf (wmvc:get-new-buffer (buffer-name buf)))
         (bts::wid-buffer-p t))
    (bts:awhen tmpl        (setf (wmvc:context-template    ctx) it))
    (bts:awhen model       (setf (wmvc:context-model       ctx) it))
    (bts:awhen validations (setf (wmvc:context-validations ctx) it))
    (wmvc:tmpl-build-buffer new-buf ctx)))

(defun* bts::wid-reload-buffer (&key buf tmpl model validations focus-to)
  (let* ((buf (or buf (current-buffer)))
         (win (get-buffer-window buf))
         (curridx (when (not focus-to)
                    (with-current-buffer buf
                      (bts:widget-get-index-at))))
         (new-buf (bts::wid-rebuild-buffer :buf buf
                                           :tmpl tmpl
                                           :model model
                                           :validations validations)))
    (cond ((or (null win) (not (window-live-p win)))
           (pop-to-buffer new-buf))
          (t
           (set-window-buffer win new-buf)
           (set-buffer new-buf)))
    (or (when focus-to
          (bts--debug "try to focus to %s" focus-to)
          (with-current-buffer new-buf
            (loop for name in (if (listp focus-to) focus-to (list focus-to))
                  if (wmvc:widget-focus-to name)
                  return (progn (bts--info "focused to %s" name)
                                t))))
        (when curridx
          (bts--debug "try to focus to %s parts" curridx)
          (with-current-buffer new-buf
            (bts:awhen (bts:widget-get-name-by-index curridx)
              (when (wmvc:widget-focus-to it)
                (bts--info "focused to %s" it))))))
    new-buf))

(defmacro bts::wid-on-wmvc-buffer (func-name &rest body)
  (declare (indent 1))
  `(if (not (wmvc:context-p wmvc:context))
       (bts--error "failed widget %s : buffer[%s] is not widget buffer" ,func-name (buffer-name))
     ,@body))


(defun bts:widget-forward ()
  "Move to a forward widget using `pophint:do' or `widget-forward'."
  (interactive)
  (if (boundp 'pophint:source-widget)
      (pophint:do :source 'pophint:source-widget :direction 'forward)
    (widget-forward 1)))

(defun bts:widget-backward ()
  "Move to a backward widget using `pophint:do' or `widget-backward'."
  (interactive)
  (if (boundp 'pophint:source-widget)
      (pophint:do :source 'pophint:source-widget :direction 'backward)
    (widget-backward 1)))

(defun bts:widget-jump ()
  "Jump to any widget using `pophint:do'."
  (interactive)
  (if (not (functionp 'pophint:do-widget))
      (bts:show-message "pophint:do-widget is not defined. Check to install pophint-config.el")
    (pophint:do-widget)))

(defun bts:widget-submit ()
  "Press a submit button."
  (interactive)
  (bts:awhen (bts:widget-get-by-name 'bts-submit)
    (widget-apply it :action)))

(defun bts:widget-cancel ()
  "Press a cancel button."
  (interactive)
  (bts:awhen (bts:widget-get-by-name 'bts-cancel)
    (widget-apply it :action)))

(defun bts:widget-get-local-attribute (name)
  "Return a local attribute identified by NAME."
  (bts::wid-on-wmvc-buffer "get local attribute"
    (let ((ret (wmvc:context-attr-get wmvc:context name)))
      ;; (bts--debug "got widget local attribute of %s in %s : %s" name (buffer-name) ret)
      ret)))

(defun bts:widget-get-local-attributes ()
  "Return all local attributes."
  (bts::wid-on-wmvc-buffer "get local attribute"
    (wmvc:context-attributes wmvc:context)))

(defun bts:widget-set-local-attribute (name value)
  "Set VALUE to a local attribute identified by NAME."
  (bts::wid-on-wmvc-buffer "set local attribute"
    (bts--debug "set widget local attribute of %s in %s : %s" name (buffer-name) value)
    (wmvc:context-attr-set wmvc:context name value)))

(defun bts:widget-goto (name)
  "Move to a widget identified by NAME."
  (bts::wid-on-wmvc-buffer "goto"
    (wmvc:widget-focus-to name)))

(defun bts:widget-get-by-name (name)
  "Return a widget identified by NAME."
  (bts::wid-on-wmvc-buffer "get by name"
    (loop for (currnm . widget) in (wmvc:context-widget-map wmvc:context)
          if (eq currnm name)
          return (progn (bts--debug "got widget by name : %s" name)
                        widget))))

(defun bts:widget-get-name-at (&optional pos)
  "Return name of a widget at POS."
  (bts::wid-on-wmvc-buffer "get point name"
    (bts:awhen (widget-at pos)
      (loop for (name . widget) in (wmvc:context-widget-map wmvc:context)
            if (eql widget it)
            return (progn (bts--debug "got widget point name : %s" name)
                          name)))))

(defun bts:widget-get-index-at (&optional pos)
  "Return index of a widget at POS."
  (bts::wid-on-wmvc-buffer "get point index"
    (bts:awhen (widget-at pos)
      (save-excursion
        (loop initially (goto-char (point-min))
              with idx = 1
              for w = (ignore-errors
                        (widget-forward 1)
                        (widget-at))
              until (not w)
              if (eql w it)
              return (progn (bts--debug "got widget point index : %s" idx)
                            idx)
              do (incf idx))))))

(defun bts:widget-get-name-by-index (index)
  "Return name of a widget identified by INDEX."
  (bts::wid-on-wmvc-buffer "get name by index"
    (ignore-errors
      (save-excursion
        (goto-char (point-min))
        (widget-forward index)
        (bts:awhen (bts:widget-get-name-at)
          (bts--debug "got widget name by index : %s" it)
          it)))))

(defun bts:widget-exist-p (name)
  "Whether a widget exists identified by NAME."
  (bts::wid-on-wmvc-buffer "exist"
    (when (assq name (wmvc:context-widget-map wmvc:context))
      t)))

(defun bts:widget-exist-flex-p (name index)
  "Whether a flex widget exists identified by NAME in flex INDEX."
  (bts::wid-on-wmvc-buffer "exist flex"
    (when (assq (bts::wid-flex-make-name-from name index)
                (wmvc:context-widget-map wmvc:context))
      t)))

(defun bts:widget-get-model ()
  "Return current model.

Model is a alist of all widgets value map.
The entry format is (NAME . VALUE)."
  (bts::wid-on-wmvc-buffer "get current model"
    (wmvc:bind-from-widgets wmvc:context)))

(defun bts:widget-get-flex-models (&optional model)
  "Return current flex models.

MODEL is model. If nil, get from `bts:widget-get-model'.
Flex models is a list of model for each widgets of flex layout.
For flex layout, see `bts:widget-build-buffer'."
  (bts::wid-on-wmvc-buffer "get flex models"
    (let ((flex-modelh (make-hash-table)))
      (dolist (e (or model (bts:widget-get-model)))
        (let* ((name (car e))
               (value (cdr e))
               (idx (bts::wid-flex-get-index name))
               (orgnm (when idx (bts::wid-flex-reverse-name-from name)))
               (newmodel (when orgnm `((,orgnm . ,value) ,@(gethash idx flex-modelh)))))
          (when newmodel
            (puthash idx newmodel flex-modelh))))
      (loop with idx = 0
            for m = (gethash idx flex-modelh)
            until (not m)
            collect m
            do (incf idx)))))

(defun bts:widget-get-value (name &optional model)
  "Return value of a widget identified by NAME.

MODEL is model. If nil, get from `bts:widget-get-model'."
  (bts::wid-on-wmvc-buffer "get value"
    (if (and (not model)
             (not (bts:widget-exist-p name)))
        (bts:widget-get-init-value name)
      (assoc-default name (or model (bts:widget-get-model))))))

(defun bts:widget-get-init-value (name)
  "Return initialized value of a widget identified by NAME."
  (bts::wid-on-wmvc-buffer "get init value"
    (assoc-default name (or (bts:widget-get-local-attribute 'bts-init-model)
                            (bts:widget-get-local-attribute 'bts-defaults)))))

(defun bts:widget-get-flex-value (name index &optional model)
  "Return value of a flex widget identified by NAME in flex INDEX.

MODEL is model. If nil, get from `bts:widget-get-model'."
  (bts::wid-on-wmvc-buffer "get flex value"
    (if (and (not model)
             (not (bts:widget-exist-flex-p name index)))
        (bts:widget-get-flex-init-value name index)
      (let* ((flex-models (bts:widget-get-flex-models model))
             (imodel (ignore-errors (nth index flex-models))))
        (assoc-default name imodel)))))

(defun bts:widget-get-flex-init-value (name index)
  "Return initialized value of a flex widget identified by NAME in flex INDEX."
  (bts::wid-on-wmvc-buffer "get flex init value"
    (bts:aif (bts:widget-get-local-attribute 'bts-init-model)
        (bts:widget-get-flex-value name index it)
      (let* ((flex-models (bts:widget-get-local-attribute 'bts-flex-defaults))
             (imodel (ignore-errors (nth index flex-models))))
        (assoc-default name imodel)))))

(defvar bts::wid-flex-current-last-index nil)
(defun bts:widget-get-flex-last-index ()
  "Return current max value of flex index."
  (bts:aif bts::wid-flex-current-last-index
      (progn
        ;; For get at start of buffer
        (bts--debug "got flex last index from current set variable : %s" it)
        it)
    (bts::wid-on-wmvc-buffer "get flex last index"
      (loop with lastidx = -1
            for e in (wmvc:context-template wmvc:context)
            for idx = (bts::wid-flex-get-propertized-index e)
            if (and idx (> idx lastidx))
            do (setq lastidx idx)
            finally return (progn
                             (bts--debug "got flex last index : %s" lastidx)
                             lastidx)))))

(defvar bts::wid-flex-current-index nil)
(defun bts:widget-get-flex-current-index ()
  "Return current value of flex index."
  (bts::wid-on-wmvc-buffer "get flex current index"
    bts::wid-flex-current-index))

(defun bts:widget-expand-flex-layout ()
  "Compile flex layout and return the result.

flex layout is a :flex-layout argument of `bts:widget-build-buffer'.
This function should be embedded in a :layout argument of `bts:widget-build-buffer'."
  (bts--debug "start widget expand flex layout.")
  (loop with flex-layout = (bts:widget-get-local-attribute 'bts-flex-layout)
        for i from 0 to (max (bts:widget-get-flex-last-index) 0)
        for bts::wid-flex-current-index = i
        append (bts::wid-flex-compile-layout flex-layout i)))

(defvar bts::default-widget-buffer-name "*BTS: Widget*")
(defun* bts:widget-build-buffer (&key buffer
                                      layout
                                      defaults
                                      flex-layout
                                      flex-defaults
                                      title
                                      title-face
                                      submit-title
                                      submit-action
                                      cancel-title
                                      cancel-action
                                      attributes)
  "Setup a widget buffer.

BUFFER is a buffer. If nil, generate a buffer named `bts::default-widget-buffer-name'.

LAYOUT is list of plist or string or BR symbol as buffer contents definition.
  If other function or variable or sexp exists, they are evaluated recursively.
  If the value is a plist, it's considered as a declare of widget.
  For check the specification, see Widget Declaration section below.

DEFAULTS is a alist as initialized value for widget from LAYOUT.
  The entry format is (NAME . VALUE).

FLEX-LAYOUT is the value like LAYOUT.
  The buffer contents part will be variable by append and delete buttons
  which are added at the last of them.

FLEX-DEFAULTS is list of alist as initialized value for each widget from FLEX-LAYOUT.

TITLE is a string for a head text of the widget buffer.
TITLE-FACE is a face for TITLE. If nil, `info-title-1' is used.

SUBMIT-TITLE is a string for submit button text. In default, \"OK\".
SUBMIT-ACTION is a function called when submit button is pressed.
  It receives an argument as model. About model, see `bts:widget-get-model'.

CANCEL-TITLE is a string for cancel button text. In default, \"Cancel\".
CANCEL-ACTION is a function called when cancel button is pressed.
  It receives an argument as model. About model, see `bts:widget-get-model'.

ATTRIBUTES is a alist for buffer local value. The entry format is (NAME . VALUE).
  `bts:widget-get-local-attribute'/`bts:widget-get-local-attributes' accesses this value.
  Also, `bts:widget-set-local-attribute' updates this value.


Widget Declaration:

***** Required Properties *****

:type - It declares widget type. It accepts the following symbols.

  const    ... read only widget. It's applied by :const property too.
  text     ... editable text field
  checkbox ... checkbox
  select   ... select list
  link     ... link
  date     ... date field
  button   ... button
  label    ... just label text. It's not widget. The contents equals the value from :label property.

***** Common Properties *****

:name - It identity widget. It accepts any symbol.
        If the widget value should be accessed, This property is required.

:action - It's a function called when action is done to the widget.
          Action means a button widget is pressed, a editable field is inputed,
          and a selectable widget is selected.
          It receives an argument as model. About model, see `bts:widget-get-model'.

:leave - It's a function called when the widget focus is lost.
         It receives an argument as model. About model, see `bts:widget-get-model'.

:help - It's a message will be shown when the widget focused.
        It accepts a string or a function returns a string.

:label - It's a text shown at before of the widget area.
         Each of this value of all widget are arrayed orderly and decorated.

:label-width - It's width of :label value. If nil, this value is calculated automatically.

:label-prefix - It's prefix of :label value. If nil, `bts:widget-label-prefix' is used.

:label-suffix - It's suffix of :label value. If nil, `bts:widget-label-suffix' is used.

:label-face - It's face for :label value. If nil, `bts:widget-label-face' is used.

:label-format - It's format for :label value. If nil, `bts:widget-label-format' is used.

:unarrayed - If non-nil, the widget :label value is not arrayed.

:require - If non-nil, the widget is considered as a required one.
           A required mark is shown after :label part of the widget.
           The widget is validated that the value is not empty when submit button is pressed.

:require-mark - It's a string of mark for a required widget. If nil, `bts:widget-require-mark' is used.

:require-face - It's face for :require-mark value. If nil, `bts:widget-require-face' is used.

:validate - It's a function called when submit button is pressed.
            It receives the arguments which are a `wmvc:context' object, the widget value, and other infos.
            For detail, see `wmvc:validate-fields'.
            It should returns a description of the error if the widget value is invalid.

:const - If non-nil, the declare is considered as const widget ignoring the value of :type property.

:value - It's the widget value. DEFAULTS is prior than this value.

***** Particular Properties *****

:type property value equals

Case const

:value - It's the widget value. It's shown as string.
:face  - It's face for the widget value.

Case text

:size   - It's width of the widget area. If nil, it's a end of line.
:secret - If non-nil, the inputed value is shown as \"*\".
:area   - If non-nil, the editable field comes multiple line.

Case checkbox

:prefix - It's prefix of checkbox.
:suffix - It's suffix of checkbox. If :prefix and :suffix is nil, \"On\" is used.
:tick   - If non-nil, the widget is ticked initially.

Case select

:options    - It's candidates of the widget value. It accepts the following formats.
              list  : (VALUE1 VALUE2 ...)
              alist : ((SHOWN-TEXT1 . VALUE1) (SHOWN-TEXT2 . VALUE2) ...)
:radio      - If non-nil, the widget is created as radio widget.
:multiple   - If non-nil, the widget value comes multiple selectable.
:horizontal - If non-nil, :options values are arrayed horizontally.

Case link

:url   - It's a string as link target url.
:info  - It's a path of info file as link target.
:file  - It's a path of file as link target.
:tip   - It's a string or variable has a string or function returns a string.
         The value is shown in popup-tip or buffer as link target.
:value - It's the link target value.
         If :url or :info or :file or :tip exists, the value is used.
:title - It's a shown text as the widget.
         If nil, :url or :info or :file or :tip or :value is used.

Case date

:separator - It's a string separates between year and month and day. If nil, \"/\" is used.

Case button

:title - It's a shown text as the widget. If nil, :label property value is used.

Case label

:value - It's a shown text as label.
"
  (let* ((attrs (bts:merge-alist attributes
                                 `((bts-title         . ,title)
                                   (bts-title-face    . ,title-face)
                                   (bts-submit-title  . ,submit-title)
                                   (bts-cancel-title  . ,cancel-title)
                                   (bts-layout        . ,layout)
                                   (bts-defaults      . ,defaults)
                                   (bts-flex-layout   . ,flex-layout)
                                   (bts-flex-defaults . ,flex-defaults))))
         (buf (or buffer (wmvc:get-new-buffer bts::default-widget-buffer-name)))
         (clayout (with-current-buffer buf
                    (set (make-local-variable 'wmvc:context) (make-wmvc:context :attributes attrs))
                    (let ((bts::wid-flex-current-last-index (1- (length flex-defaults))))
                      (bts::wid-compile-layout layout))))
         (tmpl (bts::wid-make-full-wmvc-template attrs clayout))
         (mdl (bts:merge-alist (bts::wid-make-wmvc-model clayout defaults)
                               (when flex-layout
                                 (bts::wid-flex-make-wmvc-model clayout flex-defaults))))
         (valids (bts::wid-make-wmvc-validations clayout))
         (leave (bts::wid-make-leave-map clayout)))
    (bts::wid-build-buffer :buf buf
                           :tmpl tmpl
                           :model mdl
                           :validations valids
                           :submit-action submit-action
                           :cancel-action cancel-action
                           :attributes (bts:merge-alist (with-current-buffer buf
                                                          (bts:widget-get-local-attributes))
                                                        `((bts-leave . ,leave))))))

(defun* bts:widget-update-buffer (&key buffer-or-name showp)
  "Re build a widget buffer of BUFFER-OR-NAME."
  (bts--debug "start widget update buffer. buffer-or-name[%s] showp[%s]" buffer-or-name showp)
  (with-current-buffer (or (bts:awhen buffer-or-name (get-buffer it))
                           (current-buffer))
    (bts::wid-on-wmvc-buffer "update buffer"
      (let* ((layout (bts:widget-get-local-attribute 'bts-layout))
             (flex-layout (bts:widget-get-local-attribute 'bts-flex-layout))
             (defaults (bts:widget-get-local-attribute 'bts-defaults))
             (flex-defaults (bts:widget-get-local-attribute 'bts-flex-defaults))
             (clayout (bts::wid-compile-layout layout))
             (tmpl (bts::wid-make-full-wmvc-template (bts:widget-get-local-attributes)
                                                     clayout))
             (mdl (bts:merge-alist
                   (bts:merge-alist (bts::wid-make-wmvc-model clayout defaults)
                                    (when flex-layout
                                      (bts::wid-flex-make-wmvc-model clayout flex-defaults)))
                   ;; Overwrite by current situation
                   (wmvc:bind-from-widgets wmvc:context)))
             (valids (bts::wid-make-wmvc-validations clayout))
             (leave (bts::wid-make-leave-map clayout)))
        (bts:widget-set-local-attribute 'bts-leave leave)
        (if showp
            (bts::wid-reload-buffer :tmpl tmpl :model mdl :validations valids)
          (bts::wid-rebuild-buffer :tmpl tmpl :model mdl :validations valids))))))

(bts:regist-message 'bts-validation-be-uniq
  t         "Already same value had been registed."
  'Japanese "同じ名称が登録済み")

(defun bts:widget-validation-uniq (ctx value &rest args)
  "Validate widget value is uniq."
  (let* ((partsnm (plist-get args :name))
         (check-list (plist-get args :check-list))
         (initvalue (or (plist-get args :init-value)
                        (assoc-default partsnm (wmvc:context-model ctx))
                        ""))
         (others (loop for e in check-list
                       if (not (string= e initvalue))
                       collect e))
         (check-value (or (plist-get args :value)
                          value)))
    (when (member check-value others)
      (wmvc:get-text ctx 'bts-validation-be-uniq))))


;;;;;;;;;;;;
;; System

(defstruct bts:system
  name
  project-view
  query-view
  ticket-single-view
  ticket-multi-view
  ticket-fetcher
  ticket-register
  ticket-eraser
  ticket-unique-string
  ticket-latest
  summary-format
  conflict-checker
  conflict-resolver
  )

(defvar bts::system-hash (make-hash-table))

(defun bts::system-normalize-name (sysnm)
  (cond ((stringp sysnm) sysnm)
        (t               (format "%s" sysnm))))

(defun* bts::system-select (&key method no-error)
  (let* ((sysnms (loop for k being the hash-key in bts::system-hash collect (symbol-name k)))
         (selected (bts::select-something :description "System Type"
                                          :candidates sysnms
                                          :require t
                                          :default (car sysnms)
                                          :method method
                                          :no-error no-error)))
    (when selected
      (bts:system-get selected no-error))))

(defun bts:system-regist (system)
  "Regist SYSTEM as a bug tracking system.

SYSTEM is a `bts:system' object which has the following properties.

* name (required)

It's a symbol identify the system.

* project-view (required)

It's a funciton for a declare of the widget buffer which create/update a project configuration.
It receives an argument which is a project configuration.
  That is nil means this call is for create.
It should return a plist has the following properties.
  :buffer        (optional)
  :layout        (required)
  :defaults      (optional)
  :flex-layout   (optional)
  :flex-defaults (optional)
  :submit-action (optional) should return a project configuration.
                            If nil, `bts:make-plist-from-alist' is used.
  :attributes    (optional)
  For other detail of them, see `bts:widget-build-buffer'.
  About the specification of project configuration, see `bts:project-store-config'.

* query-view (required)

It's a funciton for a declare of the widget buffer which create/update a query configuration.
It receives the arguments which are a project configuration, a query configuration.
  The query configuration is nil means this call is for craete.
It should return a plist has the following properties.
  :buffer        (optional)
  :layout        (required)
  :defaults      (optional)
  :flex-layout   (optional)
  :flex-defaults (optional)
  :submit-action (optional) should return a query configuration.
                            If nil, `bts:make-plist-from-alist' is used.
  :attributes    (optional)
  For other detail of them, see `bts:widget-build-buffer'.
  About the specification of query configuration, see `bts:query-store-config'.

* ticket-single-view (required)

It's a funciton for a declare of the widget buffer which create/update a ticket data.
It receives the arguments which are a project configuration, a ticket data.
  The ticket data is nil means this call is for craete.
It should return a plist has the following properties.
  :buffer        (optional)
  :layout        (required)
  :defaults      (optional)
  :submit-action (optional) receives the arguments which are a ticket data, model.
                            should do a update of ticket by model.
                            If nil, `bts:ticket-simple-update' is used.
  :attributes    (optional)

* ticket-multi-view (optional)

It's a funciton for a declare of the widget buffer which update multiple ticket data by only one submit.
It receives the arguments which are a project configuration, a ticket data list.
It should return a plist has the following properties.
  :buffer        (optional)
  :layout        (required)
  :defaults      (optional)
  :submit-action (optional) receives the arguments which are a ticket data, model.
                            should do a update of ticket by model.
                            If nil, `bts:ticket-multi-update' is used.
  :attributes    (optional)

* ticket-fetcher (required)

It's a function for fetching ticket data into summary buffer.
It receives the arguments which are a project configuration, a query configuration.
It should try to fetch the ticket data matched query configuration.
Then, if the action is succeed,
calls `bts:ticket-fetch-complete' and passes a query configuration, a list of fetched ticket data.
Else, calls `bts:ticket-fetch-failed' and passes a query configuration.

* ticket-register (required)

It's a function for registration of ticket.
It receives the arguments which are a project configuration, a ticket data, a diff list.
  The diff list is a list of the information about updated proprety in the ticket.
  The format is ((NAME1 . (BEFORE1 . AFTER1)) (NAME2 . (BEFORE2 . AFTER2))...).
  The diff list is nil means this call is for create.
It should return non-nil if the registration is succeed.
  If the returned value is a ticket data, it's used as the latest data.
  Else if ticket-latest is defined in the system, it's called and
  the return value is used as the latest data.
  Else, argument ticket data is used as the latest data.

* ticket-eraser (optional)

It's a function for erasion of ticket.
It receives the arguments which are a project configuration, a ticket data.
It should raise error if the erasion is failed.

* ticket-unique-string (required)

It's a function for getting a unique string of ticket.
It receives an argument which is a ticket data.
It should return a unique string in all tickets of the system.

* ticket-latest (optional)

It's a function for fetching ticket latest data.
It receives the arguments which are a project configuration, a ticket data.
It should return a latest data of the ticket.

* summary-format (required)

It's a function for a declare of entry format in summary buffer.
It receives the arguments which are a project configuration, a query configuration.
It should return a list of plist has the following properties as a declare of column.
  :name     (required) a symbol as property name of ticket.
  :label    (optional) a shown text in column header. If nil, :name value is used as string.
  :size     (optional) a integer of column width. If nil, the width is no limit.
  :sort     (optional) how to sort entries by this column.
                       If nil, this column cannot be used for sorting.
                       If t, sort by comparing the string value printed in the column.
                       Otherwise, it should be a function suitable for `sort' and receives two ticket data.
  :ellipsis (optional) If nil, do `truncate-string-to-width' for column text using the value.
  :format   (optional) a string as first argument for `format' or a function for the string.
                       The function receives a ticket data and should return the string.
  :face     (optional) If nil, do `propertize' for column text using the value.

  About other properties, see `tabulated-list-format'.

* conflict-checker (optional)

How to find conflict of ticket in the registration flow.
Conflict means other user or application has changed same property of the ticket while
user gets ticket from the system and changes that.
If this option is nil, only the properties which user has changed are checked using `equal'.
If 'any, all properties are checked using `equal'.
If the registration for the system handles only the updated properties, nil is better.
Else, 'any should be specified.
Also, ticket-latest function is required in their cases.
Otherwise, it should be a function receives the arguments which are before ticket data,
changed ticket data and returns a list of the conflict if conflict is found.
The format should be ((NAME1 . (CHANGED1 . LATEST1)) (NAME2 . (CHANGED2 . LATEST2))...).

* conflict-resolver (optional)

It's a function for resolving found conflict in the registration flow.
  If nil, `bts:ticket-resolve-conflict' is used.
It receives the arguments which are a ticket data, the found conflict.
  Conflict format is ((NAME1 . (CHANGED1 . LATEST1)) (NAME2 . (CHANGED2 . LATEST2))...).
It should repair the ticket data and return t if the conflict has been resolved."
  (yaxception:$
    (yaxception:try
      (bts--debug "start system regist. system[%s]" system)
      (when (or (not (bts:system-p                               system))
                (not (bts:system-name                            system))
                (not (functionp (bts:system-project-view         system)))
                (not (functionp (bts:system-query-view           system)))
                (not (functionp (bts:system-ticket-single-view   system)))
                (not (functionp (bts:system-ticket-fetcher       system)))
                (not (functionp (bts:system-ticket-register      system)))
                (not (functionp (bts:system-ticket-unique-string system)))
                (not (functionp (bts:system-summary-format       system))))
        (yaxception:throw 'bts:invalid-argument))
      (puthash (bts:system-name system) system bts::system-hash)
      (bts--info "registed system : %s" (bts:system-name system)))
    (yaxception:catch 'error e
      (bts--error "failed system regist : %s" (yaxception:get-text e))
      (yaxception:throw e))))

(defun bts:system-get (sysnm &optional no-error)
  "Return a structure of `bts:system' from SYSNM."
  (bts:aif (gethash (intern (bts::system-normalize-name sysnm)) bts::system-hash)
      it
    (bts--error "failed system get from sysnm[%s]" sysnm)
    (when (not no-error)
      (error "System '%s' is not yet registed." sysnm))))


;;;;;;;;;;;;;
;; Project

(defvar bts::project-store nil
  "Value is a return of `plstore-open'.")

(defsubst bts::project-get-store-key (projnm sysnm &optional no-error make-effort)
  (let ((fmt "%s [%s]"))
    (if (and (and (stringp projnm) (not (string= projnm "")))
             (or (symbolp sysnm) (stringp sysnm)))
        (format fmt projnm sysnm)
      (when (not no-error)
        (bts--error "failed project get store key from projnm[%s] sysnm[%s]" projnm sysnm)
        (yaxception:throw 'bts:invalid-argument))
      (when make-effort
        (format fmt projnm sysnm)))))

(defsubst bts::project-load-config (&optional force)
  (bts::plstore-load 'bts::project-store bts:project-cache-file force))

(defun bts::project-get-new-id ()
  (bts--debug "start project get new id.")
  (let ((exists-id-list (loop for proj in (bts:project-get-config-all)
                              collect (string-to-number (plist-get proj :bts-id)))))
    (loop with i = 1
          while t
          if (not (memq i exists-id-list))
          return (progn (bts--info "got project new id : %s" i)
                        i)
          do (incf i))))

(defun* bts::project-select (&key method multiple no-error)
  (let* ((allprops (bts:project-get-config-all))
         (projkeys (loop for p in allprops
                         collect (bts:project-get-unique-string p no-error t)))
         (selected (bts::select-something :description "Project"
                                          :candidates projkeys
                                          :require t
                                          :default (car projkeys)
                                          :multiple multiple
                                          :method method
                                          :no-error no-error)))
    (if multiple
        (-filter (lambda (p)
                   (member (bts:project-get-unique-string p no-error t)
                           selected))
                 allprops)

      (-find (lambda (p)
               (string= (bts:project-get-unique-string p no-error t)
                        selected))
             allprops))))

(defun bts::project-validate-name (ctx value &rest args)
  (let* ((allkeys (loop for e in (bts:project-get-config-all)
                        collect (bts::project-get-store-key (plist-get e :bts-name)
                                                            (plist-get e :bts-system))))
         (sysnm (bts:widget-get-value 'bts-system))
         (currkey (bts::project-get-store-key value sysnm))
         (currproj (bts:awhen (bts:widget-get-local-attribute 'bts-id)
                     (bts:project-get-config-by-id it)))
         (initkey (when currproj
                    (bts::project-get-store-key (plist-get currproj :bts-name)
                                                (plist-get currproj :bts-system)))))
    (bts:widget-validation-uniq ctx value
                                :name 'bts-project
                                :check-list allkeys
                                :value currkey
                                :init-value initkey)))

(bts:regist-message 'bts-project-stored
  t         "Stored project configuration of %s"
  'Japanese "プロジェクト'%s'の設定を保存しました")

(defun bts::project-submit (mdl)
  (let* ((sysnm (assoc-default 'bts-system mdl))
         (projnm (assoc-default 'bts-project mdl))
         (projid (bts:widget-get-local-attribute 'bts-id))
         (realfunc (or (bts:widget-get-local-attribute 'bts-system-submit-action)
                       'bts:make-plist-from-alist))
         (prop (funcall realfunc mdl)))
    (bts:project-store-config projid projnm sysnm prop)
    (bts:show-message (bts:get-message 'bts-project-stored projnm))
    (bts--debug "finished project submit for %s" projnm)))

(defvar bts::default-project-view-buffer-name "*BTS: Project*")
(defun bts::project-open-view (sys &optional proj)
  (let* ((sysnm (bts:system-name sys))
         (arg (progn (bts--debug "call project view of %s" sysnm)
                     (funcall (bts:system-project-view sys) (copy-sequence proj))))
         (buf (or (plist-get arg :buffer)
                  (wmvc:get-new-buffer bts::default-project-view-buffer-name)))
         (layout `((:type const :name bts-system :label "SystemType") BR
                   (:type text :name bts-project :label "ProjectName" :size 30
                          :require t :validate bts::project-validate-name)
                   BR
                   ,@(plist-get arg :layout)))
         (defs (append `((bts-system  . ,sysnm)
                         (bts-project . ,(plist-get proj :bts-name)))
                       (plist-get arg :defaults)))
         (attrs (append `((bts-system-submit-action . ,(plist-get arg :submit-action))
                          (bts-id                   . ,(plist-get proj :bts-id)))
                        (plist-get arg :attributes))))
    (pop-to-buffer
     (bts:widget-build-buffer :buffer buf
                              :layout layout
                              :flex-layout (plist-get arg :flex-layout)
                              :defaults defs
                              :flex-defaults (plist-get arg :flex-defaults)
                              :title (if proj "Update Project" "Add Project")
                              :submit-title "Regist"
                              :submit-action 'bts::project-submit
                              :attributes attrs))))

(defun bts:project-p (project)
  "Whether PROJECT is a project configuration."
  (when (ignore-errors (and (plist-get project :bts-id)
                            (plist-get project :bts-name)
                            (plist-get project :bts-system)))
    t))

(defun bts:project-get-config-all ()
  "Return all project configurations."
  (bts::project-load-config)
  (loop for e in (plstore-find bts::project-store nil)
        collect (cdr e)))

(defun bts:project-get-config-by-id (projid)
  "Return a project configuration of PROJID.

PROJID is a integer/string."
  (bts::project-load-config)
  (let ((projid (if (stringp projid)
                    projid
                  (number-to-string projid))))
    (bts:awhen (plstore-find bts::project-store `(:bts-id (,projid)))
      (cdr (pop it)))))

(defun bts:project-get-config-by-name (projnm sysnm)
  "Return a project configuration of PROJNM/SYSNM.

PROJNM is a string.
SYSNM is a string/symbol."
  (bts::project-load-config)
  (let* ((sysnm (bts::system-normalize-name sysnm)))
    (bts:awhen (plstore-find bts::project-store `(:bts-name (,projnm) :bts-system (,sysnm)))
      (cdr (pop it)))))

(defun bts:project-get-config-name (project)
  "Return a name of PROJECT.

PROJECT is a plist as project configuration."
  (if (not (bts:project-p project))
      (yaxception:throw 'bts:invalid-argument)
    (plist-get project :bts-name)))

(defun bts:project-get-system (project)
  "Return a `bts:system' object of PROJECT.

PROJECT is a plist as project configuration."
  (if (not (bts:project-p project))
      (yaxception:throw 'bts:invalid-argument)
    (bts:system-get (plist-get project :bts-system))))

(defun bts:project-get-unique-string (project &optional no-error make-effort)
  "Return a unique string of PROJECT.

PROJECT is a plist as project configuration."
  (bts::project-get-store-key (plist-get project :bts-name)
                              (plist-get project :bts-system)
                              no-error
                              make-effort))

(defun bts:project-store-config (projid projnm sysnm prop)
  "Store a project configuration as PROP of PROJID into `bts:project-cache-file'.

PROJID is a integer/string. If nil, means to add a new configuration.
PROJNM is a string.
SYSNM is a string/symbol.
PROP is a plist as project configuration.

If secret property exists (eg. password) in PROP,
the property name should be added a \"secret-\" prefix.
For example, If the name is :password, it should be :secret-password in PROP.
Then, the property value is encrypted by EasyPG when it's stored into file."
  (bts--debug "start project store config. projid[%s] projnm[%s] sysnm[%s]" projid projnm sysnm)
  (bts::project-load-config)
  (let* ((sysnm (bts::system-normalize-name sysnm))
         (key (bts::project-get-store-key projnm sysnm))
         (id (cond ((stringp projid) projid)
                   (projid           (number-to-string projid))
                   (t                (number-to-string (bts::project-get-new-id)))))
         (currconf (bts:project-get-config-by-id id)))
    (when currconf
      (bts:project-delete-config (plist-get currconf :bts-name)
                                 (plist-get currconf :bts-system)))
    (bts::plstore-save bts::project-store
                       key
                       `(:bts-id ,id :bts-name ,projnm :bts-system ,sysnm ,@prop))))

(defun bts:project-delete-config (projnm sysnm)
  "Delete a project configuration of PROJNM/SYSNM in `bts:project-cache-file'.

PROJNM is a string.
SYSNM is a string/symbol."
  (bts--debug "start project delete config. projnm[%s] sysnm[%s]" projnm sysnm)
  (bts::project-load-config)
  (bts::plstore-remove bts::project-store (bts::project-get-store-key projnm sysnm)))

;;;###autoload
(defun bts:project-new (system)
  "Popup a widget buffer for creating a new project configuration for SYSTEM.

SYSTEM is a `bts:system' object."
  (interactive
   (list (bts::system-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start project new.")
      (bts::project-open-view system))
    (yaxception:catch 'error e
      (bts--fatal "failed project new : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed project new : %s" (yaxception:get-text e)))))

;;;###autoload
(defun bts:project-update (project)
  "Popup a widget buffer for updating PROJECT.

PROJECT is a plist as project configuration."
  (interactive
   (list (bts::project-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start project update.")
      (let ((sysnm (plist-get project :bts-system)))
        (bts::project-open-view (bts:system-get sysnm) project)))
    (yaxception:catch 'error e
      (bts--fatal "failed project update : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed project update : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-project-remove-confirm
  t         "Remove project and the belonged queries configuration of '%s'?"
  'Japanese "プロジェクト'%s'とそれに属するクエリの設定を削除してよろしいですか？")

;;;###autoload
(defun bts:project-remove (project)
  "Remove PROJECT with confirm.

PROJECT is a plist as project configuration."
  (interactive
   (list (bts::project-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start project remove.")
      (let* ((projnm (plist-get project :bts-name))
             (sysnm (plist-get project :bts-system))
             (key (bts:project-get-unique-string project)))
        (if (not (y-or-n-p (bts:get-message 'bts-project-remove-confirm key)))
            (bts:show-message "Quit")
          (bts:project-delete-config projnm sysnm)
          (bts:query-delete-config-all project)
          (bts:show-message "Removed configuration"))))
    (yaxception:catch 'error e
      (bts--fatal "failed project remove : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed project remove : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-project-remove-all-confirm
  t         "Remove all projects and queries configuration?"
  'Japanese "全てのプロジェクト及びクエリの設定を削除してよろしいですか？")

;;;###autoload
(defun bts:project-remove-all ()
  "Remove all project configurations with confirm."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start project remove all.")
      (if (not (y-or-n-p (bts:get-message 'bts-project-remove-all-confirm)))
          (bts:show-message "Quit")
        (bts::plstore-clear 'bts::project-store)
        (bts:query-delete-config-all nil t)
        (bts:show-message "Removed configuration")))
    (yaxception:catch 'error e
      (bts--fatal "failed project remove all : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed project remove all : %s" (yaxception:get-text e)))))


;;;;;;;;;;;
;; Query

(defvar bts::query-store nil
  "Value is a return of `plstore-open'.")

(defsubst bts::query-get-format-string (fmt fmt-args no-error make-effort)
  (apply 'format
         fmt
         (loop with idx = 1
               for e in fmt-args
               if (and (not no-error)
                       (or (not (stringp e)) (string= e "")))
               do (progn
                    (bts--error "failed query get format string : index[%s] value[%s]" idx e)
                    (yaxception:throw 'bts:invalid-argument))
               collect e
               do (incf idx))))

(defsubst bts::query-get-store-key (projid querynm &optional no-error make-effort)
  (let* ((fmt "Project[%s] - %s")
         (proj (ignore-errors (bts:project-get-config-by-id projid)))
         (projid (plist-get proj :bts-id)))
    (bts::query-get-format-string fmt `(,projid ,querynm) no-error make-effort)))

(defsubst bts::query-load-config (&optional force)
  (bts::plstore-load 'bts::query-store bts:query-cache-file force))

(defun bts::query-get-new-id ()
  (bts--debug "start query get new id.")
  (let ((exists-id-list (loop for q in (bts:query-get-config-all)
                              collect (string-to-number (plist-get q :bts-id)))))
    (loop with i = 1
          while t
          if (not (memq i exists-id-list))
          return (progn (bts--info "got query new id : %s" i)
                        i)
          do (incf i))))

(defun* bts::query-select (&key method multiple no-error)
  (let* ((allquery (bts:query-get-config-all))
         (querykeys (loop for q in allquery
                          collect (bts:query-get-description q
                                                             :no-error no-error
                                                             :make-effort t)))
         (selected (bts::select-something :description "Query"
                                          :candidates querykeys
                                          :require t
                                          :default (car querykeys)
                                          :multiple multiple
                                          :method method
                                          :no-error no-error)))
    (if multiple
        (-filter (lambda (q)
                   (member (bts:query-get-description q :no-error no-error :make-effort t)
                           selected))
                 allquery)
      (-find (lambda (q)
               (string= (bts:query-get-description q :no-error no-error :make-effort t)
                        selected))
             allquery))))

(defun bts::query-validate-name (ctx value &rest args)
  (let* ((allkeys (loop for e in (bts:query-get-config-all)
                        collect (bts::query-get-store-key (plist-get e :bts-project)
                                                          (plist-get e :bts-name))))
         (projid (bts:widget-get-local-attribute 'bts-project-id))
         (currkey (bts::query-get-store-key projid value))
         (query (bts:awhen (bts:widget-get-local-attribute 'bts-id)
                  (bts:query-get-config-by-id it)))
         (initkey (when query
                    (bts::query-get-store-key projid
                                              (plist-get query :bts-name)))))
    (bts:widget-validation-uniq ctx value
                                :name 'bts-query
                                :check-list allkeys
                                :value currkey
                                :init-value initkey)))

(bts:regist-message 'bts-query-stored
  t         "Stored query configuration of %s"
  'Japanese "クエリ'%s'の設定を保存しました")

(defun bts::query-submit (mdl)
  (let* ((querynm (assoc-default 'bts-query mdl))
         (queryid (bts:widget-get-local-attribute 'bts-id))
         (projid (bts:widget-get-local-attribute 'bts-project-id))
         (proj (bts:project-get-config-by-id projid))
         (realfunc (or (bts:widget-get-local-attribute 'bts-system-submit-action)
                       'bts:make-plist-from-alist))
         (prop (funcall realfunc mdl)))
    (bts:query-store-config queryid proj querynm prop)
    (bts:show-message (bts:get-message 'bts-query-stored querynm))
    (bts--debug "finished query submit for %s" querynm)))

(defvar bts::default-query-view-buffer-name "*BTS: Query*")
(defun bts::query-open-view (proj &optional query)
  (let* ((projnm (plist-get proj :bts-name))
         (sysnm (plist-get proj :bts-system))
         (sys (bts:system-get sysnm))
         (arg (progn (bts--debug "call query view of %s" sysnm)
                     (funcall (bts:system-query-view sys)
                              (copy-sequence proj)
                              (copy-sequence query))))
         (buf (or (plist-get arg :buffer)
                  (wmvc:get-new-buffer bts::default-query-view-buffer-name)))
         (layout `((:type const :name bts-system :label "SystemType") BR
                   (:type const :name bts-project :label "ProjectName") BR
                   (:type text :name bts-query :label "QueryName" :size 30
                          :require t :validate bts::query-validate-name)
                   BR
                   ,@(plist-get arg :layout)))
         (defs (append `((bts-system  . ,sysnm)
                         (bts-project . ,projnm)
                         (bts-query   . ,(plist-get query :bts-name)))
                       (plist-get arg :defaults)))
         (attrs (append `((bts-system-submit-action . ,(plist-get arg :submit-action))
                          (bts-project-id           . ,(plist-get proj :bts-id))
                          (bts-id                   . ,(plist-get query :bts-id)))
                        (plist-get arg :attributes))))
    (pop-to-buffer
     (bts:widget-build-buffer :buffer buf
                              :layout layout
                              :flex-layout (plist-get arg :flex-layout)
                              :defaults defs
                              :flex-defaults (plist-get arg :flex-defaults)
                              :title (if query "Update Query" "Add Query")
                              :submit-title "Regist"
                              :submit-action 'bts::query-submit
                              :attributes attrs))))

(defun bts:query-p (query)
  "Whether QUERY is a query configuration."
  (when (ignore-errors (and (plist-get query :bts-id)
                            (plist-get query :bts-name)
                            (plist-get query :bts-project)))
    t))

(defun bts:query-get-config-all (&optional project)
  "Return all query configurations belongs to all projects or PROJECT.

PROJECT is a plist as project configuration."
  (bts::query-load-config)
  (loop with projid = (plist-get project :bts-id)
        with keys = (when projid
                      `(:bts-project (,projid)))
        for e in (plstore-find bts::query-store keys)
        collect (cdr e)))

(defun bts:query-get-config-by-id (queryid)
  "Return a query configuration of QUERYID.

QUERYID is a integer/string."
  (bts::query-load-config)
  (let ((queryid (if (stringp queryid)
                     queryid
                   (number-to-string queryid))))
    (bts:awhen (plstore-find bts::query-store `(:bts-id (,queryid)))
      (cdr (pop it)))))

(defun bts:query-get-config-by-name (project querynm)
  "Return a query configuration of QUERYNM belongs to PROJECT.

PROJECT is a plist as project configuration.
QUERYNM is a string."
  (bts::query-load-config)
  (let ((projid (plist-get project :bts-id)))
    (bts:awhen (plstore-find bts::query-store `(:bts-name (,querynm) :bts-project (,projid)))
      (cdr (pop it)))))

(defun bts:query-get-config-name (query)
  "Return a name of QUERY.

QUERY is a plist as query configuration."
  (if (not (bts:query-p query))
      (yaxception:throw 'bts:invalid-argument)
    (plist-get query :bts-name)))

(defun bts:query-get-project (query)
  "Return a project configuration of QUERY.

QUERY is a plist as query configuration."
  (if (not (bts:query-p query))
      (yaxception:throw 'bts:invalid-argument)
    (bts:project-get-config-by-id (plist-get query :bts-project))))

(defun bts:query-get-unique-string (query &optional no-error make-effort)
  "Return a unique string of QUERY.

QUERY is a plist as query configuration."
  (bts::query-get-store-key (plist-get query :bts-project)
                            (plist-get query :bts-name)
                            no-error
                            make-effort))

(defun* bts:query-get-description (query &key no-error make-effort short)
  "Return a description string of QUERY.

QUERY is a plist as query configuration."
  (let* ((fmt (if short "%s[%s]:%s" "%s [%s] - %s"))
         (proj (ignore-errors (bts:project-get-config-by-id (plist-get query :bts-project))))
         (projnm (plist-get proj :bts-name))
         (sysnm (plist-get proj :bts-system))
         (querynm (ignore-errors (plist-get query :bts-name))))
    (bts::query-get-format-string fmt `(,projnm ,sysnm ,querynm) no-error make-effort)))

(defun bts:query-store-config (queryid project querynm query)
  "Store a query configuration as QUERY of QUERYID belongs to PROJECT into `bts:query-cache-file'.

QUERYID is a integer/string. If nil, means to add a new configuration.
PROJECT is a plist as project configuration.
QUERYNM is a string.
QUERY is a plist as query configuration.

If secret property exists (eg. password) in QUERY,
the property name should be added a \"secret-\" prefix.
For example, If the name is :password, it should be :secret-password in QUERY.
Then, the property value is encrypted by EasyPG when it's stored into file."
  (bts--debug* "start query store config. queryid[%s] querynm[%s] project[%s]"
               queryid querynm (bts:project-get-unique-string project t))
  (bts::query-load-config)
  (let* ((projid (plist-get project :bts-id))
         (key (bts::query-get-store-key projid querynm))
         (id (cond ((stringp queryid) queryid)
                   (queryid           (number-to-string queryid))
                   (t                 (number-to-string (bts::query-get-new-id)))))
         (currconf (bts:query-get-config-by-id id)))
    (when currconf
      (bts:query-delete-config project (plist-get currconf :bts-name)))
    (bts::plstore-save bts::query-store
                       key
                       `(:bts-id ,id :bts-name ,querynm :bts-project ,projid ,@query))))

(defun bts:query-delete-config (project querynm)
  "Delete a query configuration of QUERYNM belongs to PROJECT in `bts:query-cache-file'.

PROJECT is a plist as project configuration.
QUERYNM is a string."
  (bts--debug* "start query delete config. querynm[%s] project[%s]"
               querynm (bts:project-get-unique-string project t))
  (bts::query-load-config)
  (bts::plstore-remove bts::query-store
                       (bts::query-get-store-key (plist-get project :bts-id) querynm)))

(defun bts:query-delete-config-all (project &optional all-clear)
  "Delete all query configurations belongs to PROJECT in `bts:query-cache-file'.

PROJECT is a plist as project configuration.
If ALL-CLEAR is non-nil and PROJID is nil,
delete all query configuration in `bts:query-cache-file'."
  (bts--debug* "start query delete config all. project[%s] all-clear[%s]"
               (bts:project-get-unique-string project t) all-clear)
  (bts::query-load-config)
  (if (and all-clear (not project))
      (bts::plstore-clear 'bts::query-store)
    (loop with projid = (plist-get project :bts-id)
          for found in (plstore-find bts::query-store `(:bts-project (,projid)))
          for key = (car found)
          do (bts::plstore-remove bts::query-store key))))

;;;###autoload
(defun bts:query-new (project)
  "Popup a widget buffer for creating a new query configuration belongs to PROJECT.

PROJECT is a plist as project configuration."
  (interactive
   (list (bts::project-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start query new.")
      (bts::query-open-view project))
    (yaxception:catch 'error e
      (bts--fatal "failed query new : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed query new : %s" (yaxception:get-text e)))))

;;;###autoload
(defun bts:query-update (query)
  "Popup a widget buffer for updating QUERY.

QUERY is a plist as query configuration."
  (interactive
   (list (bts::query-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start query update.")
      (let ((projid (plist-get query :bts-project)))
        (bts::query-open-view (bts:project-get-config-by-id projid) query)))
    (yaxception:catch 'error e
      (bts--fatal "failed query update : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed query update : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-query-remove-confirm
  t         "Remove query configuration of '%s'?"
  'Japanese "クエリ'%s'の設定を削除してよろしいですか？")

;;;###autoload
(defun bts:query-remove (query)
  "Remove QUERY with confirm.

QUERY is a plist as query configuration."
  (interactive
   (list (bts::query-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start query remove.")
      (let* ((querynm (plist-get query :bts-name))
             (proj (bts:project-get-config-by-id (plist-get query :bts-project)))
             (key (bts:query-get-description query)))
        (if (not (y-or-n-p (bts:get-message 'bts-query-remove-confirm key)))
            (bts:show-message "Quit")
          (bts:query-delete-config proj querynm)
          (bts:show-message "Removed configuration"))))
    (yaxception:catch 'error e
      (bts--fatal "failed query remove : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed query remove : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-query-remove-all-confirm
  t         "Remove all query configuration?"
  'Japanese "全てのクエリの設定を削除してよろしいですか？")

;;;###autoload
(defun bts:query-remove-all ()
  "Remove all query configurations with confirm."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start query remove all.")
      (if (not (y-or-n-p (bts:get-message 'bts-query-remove-all-confirm)))
          (bts:show-message "Quit")
        (bts::plstore-clear 'bts::query-store)
        (bts:show-message "Removed configuration")))
    (yaxception:catch 'error e
      (bts--fatal "failed query remove all : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed query remove all : %s" (yaxception:get-text e)))))


;;;;;;;;;;;;;;;;;;;;
;; Ticket Utility

(defun bts:ticket-p (ticket)
  "Whether TICKET is a ticket data."
  (when (ignore-errors (plist-get ticket :bts-project))
    t))

(bts:regist-message 'bts-project-not-found
  t         "Can't find a project configuration of ticket"
  'Japanese "チケットの属するプロジェクトが見つかりません")

(defun bts:ticket-get-project (ticket)
  "Return the project configuration TICKET belongs to.

TICKET is a plist as ticket data."
  (if (not (bts:ticket-p ticket))
      (yaxception:throw 'bts:invalid-argument)
    (bts:aif (bts:project-get-config-by-id (plist-get ticket :bts-project))
        it
      (bts--fatal "failed ticket get project from projid[%s]\nticket...%s"
                  (plist-get ticket :bts-project) ticket)
      (yaxception:throw 'bts:error
                        :msg (bts:get-message 'bts-project-not-found)))))

(defun bts:ticket-get-system (ticket)
  "Return a `bts:system' object TICKET belongs to.

TICKET is a plist as ticket data."
  (bts:project-get-system (bts:ticket-get-project ticket)))

(defun bts:ticket-get-unique-string (ticket)
  "Return a unique string of TICKET.

TICKET is a plist as ticket data."
  (funcall (bts:system-ticket-unique-string (bts:ticket-get-system ticket)) ticket))

(defun bts:ticket-belong-to-same-project-p (tickets)
  "Whether all of TICKETS belongs to same project.

TICKETS is a list of plist as ticket data."
  (loop with projid = (plist-get (nth 0 tickets) :bts-project)
        for tic in tickets
        always (equal (plist-get tic :bts-project) projid)))

(bts:regist-message 'bts-ticket-latest-undefined
  t         "Can't get latest ticket because method is not defined in %s"
  'Japanese "%sに機能が定義されていないため、最新のチケットを取得できません")

(bts:regist-message 'bts-ticket-latest-failed
  t         "Failed to fetch latest ticket of %s"
  'Japanese "%sの最新のチケットの取得に失敗しました")

(defun bts:ticket-get-latest (ticket)
  "Return a latest data of TICKET.

TICKET is a plist as ticket data."
  (bts--debug* "start ticket get latest. ticket[%s]"
               (ignore-errors (bts:ticket-get-unique-string ticket)))
  (let* ((ticstr (bts:ticket-get-unique-string ticket))
         (proj (copy-sequence (bts:ticket-get-project ticket)))
         (sys (bts:project-get-system proj))
         (sysnm (bts:system-name sys))
         (fetcher (bts:system-ticket-latest sys))
         (ret (if (not (functionp fetcher))
                  (yaxception:throw
                    'bts:error
                    :msg (bts:get-message 'bts-ticket-latest-undefined sysnm))
                (yaxception:$
                  (yaxception:try
                    (bts--debug "call ticket latest of %s" sysnm)
                    (funcall fetcher proj (copy-sequence ticket)))
                  (yaxception:catch 'error e
                    (bts--error "failed to call ticket latest of %s : %s\n%s"
                                sysnm
                                (yaxception:get-text e)
                                (yaxception:get-stack-trace-string e))
                    (yaxception:throw
                      'bts:error
                      :msg (bts:get-message 'bts-ticket-latest-failed ticstr)))))))
    (cond ((not ret)
           (yaxception:throw
             'bts:error
             :msg (bts:get-message 'bts-ticket-latest-failed ticstr)))
          ((not (string= ticstr (funcall (bts:system-ticket-unique-string sys) ret)))
           (bts--error "got latest ticket value is invalid of %s" ticstr)
           (yaxception:throw
             'bts:error
             :msg (bts:get-message 'bts-ticket-latest-failed ticstr)))
          (t
           (plist-put ret :bts-project (plist-get proj :bts-id))
           ret))))

(defun bts:ticket-simple-update (ticket model)
  "Update TICKET about all non-nil properties in MODEL.

TICKET is a plist as ticket data.
MODEL is a alist as model. For detail, see `bts:widget-get-model'."
  (bts--debug* "start ticket simple update for %s"
               (ignore-errors (bts:ticket-get-unique-string ticket)))
  (let ((ret (loop for (k . v) in model
                   if (and (not (bts:internal-symbol-p k))
                           v)
                   collect (progn (plist-put ticket (bts:conv-symbol-to-keyword k) v)
                                  k))))
    (bts--info "finished ticket simple update : %s" (mapconcat 'symbol-name ret ", "))))

(bts:regist-message 'bts-conflict-description
  t         "Conflict has been found. Select a action for below property.\n\n"
  'Japanese "コンフリクトが見つかりました。どうするか選択して下さい。\n\n")

(defun bts:ticket-multi-update (ticket model)
  "Update TICKET about non-nil and not empty properties in MODEL.

TICKET is a plist as ticket data.
MODEL is a alist as model. For detail, see `bts:widget-get-model'."
  (bts--debug* "start ticket multi update for %s"
               (ignore-errors (bts:ticket-get-unique-string ticket)))
  (loop with updated = nil
        for (k . v) in model
        for vstr = (cond ((stringp v) (s-trim v))
                         (v           (format "%s" v))
                         (t           ""))
        ;; Update only if widget has some inputed/selected value.
        if (and (not (bts:internal-symbol-p k))
                (not (string= vstr "")))
        do (progn (plist-put ticket (bts:conv-symbol-to-keyword k) v)
                  (push k updated))
        finally
        do (bts--info "finished ticket multi update : %s" (mapconcat 'symbol-name updated ", "))))

(defun bts:ticket-setup-conflict-buffer (buffer project-name system-name ticket-string
                                                property-name local-value latest-value)
  "Setup a conflict display buffer."
  (bts--debug* "start ticket setup conflict buffer. buffer[%s] project-name[%s] system-name[%s]
ticket-string... %s
property-name... %s
local-value... %s
latest-value... %s"
               buffer project-name system-name ticket-string property-name local-value latest-value)
  (let ((writer (lambda (v)
                  (let* ((v (format "%s" v))
                         (pt (point))
                         (ov (progn (insert v)
                                    (make-overlay pt (point)))))
                    (overlay-put ov 'face 'widget-field)
                    (insert "\n\n"))))
        (labeler (lambda (v)
                   (bts::wid-format-label nil v nil)))
        (propnm (if (stringp property-name) property-name (symbol-name property-name))))
    (with-current-buffer buffer
      (erase-buffer)
      (remove-overlays)
      (insert (propertize "Resolve Conflict" 'face 'info-title-1) "\n\n")
      (insert (bts:get-message 'bts-conflict-description))
      (insert (funcall labeler "      ProjectName") project-name "\n")
      (insert (funcall labeler "       SystemType") (bts::system-normalize-name system-name) "\n")
      (insert (funcall labeler "           Ticket") ticket-string "\n")
      (insert (funcall labeler "Conflict Property") propnm "\n\n")
      (insert (propertize "Local value by you" 'face 'info-title-4) "\n")
      (funcall writer local-value)
      (insert (propertize "Latest value in system" 'face 'info-title-4) "\n")
      (funcall writer latest-value))
    buffer))

(bts:regist-message 'bts-conflict-use-local
  t         "update by your value"
  'Japanese "処理を続行")

(bts:regist-message 'bts-conflict-use-system
  t         "keep system value"
  'Japanese "この属性は更新しない")

(bts:regist-message 'bts-conflict-quit
  t         "quit this ticket update"
  'Japanese "このチケットは更新しない")

(bts:regist-message 'bts-conflict-inform-invalid
  t         "Input y/n/q"
  'Japanese "y/n/qを入力して下さい")

(defvar bts::default-resolve-conflict-buffer-name " *BTS: Conflict*")
(defun bts:ticket-resolve-conflict (ticket conflicts)
  "Update TICKET by asking user how to handle the property in CONFLICTS.

TICKET is a plist as ticket data.
CONFLICTS is a alist. The format is ((NAME1 . (CHANGED1 . LATEST1)) (NAME2 . (CHANGED2 . LATEST2))...)."
  (bts--debug* "start ticket resolve conflict for %s"
               (ignore-errors (bts:ticket-get-unique-string ticket)))
  (loop initially (bts--trace "start loop")
        with update-prop = nil
        with local-selected-p = nil
        with buf = (window-buffer
                    (display-buffer (get-buffer-create bts::default-resolve-conflict-buffer-name)))
        with ticstr = (bts:ticket-get-unique-string ticket)
        with projnm = (plist-get (bts:ticket-get-project ticket) :bts-name)
        with sysnm = (bts:system-name (bts:ticket-get-system ticket))
        with prompt = (format " y:[%s]  n:[%s]  q:[%s] "
                              (bts:get-message 'bts-conflict-use-local)
                              (bts:get-message 'bts-conflict-use-system)
                              (bts:get-message 'bts-conflict-quit))
        with answer-map = `(("y" . use-local)
                            ("n" . use-latest)
                            ("q" . quit))
        for c in conflicts
        for propnm = (car c)
        for local-val = (car (cdr c))
        for latest-val = (cdr (cdr c))
        for mtd = (progn
                    (bts:ticket-setup-conflict-buffer buf projnm sysnm ticstr propnm local-val latest-val)
                    (loop while t
                          for ret = (assoc-default (s-trim (read-string prompt)) answer-map)
                          if ret return ret
                          do (message (bts:get-message 'bts-conflict-inform-invalid))
                          do (sleep-for 2)))
        do (bts--info "got how to resolve conflict of %s : %s" propnm mtd)
        if      (eq mtd 'quit)       return nil
        else if (eq mtd 'use-local)  do (setq local-selected-p t)
        else if (eq mtd 'use-latest) do (push propnm update-prop)
        finally
        return (progn
                 (kill-buffer buf)
                 ;; Update the properties applied latest value to
                 (dolist (p update-prop)
                   (let ((k (bts:conv-symbol-to-keyword p))
                         (v (cdr (cdr (assq p conflicts)))))
                     (plist-put ticket k v)))
                 ;; If user selected latest value in all properties, quit to regist
                 (when local-selected-p
                   t))))


;;;;;;;;;;;;;;;;;;;
;; Regist Ticket

(defun bts::ticket-update (tickets mdl &optional method)
  (bts--debug "start ticket update.")
  (let ((updator (or (bts:widget-get-local-attribute 'bts-system-submit-action)
                     method
                     'bts:ticket-simple-update)))
    (dolist (tic tickets)
      (funcall updator tic mdl)))
  (bts--info "finished ticket update"))

(defun bts::ticket-diff (tic1 tic2)
  (bts--debug* "start ticket diff for %s"
               (ignore-errors (bts:ticket-get-unique-string tic1)))
  (let ((prop-keys (-uniq (append (loop for (k v) on tic1 by 'cddr collect k)
                                  (loop for (k v) on tic2 by 'cddr collect k)))))
    (loop for p in prop-keys
          for oval1 = (plist-get tic1 p)
          for oval2 = (plist-get tic2 p)
          ;; Consider nil equals a empty string
          for val1 = (or oval1 "")
          for val2 = (or oval2 "")
          if (not (equal val1 val2))
          collect (let ((propsym (bts:conv-keyward-to-symbol p)))
                    (bts--debug "found diff : name[%s] old[%s] new[%s]" propsym oval1 oval2)
                    (cons propsym (cons oval1 oval2))))))

(defun bts::ticket-chk-diff-conflicts (tic diffs)
  (bts--debug* "start ticket chk diff conflicts for %s"
               (ignore-errors (bts:ticket-get-unique-string tic)))
  (let ((latest-tic (bts:ticket-get-latest tic)))
    (loop for d in (bts::ticket-diff tic latest-tic)
          for propsym = (car d)
          for latest-val = (cddr d)
          for curr-diff = (assq propsym diffs)
          for curr-val = (bts:awhen curr-diff (cddr it))
          ;; If same property has been updated at latest version in system
          ;; and the value is different between local and latest,
          ;; the property is conflicted.
          if (and curr-diff
                  (not (equal curr-val latest-val)))
          collect (progn
                    (bts--info "found conflict : name[%s] local[%s] latest[%s]"
                               propsym curr-val latest-val)
                    (cons propsym (cons curr-val latest-val))))))

(defun bts::ticket-chk-any-conflicts (tic diffs local-tic)
  (bts--debug* "start ticket chk any conflicts for %s"
               (ignore-errors (bts:ticket-get-unique-string tic)))
  (let ((latest-tic (bts:ticket-get-latest tic)))
    (loop for d in (bts::ticket-diff tic latest-tic)
          for propsym = (car d)
          for latest-val = (cddr d)
          for curr-diff = (assq propsym diffs)
          for curr-val = (plist-get local-tic (bts:conv-symbol-to-keyword propsym))
          ;; About the property has been updated at latest version in system,
          ;; if the property has not been updated to same value at local,
          ;; the property is conflicted.
          if (or (not curr-diff)
                 (not (equal curr-val latest-val)))
          collect (progn
                    (bts--info "found conflict : name[%s] local[%s] latest[%s]"
                               propsym curr-val latest-val)
                    (cons propsym (cons curr-val latest-val))))))

(bts:regist-message 'bts-ticket-regist-finished
  t         "Finished ticket regist : tried[%s] succeed[%s] skipped[%s] failed[%s]"
  'Japanese "Finished ticket regist : 実行[%s] 成功[%s] スキップ[%s] 失敗[%s]")

(defun bts::ticket-regist (tickets updatep before-tickets &optional project)
  (bts--debug* "start ticket regist. updatep[%s]
tickets... %s
before-tickets... %s
project... %s"
               updatep
               (ignore-errors (mapconcat 'bts:ticket-get-unique-string tickets ", "))
               (ignore-errors (mapconcat 'bts:ticket-get-unique-string before-tickets ", "))
               (ignore-errors (bts:project-get-unique-string project t)))
  (let (skipped succeed failed)
    (dolist (tic tickets)
      (yaxception:$
        (yaxception:try
          (let* ((before-tic (when updatep (pop before-tickets)))
                 (ticproj (bts:ticket-get-project tic))
                 (proj (copy-sequence (or project ticproj)))
                 (ticstr (if updatep
                             (bts:ticket-get-unique-string tic)
                           (format "new ticket of %s" (bts:project-get-unique-string proj))))
                 (sys (progn
                        (bts--info "start ticket regist flow for %s" ticstr)
                        (bts:show-message "Start ticket regist : %s" ticstr)
                        (bts:project-get-system proj)))
                 (sysnm (bts:system-name sys))
                 (register (bts:system-ticket-register sys))
                 (latest-fetcher (bts:system-ticket-latest sys))
                 (diffs (when updatep (bts::ticket-diff before-tic tic)))
                 (conflict-checker (bts:system-conflict-checker sys))
                 (conflict-resolver (or (bts:system-conflict-resolver sys)
                                        'bts:ticket-resolve-conflict))
                 (conflict-check-p (and (equal (plist-get proj :bts-id)
                                               (plist-get ticproj :bts-id))
                                        diffs))
                 (conflicts (when conflict-check-p
                              (cond
                               ((functionp conflict-checker)
                                (funcall conflict-checker proj (copy-sequence before-tic) (copy-sequence tic)))
                               ((not latest-fetcher)
                                (bts--info "skip ticket check conflict : no ticket latest function in %s" sysnm))
                               ((eq conflict-checker 'any)
                                (bts::ticket-chk-any-conflicts before-tic diffs tic))
                               (t
                                (bts::ticket-chk-diff-conflicts before-tic diffs))))))
            ;; Try to resolve conflict
            (when conflicts
              (if (funcall conflict-resolver tic conflicts)
                  (setq diffs (bts::ticket-diff before-tic tic))
                (bts--info "quited ticket resolve conflict for %s" ticstr)
                (bts:show-message "Skip ticket regist : quited resolve conflict for %s" ticstr)
                (setq tic nil)))
            (cond
             ((not (bts:ticket-p tic))
              (push tic skipped)
              (bts--info "skip ticket regist flow : data is not ticket"))
             ((and updatep (not diffs))
              (push tic skipped)
              (bts--info "skip ticket regist flow : ticket is not updated")
              (bts:show-message "Skip ticket regist : no changed '%s'" ticstr))
             (t
              ;; Start to regist into system
              (if updatep
                  (progn
                    (bts--info "found updated member : %s"
                               (mapconcat (lambda (e) (symbol-name (car e))) diffs ", "))
                    (bts--debug "call ticket register of %s as update for %s" sysnm ticstr)
                    (bts:show-message "Update '%s'..." ticstr))
                (bts--debug "call ticket register of %s for %s" sysnm ticstr)
                (bts:show-message "Regist %s..." ticstr))
              (bts:aif (funcall register proj (copy-sequence tic) diffs)
                  (progn
                    ;; Finished to regist into system
                    (bts--info "succeed ticket regist flow for %s" ticstr)
                    (bts:show-message "Succeed ticket regist : %s" ticstr)
                    (push tic succeed)
                    (plist-put it :bts-project (plist-get proj :bts-id))
                    (when updatep
                      ;; Try to update summary buffer by fetching latest ticket
                      (bts:aif (cond ((listp it)     it)
                                     (latest-fetcher (bts:ticket-get-latest tic))
                                     (t              tic))
                          (bts::summary-update-entry it)
                        (bts--error "skip summary update : failed to get a latest ticket")
                        (bts::summary-update-entry tic t))))
                ;; Not registed into system for some reason
                (push tic skipped)
                (bts--info "skip ticket regist flow : ticket register returns nil"))))))
        (yaxception:catch 'error e
          (bts--error "failed ticket regist flow : %s\n%s"
                      (yaxception:get-text e) (yaxception:get-stack-trace-string e))
          (bts:show-message "Failed ticket regist : %s" (yaxception:get-text e))
          (push tic failed)
          (when updatep (bts::summary-update-entry tic t)))))
    (let ((tried-len (number-to-string (length tickets)))
          (succeed-len (number-to-string (length succeed)))
          (skipped-len (number-to-string (length skipped)))
          (failed-len (number-to-string (length failed))))
      (bts--info "finished ticket regist. tried[%s] succeed[%s] skipped[%s] failed[%s]"
                 tried-len succeed-len skipped-len failed-len)
      (bts:show-message
       (bts:get-message 'bts-ticket-regist-finished
                        tried-len
                        (if (string= succeed-len "0")
                            "0"
                          (propertize succeed-len 'face 'bts:ticket-regist-message-succeed-face))
                        (if (string= skipped-len "0")
                            "0"
                          (propertize skipped-len 'face 'bts:ticket-regist-message-skipped-face))
                        (if (string= failed-len "0")
                            "0"
                          (propertize failed-len 'face 'bts:ticket-regist-message-failed-face)))))))

;;;;;;;;;;;;;;;;;
;; View Ticket

(defun bts::ticket-action-simple-regist (mdl updatep &optional updator)
  (bts--debug "start ticket action simple regist. updatep[%s]" updatep)
  (bts--trace* "model is\n%s" mdl)
  (let* ((tickets (bts:widget-get-local-attribute 'bts-current-tickets))
         (init-tics (when updatep (mapcar 'copy-sequence tickets))))
    (bts::ticket-update tickets mdl updator)
    (bts::ticket-regist tickets updatep init-tics)
    (bts--debug "finished ticket action simple regist")))

(defun bts::ticket-action-sequence-regist (mdl)
  (bts--debug "start ticket action sequence regist.")
  (bts--trace* "model is\n%s" mdl)
  (let* ((tickets (bts:widget-get-local-attribute 'bts-current-tickets))
         (init-tics (bts:widget-get-local-attribute 'bts-init-tickets))
         (lastidx (1- (length tickets)))
         (active-tic (nth lastidx tickets)))
    (bts::ticket-update (list active-tic) mdl)
    (bts::ticket-regist tickets t init-tics)))

(defun bts::ticket-action-next-view (mdl)
  (bts--debug "start ticket action next view.")
  (bts--trace* "model is\n%s" mdl)
  (let* ((tickets (bts:widget-get-local-attribute 'bts-current-tickets))
         (init-tics (bts:widget-get-local-attribute 'bts-init-tickets))
         (next-tics (bts:widget-get-local-attribute 'bts-next-tickets))
         (lastidx (1- (length tickets)))
         (active-tic (nth lastidx tickets)))
    (bts::ticket-update (list active-tic) mdl)
    (bts::ticket-open-sequence-view next-tics tickets init-tics)))

(defun bts::ticket-action-previous-view (mdl)
  (bts--debug "start ticket action previous view.")
  (bts--trace* "model is\n%s" mdl)
  (let* ((tickets (bts:widget-get-local-attribute 'bts-current-tickets))
         (init-tics (bts:widget-get-local-attribute 'bts-init-tickets))
         (next-tics (bts:widget-get-local-attribute 'bts-next-tickets))
         (previdx (- (length tickets) 2))
         (prev-tics (-slice tickets 0 previdx))
         (curr-tics (-slice tickets previdx)))
    (bts::ticket-open-sequence-view (append curr-tics next-tics) prev-tics init-tics)))

(defvar bts::default-ticket-view-buffer-name "*BTS: Ticket*")
(defun* bts::ticket-open-single-view (ticket &key
                                             (updatep t)
                                             submit-title
                                             submit-action
                                             cancel-title
                                             cancel-action
                                             attributes)
  (bts--debug* "start ticket open single view. updatep[%s]\nticket... %s"
               updatep (ignore-errors (bts:ticket-get-unique-string ticket)))
  (bts--trace* "full arguments are
ticket... %s
submit title... %s
submit action... %s
cancel title... %s
cancel action... %s
attributes... %s"
               ticket submit-title submit-action cancel-title cancel-action attributes)
  (let* ((proj (bts:ticket-get-project ticket))
         (sys (bts:project-get-system proj))
         (projnm (plist-get proj :bts-name))
         (sysnm (plist-get proj :bts-system))
         (arg (progn (bts--debug "call ticket single view of %s" sysnm)
                     (funcall (bts:system-ticket-single-view sys)
                              (copy-sequence proj)
                              (when updatep (copy-sequence ticket)))))
         (buf (or (plist-get arg :buffer)
                  (wmvc:get-new-buffer bts::default-ticket-view-buffer-name)))
         (layout `((:type const :name bts-system :label "SystemType" :unarrayed t) "  "
                   (:type const :name bts-project :label "ProjectName" :unarrayed t) BR
                   BR
                   ,@(plist-get arg :layout)))
         (defs (append `((bts-system  . ,sysnm)
                         (bts-project . ,projnm))
                       (plist-get arg :defaults)))
         (submit (or submit-action
                     `(lambda (mdl)
                        (bts::ticket-action-simple-regist mdl ,updatep))))
         (attrs (append
                 (bts:merge-alist
                  `((bts-system-submit-action . ,(plist-get arg :submit-action))
                    (bts-current-tickets      . ,(list ticket)))
                  attributes)
                 (plist-get arg :attributes))))
    (pop-to-buffer
     (bts:widget-build-buffer :buffer buf
                              :layout layout
                              :defaults defs
                              :title (if updatep "Update Ticket" "Add Ticket")
                              :submit-title (or submit-title "Regist")
                              :submit-action submit
                              :cancel-title cancel-title
                              :cancel-action cancel-action
                              :attributes attrs))))

(bts:regist-message 'bts-ticket-multi-regist-confirm
  t         "Update the selected %i tickets by the inputed contents?"
  'Japanese "選択された%i個のチケットを入力内容で更新してよろしいですか？")

(defun bts::ticket-open-system-multi-view (tickets proj arg)
  (bts--debug* "start ticket open system multi view. proj[%s]\ntickets... %s"
               (ignore-errors (bts:project-get-unique-string proj))
               (mapconcat (lambda (e) (ignore-errors (bts:ticket-get-unique-string e)))
                          tickets
                          ", "))
  (bts--trace* "full arguments are
tickets...\n%s
declare...%s"
               (mapconcat (lambda (e) (format "%s" e)) tickets "\n") arg)
  (let* ((sys (bts:project-get-system proj))
         (sysnm (bts::system-normalize-name (bts:system-name sys)))
         (buf (or (plist-get arg :buffer)
                  (wmvc:get-new-buffer bts::default-ticket-view-buffer-name)))
         (layout `((:type const :name bts-system :label "SystemType" :unarrayed t) "  "
                   (:type const :name bts-project :label "ProjectName" :unarrayed t) BR
                   BR
                   ,@(plist-get arg :layout)))
         (defs (append `((bts-system  . ,sysnm)
                         (bts-project . ,(plist-get proj :bts-name)))
                       (plist-get arg :defaults)))
         (submit `(lambda (mdl)
                    (if (not (y-or-n-p (bts:get-message 'bts-ticket-multi-regist-confirm
                                                        ,(length tickets))))
                        (bts:show-message "Quit")
                      (bts::ticket-action-simple-regist mdl t 'bts:ticket-multi-update))))
         (attrs (append `((bts-system-submit-action . ,(plist-get arg :submit-action))
                          (bts-current-tickets      . ,tickets))
                        (plist-get arg :attributes))))
    (pop-to-buffer
     (bts:widget-build-buffer :buffer buf
                              :layout layout
                              :defaults defs
                              :title "Update Tickets In A Lump"
                              :submit-title "Regist"
                              :submit-action submit
                              :attributes attrs))))

(defun bts::ticket-open-sequence-view (tickets ticket-queue init-tickets)
  (bts--debug* "start ticket open sequence view.
tickets... %s
ticket-queue... %s
init-tickets... %s"
               (mapconcat (lambda (e) (ignore-errors (bts:ticket-get-unique-string e)))
                          tickets
                          ", ")
               (mapconcat (lambda (e) (ignore-errors (bts:ticket-get-unique-string e)))
                          ticket-queue
                          ", ")
               (mapconcat (lambda (e) (ignore-errors (bts:ticket-get-unique-string e)))
                          init-tickets
                          ", "))
  (let* ((init-tics (or init-tickets
                        (mapcar 'copy-sequence tickets)))
         (tic (pop tickets))
         (next-tics tickets)
         (submit-title (if next-tics "Next" "Regist"))
         (submit-action (if next-tics
                            'bts::ticket-action-next-view
                          'bts::ticket-action-sequence-regist))
         (cancel-title (when ticket-queue "Back"))
         (cancel-action (when ticket-queue 'bts::ticket-action-previous-view))
         (attrs `((bts-current-tickets . ,(append ticket-queue (list tic)))
                  (bts-next-tickets . ,next-tics)
                  (bts-init-tickets . ,init-tics))))
    (bts::ticket-open-single-view tic
                                  :updatep t
                                  :submit-title submit-title
                                  :submit-action submit-action
                                  :cancel-title cancel-title
                                  :cancel-action cancel-action
                                  :attributes attrs)))

(bts:regist-message 'bts-ticket-multi-not-same-project
  t         "Open sequence views as substitute for multi view because the selected tickets don't belong to same project"
  'Japanese "選択されたチケットは同じプロジェクトに属していないため、一括処理画面の代わりに順次処理画面を開きます")

(bts:regist-message 'bts-ticket-multi-not-system-declare
  t         "Open sequence views because %s seems to be not able to apply multi view for the selected tickets"
  'Japanese "%sの一括処理画面は選択されたチケットには対応していないようなので、順次処理画面を開きます")

(defun bts::ticket-open-multi-view (tickets &optional sequential)
  (bts--debug "start ticket open multi view.")
  (let* ((proj (bts:ticket-get-project (nth 0 tickets)))
         (sys (bts:project-get-system proj))
         (sysnm (bts:system-name sys))
         (view-maker (bts:system-ticket-multi-view sys))
         (try-system-p (and (not sequential)
                            bts:ticket-multi-view-preferred
                            view-maker))
         (same-project-p (bts:ticket-belong-to-same-project-p tickets))
         (view-declare (when (and try-system-p same-project-p)
                         (bts--debug "call ticket multi view of %s" sysnm)
                         (funcall view-maker (copy-sequence proj) (mapcar 'copy-sequence tickets)))))
    (bts:aif view-declare
        (bts::ticket-open-system-multi-view tickets proj it)
      (when try-system-p
        (cond ((not same-project-p)
               (bts:show-message (bts:get-message 'bts-ticket-multi-not-same-project)))
              (t
               (bts:show-message (bts:get-message 'bts-ticket-multi-not-system-declare sysnm))))
        (sleep-for 1))
      (bts::ticket-open-sequence-view tickets nil nil))))


(defun bts:ticket-open-add-view (project)
  "Popup a widget buffer for creating a new ticket of PROJECT.

PROJECT is a plist as project configuration."
  (bts--debug "start ticket open add view. project[%s]"
              (bts:project-get-unique-string project t))
  (let ((ticket `(:bts-project ,(plist-get project :bts-id))))
    (bts::ticket-open-single-view ticket :updatep nil)))

(defun bts:ticket-open-update-view (tickets &optional sequential)
  "Popup a widget buffer for updating TICKETS.

TICKETS is a list of plist as ticket data.
If SEQUENTIAL is non-nil or the counf of TICKETS is one, ticket-single-view of the system which
TICKETS belongs to is used.
Else, if `bts:ticket-multi-view-preferred' is non-nil and ticket-multi-view of the system is
defined, it's used.
Otherwise, ticket-single-view is used."
  (bts--debug* "start ticket open update view.\ntickets... %s"
               (mapconcat (lambda (e) (ignore-errors (bts:ticket-get-unique-string e)))
                          tickets
                          ", "))
  (let ((tickets (if (listp tickets)
                     (mapcar 'copy-sequence tickets)
                   tickets)))
    (cond ((not tickets)                (yaxception:throw 'bts:invalid-argument))
          ((bts:ticket-p tickets)       (bts::ticket-open-single-view tickets :updatep t))
          ((and (listp tickets)
                (= (length tickets) 1)) (bts::ticket-open-single-view (pop tickets) :updatep t))
          ((listp tickets)              (bts::ticket-open-multi-view tickets sequential))
          (t                            (yaxception:throw 'bts:invalid-argument)))))


;;;;;;;;;;;;;;;;;;
;; Fetch Ticket

(defvar bts::ticket-fetch-finish-check-timer nil)
(defvar bts::ticket-fetch-result-hash nil)

(defun bts::ticket-fetch-init-result ()
  (bts--debug "start ticket fetch init result.")
  (setq bts::ticket-fetch-result-hash (make-hash-table :test 'equal)))

(defun bts::ticket-fetch-cancel-timer ()
  (when bts::ticket-fetch-finish-check-timer
    (bts--debug "start ticket fetch cancel timer.")
    (cancel-timer bts::ticket-fetch-finish-check-timer)
    (setq bts::ticket-fetch-finish-check-timer nil)))

(defun bts::ticket-fetch-make-fetch-key (query)
  (if (not (bts:query-p query))
      (yaxception:throw 'bts:invalid-argument)
    (bts:query-get-description query)))

(bts:regist-message 'bts-ticket-fetch-waiting
  t         "Waiting for the %i task finish to fetch ticket ..."
  'Japanese "%i個のチケット取得処理の完了を待っています ...")

(bts:regist-message 'bts-ticket-fetch-finished
  t         "Finished to fetch ticket"
  'Japanese "チケット取得処理が完了しました")

(defun* bts::ticket-fetch-sentinel (nextfunc limit-seconds &key wait-all-finished)
  (yaxception:$
    (yaxception:try
      (bts--trace "start ticket fetch sentinel.")
      (let* ((curr-seconds (bts:make-seconds nil nil))
             (limit-over (progn (bts--debug "check ticket fetch limit over. curr[%s] limit[%s]"
                                            curr-seconds limit-seconds)
                                (> curr-seconds limit-seconds)))
             (allkeys (when (hash-table-p bts::ticket-fetch-result-hash)
                        (loop for k being the hash-keys in bts::ticket-fetch-result-hash using (hash-values v)
                              if (eq v 'failed)       collect k into failkeys
                              else if (eq v 'running) collect k into runkeys
                              else                    collect k into finkeys
                              finally return (list failkeys runkeys finkeys))))
             (failkeys (nth 0 allkeys))
             (runkeys (nth 1 allkeys))
             (finkeys (nth 2 allkeys))
             (nextcall (when finkeys
                         (if wait-all-finished (not runkeys) t)))
             (quit '(lambda (&optional errmsg failkeys)
                      (bts::ticket-fetch-cancel-timer)
                      (cond (errmsg
                             (bts:show-message "Failed to fetch ticket : %s" errmsg))
                            (failkeys
                             (let ((failstr (mapconcat 'identity failkeys ", ")))
                               (bts--error "failed to fetch ticket of %s" failstr)
                               (bts:show-message "Failed to fetch ticket of %s" failstr)))
                            (t
                             (bts:show-message (bts:get-message 'bts-ticket-fetch-finished)))))))
        (cond (nextcall
               (let ((tickets (loop for k in (sort finkeys 'string<)
                                    append (gethash k bts::ticket-fetch-result-hash)
                                    do (remhash k bts::ticket-fetch-result-hash))))
                 (when (not runkeys) (funcall quit))
                 (bts--debug "call ticket fetch next function : tickets[%s]" (length tickets))
                 (run-with-timer 0 nil nextfunc tickets (not runkeys))))
              (limit-over
               (bts--error "over ticket fetch limit. remained %s" (mapconcat 'identity runkeys ", "))
               (funcall quit "wait limit is over"))
              (runkeys
               (when (not (active-minibuffer-window))
                 (bts:show-message (bts:get-message 'bts-ticket-fetch-waiting (length runkeys)))))
              (t
               (funcall quit nil failkeys)))))
    (yaxception:catch 'error e
      (bts--error "failed ticket fetch sentinel : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts::ticket-fetch-cancel-timer)
      (bts::ticket-fetch-init-result)
      (yaxception:throw e))))

(bts:regist-message 'bts-ticket-fetch-already-running
  t         "Can't start to fetch ticket : other task is running"
  'Japanese "別のチケット取得処理が実行中のため、チケット取得を開始できません")

(defun* bts:ticket-fetch (queries callback &key limit wait-all-finished)
  "Start to fetch ticket using QUERIES and the task checkes the finish.

QUERIES is a plist or list of plist as query configuration.
CALLBACK is a function called after the fetching finish.
 It receives a list of the fetched tickets from all of QUERIES as one argument.
LIMIT is a string as limit to wait for the fetching finish. In default, \"3min\".
 The format is argument for `timer-duration'.
WAIT-ALL-FINISHED is a boolean whether to wait for the invoke of CALLBACK
 until all QUERIES tasks is finished."
  (bts--debug* "start ticket fetch. limit[%s] wait-all-finished[%s]
queries... %s"
               limit wait-all-finished
               (mapconcat (lambda (q) (bts:query-get-unique-string q t))
                          queries
                          ", "))
  (bts--trace* "full arguments are
callback... %s"
               callback)
  (if bts::ticket-fetch-finish-check-timer
      (progn
        (bts--info "quit ticket fetch : other ticket fetch task exists")
        (bts:show-message (bts:get-message 'bts-ticket-fetch-already-running)))
    (bts::ticket-fetch-init-result)
    (dolist (q (if (bts:query-p queries)
                   (list queries)
                 queries))
      (let* ((q (copy-sequence q))
             (proj (copy-sequence (bts:query-get-project q)))
             (sys (bts:project-get-system proj))
             (fetchkey (bts::ticket-fetch-make-fetch-key q)))
        (bts--debug "set timer to call ticket fetcher for %s" fetchkey)
        (puthash fetchkey 'running bts::ticket-fetch-result-hash)
        (run-with-idle-timer 0.1 nil (bts:system-ticket-fetcher sys) proj q)))
    (bts--info "set timer to check fetch finish")
    (let ((limitsec (bts:make-seconds nil (or limit "3min")))
          (iv bts:ticket-fetch-check-interval))
      (setq bts::ticket-fetch-finish-check-timer
            (run-with-timer iv iv 'bts::ticket-fetch-sentinel
                            callback limitsec :wait-all-finished wait-all-finished)))
    (bts:show-message "Start to fetch ticket")))

(defun bts:ticket-fetch-failed (query)
  "Inform the fail of fetching ticket using QUERY to the task checkes the finish.

QUERY is a plist as query configuration."
  (yaxception:$
    (yaxception:try
      (bts--debug* "start ticket fetch failed. query[%s]"
                   (bts:query-get-unique-string query))
      (let ((fetchkey (bts::ticket-fetch-make-fetch-key query)))
        (puthash fetchkey 'failed bts::ticket-fetch-result-hash)))
    (yaxception:catch 'error e
      (bts--error "failed ticket fetch failed : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts::ticket-fetch-init-result))))

(defun bts:ticket-fetch-complete (query tickets)
  "Inform the complete of fetching ticket using QUERY to the task checkes the finish.

QUERY is a plist as query configuration.
TICKETS is a list of plist as fetched ticket from QUERY."
  (yaxception:$
    (yaxception:try
      (bts--debug* "start ticket fetch complete. tickets[%s] query[%s]"
                   (length tickets)
                   (bts:query-get-unique-string query))
      (let ((projid (plist-get query :bts-project))
            (fetchkey (bts::ticket-fetch-make-fetch-key query)))
        (if (not (eq (gethash fetchkey bts::ticket-fetch-result-hash) 'running))
            (bts--warn "quit ticket fetch complete : state is not running")
          (dolist (tic tickets)
            (plist-put tic :bts-project projid))
          (puthash fetchkey tickets bts::ticket-fetch-result-hash)
          (bts--info "set ticket fetch result for %s" fetchkey))))
    (yaxception:catch 'error e
      (bts--error "failed ticket fetch complete : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:ticket-fetch-failed query)
      (yaxception:throw e))))

(defun bts:ticket-fetch-quit ()
  "Quit the running tasks fetches ticket."
  (interactive)
  (bts::ticket-fetch-cancel-timer)
  (bts:show-message "Finished ticket fetch quit"))


;;;;;;;;;;;;;
;; Summary

(defvar bts::summary-query-idlist nil)
(defvar bts::summary-ticket-hash nil)
(defvar bts::summary-format nil)
(defvar bts::summary-sort-key nil)

(defsubst bts::summary-get-column-label-at (&optional pos)
  (get-text-property (or pos (point)) 'tabulated-list-column-name))

(defsubst bts::summary-get-column-index-by-label (labelnm)
  (loop with idx = 0
        for e in (mapcar 'car tabulated-list-format)
        if (equal e labelnm) return idx
        else                 do (incf idx)))

(defsubst bts::summary-get-column-label-by-index (index)
  (ignore-errors (nth index (mapcar 'car tabulated-list-format))))

(defsubst bts::summary-get-column-start-by-label (labelnm)
  (loop with pt = (point-at-bol)
        with limit = (point-at-eol)
        while (< pt limit)
        for currnm = (bts::summary-get-column-label-at pt)
        if (and currnm
                (equal currnm labelnm))
        return pt
        else
        do (setq pt (or (next-property-change pt nil limit)
                        limit))))

(defsubst bts::summary-get-entry-id (ticket)
  (cons (bts:system-name (bts:ticket-get-system ticket))
        (bts:ticket-get-unique-string ticket)))

(defsubst bts::summary-get-ticket-at (&optional pos)
  (bts:awhen (tabulated-list-get-id pos)
    (gethash it bts::summary-ticket-hash)))

(defsubst bts::summary-marked-at (&optional pos)
  (when (-find (lambda (ov)
                 (overlay-get ov 'bts:mark))
               (overlays-at (or pos (point))))
    t))

(defsubst bts::summary-get-marking-in (begin end)
  (loop for ov in (overlays-in begin end)
        if (overlay-get ov 'bts:mark)
        collect ov))

(defsubst bts::summary-mark-at-point ()
  (bts--debug "mark a unmarked summary at point")
  (when (bts::summary-get-ticket-at)
    (let ((ov (make-overlay (point-at-bol) (1+ (point-at-eol)) nil t t)))
      (overlay-put ov 'bts:mark t)
      (overlay-put ov 'face 'bts:summary-mark-face))))

(defsubst bts::summary-unmark (begin end)
  (bts--debug "unmark summary from %s to %s" begin end)
  (remove-overlays begin end 'bts:mark t))

(defsubst bts::summary-unmark-at-point ()
  (bts::summary-unmark (point-at-bol) (1+ (point-at-eol))))

(defsubst bts::summary-toggle-marking-at-point ()
  (if (bts::summary-marked-at)
      (bts::summary-unmark-at-point)
    (bts::summary-mark-at-point)))

(defun bts::summary-buffers ()
  (bts--debug "start summary buffers.")
  (loop for buf in (buffer-list)
        if (buffer-local-value 'bts::summary-query-idlist buf)
        collect buf))

(defun* bts::summary-find-buffer (queries &key not-exact)
  (bts--debug* "start summary find buffer. queries[%s] not-exact[%s]"
               (mapconcat (lambda (q) (bts:query-get-unique-string q t))
                          queries
                          ", ")
               not-exact)
  (loop with query-idlist = (loop for q in queries
                                  collect (plist-get q :bts-id))
        with exact-matcher = (lambda (idlist)
                               (and (= (length idlist)
                                       (length query-idlist))
                                    (loop for id in idlist
                                          always (member id query-idlist))))
        with fuzzy-matcher = (lambda (idlist)
                               (loop for id in query-idlist
                                     always (member id idlist)))
        with matcher = (if not-exact fuzzy-matcher exact-matcher)
        for buf in (buffer-list)
        for curr-query-idlist = (buffer-local-value 'bts::summary-query-idlist buf)
        if (funcall matcher curr-query-idlist)
        return (progn (bts--debug "found summary buffer : %s" (buffer-name buf))
                      buf)))

(defun bts::summary-goto-line (ticket)
  (let* ((startpt (point))
         (sum-id (bts::summary-get-entry-id ticket)))
    (loop initially (goto-char (point-min))
          while (not (eobp))
          for curr-id = (tabulated-list-get-id)
          if (equal curr-id sum-id)
          return t
          do (forward-line 1)
          finally return (progn (goto-char startpt)
                                nil))))

(defun bts::summary-update-mode-line (finish-p &optional now-fetching-p warn)
  (bts--debug "start summary update mode line. finish-p[%s] now-fetching-p[%s] warn[%s]"
              finish-p now-fetching-p warn)
  (let ((finished-p (or finish-p
                        (and (not now-fetching-p)
                             (> (length mode-line-format) 5)
                             (string= (nth 5 mode-line-format) ""))))
        (warn-p (or warn
                    (-find (lambda (e) (overlay-get e 'bts:failed))
                           (overlays-in (point-min) (point-max))))))
    (setq mode-line-format
          (list "-" mode-line-mule-info
                " " mode-line-position
                (if (not warn-p)
                    ""
                  (concat " " (propertize "*Warning exists*" 'face 'bts:summary-condition-warn-part-face)))
                (if finished-p
                    ""
                  (concat " " (propertize "Fetching..." 'face 'bts:summary-condition-fetch-part-face)))
                " Marked:"
                (propertize (format "%i/%i"
                                    (length (bts::summary-get-marking-in (point-min) (point-max)))
                                    (count-lines (point-min) (point-max)))
                            'face 'bts:summary-condition-marked-part-face)
                " Queries:"
                (mapconcat (lambda (queryid)
                             (propertize (bts:query-get-description (bts:query-get-config-by-id queryid)
                                                                    :short t)
                                         'face 'bts:summary-condition-query-part-face))
                           bts::summary-query-idlist
                           ",")
                "-%-"))))

(defun bts::summary-make-tabulated-format (layout)
  (bts--debug "start summary make tabulated format.")
  (bts--trace* "layout is\n%s" layout)
  `[,@(loop for e in layout
            for lbl = (or (plist-get e :label)
                          (format "%s" (plist-get e :name)))
            for size = (or (plist-get e :size) 0)
            for sort = (bts:awhen (plist-get e :sort)
                         (if (functionp it)
                             `(lambda (A B)
                                (funcall ',it
                                         (gethash (car A) bts::summary-ticket-hash)
                                         (gethash (car B) bts::summary-ticket-hash)))
                           it))
            for exprops = (loop for k in '(:pad-right :right-align)
                                for v = (plist-get e k)
                                if v append (list k v))
            collect (append (list lbl size sort)
                            exprops))])

(defun* bts::summary-make-tabulated-entry (layout tic &key face-warn propkey-generator)
  (let ((sum-id (bts::summary-get-entry-id tic)))
    (bts--trace "make entry of %s" sum-id)
    `(,sum-id
      [,@(loop for e in layout
               for size = (or (plist-get e :size) 0)
               for ellipsis = (plist-get e :ellipsis)
               for formatter = (plist-get e :format)
               for key = (bts:aif propkey-generator
                             (funcall it (plist-get e :name))
                           (bts:conv-symbol-to-keyword (plist-get e :name)))
               for val = (or (plist-get tic key) "")
               for val = (cond ((functionp formatter) (or (funcall formatter tic) ""))
                               ((stringp formatter)   (format formatter val))
                               ((stringp val)         val)
                               (t                     (format "%s" val)))
               for val = (replace-regexp-in-string "[\t\n]+" " " val)
               for val = (replace-regexp-in-string "[\r]" "" val)
               for val = (if (and (> size 0) ellipsis)
                             (truncate-string-to-width val size nil nil ellipsis)
                           val)
               for face = (plist-get e :face)
               for face = (cond ((functionp face) (funcall face tic))
                                ((facep face)     face)
                                (face             (funcall face-warn face)))
               if (facep face)
               collect (propertize val 'face face)
               else
               collect val)])))

(defmacro bts::summary-with-make-tabulated-entry (elt tickets layout &rest body)
  (declare (indent 3))
  (let ((tic-sym (cl-gensym))
        (face-warned (cl-gensym))
        (face-warn (cl-gensym))
        (propnm-hash (cl-gensym))
        (propnm-getter (cl-gensym)))
    `(progn
       (bts--debug "start summary with make tabulated entry. tickets[%s]" (length ,tickets))
       (bts--trace* "layout is\n%s" ,layout)
       (dolist (,tic-sym ,tickets)
         (let* ((,face-warned nil)
                (,face-warn (lambda (v)
                              (when (not ,face-warned)
                                (bts--error "invalid setting of summary face : %s" v)
                                (setq ,face-warned t))
                              nil))
                (,propnm-hash (make-hash-table))
                (,propnm-getter (lambda (sym)
                                  (or (gethash sym ,propnm-hash)
                                      (puthash sym (bts:conv-symbol-to-keyword sym) ,propnm-hash))))
                (,elt (bts::summary-make-tabulated-entry ,layout ,tic-sym
                                                         :face-warn ,face-warn
                                                         :propkey-generator ,propnm-getter)))
           ,@body)))))

(defun bts::summary-entry-registed-p (ticket)
  (when (and (hash-table-p bts::summary-ticket-hash)
             (gethash (bts::summary-get-entry-id ticket)
                      bts::summary-ticket-hash))
    t))

(defun bts::summary-regist-entries (tickets &optional force)
  (bts--debug "start summary regist entries. tickets[%s] force[%s]" (length tickets) force)
  (if (not (hash-table-p bts::summary-ticket-hash))
      (bts--error "failed summary regist tickets : bts::summary-ticket-hash is invalid.")
    (loop for tic in tickets
          for sum-id = (bts::summary-get-entry-id tic)
          if (or force
                 (not (gethash sum-id bts::summary-ticket-hash)))
          collect (progn
                    (bts--trace "regist entry : %s" sum-id)
                    (puthash sum-id tic bts::summary-ticket-hash)))))

(defun bts::summary-update-entry (latest-ticket &optional failed)
  (bts--debug* "start summary update entry for %s. failed[%s]"
               (ignore-errors (bts:ticket-get-unique-string latest-ticket)) failed)
  (dolist (buf (bts::summary-buffers))
    (with-current-buffer buf
      (when (bts::summary-entry-registed-p latest-ticket)
        (bts::summary-regist-entries (list latest-ticket) t)
        (save-excursion
          (if (not (bts::summary-goto-line latest-ticket))
              (bts--error "failed to go to line of %s"
                          (ignore-errors (bts:ticket-get-unique-string latest-ticket)))
            (let* ((inhibit-read-only t)
                   (queryid (car bts::summary-query-idlist))
                   (query (copy-sequence (bts:query-get-config-by-id queryid)))
                   (proj (copy-sequence (bts:query-get-project query)))
                   (sys (bts:project-get-system proj))
                   (layout (funcall (bts:system-summary-format sys) proj query))
                   (entry (bts::summary-make-tabulated-entry layout latest-ticket))
                   (begin (line-beginning-position))
                   (end (1+ (line-end-position)))
                   (marked-p (bts::summary-marked-at begin)))
              (mapc 'delete-overlay (overlays-in begin end))
              (delete-region begin end)
              (apply tabulated-list-printer entry)
              (when marked-p
                (save-excursion
                  (goto-char begin)
                  (bts::summary-mark-at-point)))
              (when failed
                (let ((ov (make-overlay begin (point) nil t t)))
                  (overlay-put ov 'face 'compilation-error)
                  (overlay-put ov 'bts:failed failed)))
              (bts::summary-update-mode-line t nil failed))))))))

(bts:regist-message 'bts-summary-started
  t         "Start to print ticket summaries"
  'Japanese "サマリバッファへのチケット出力を開始します")

(bts:regist-message 'bts-summary-printed
  t         "Printing %i/%i ticket summaries ..."
  'Japanese "%i/%i個のチケットを出力中 ...")

(bts:regist-message 'bts-summary-finished
  t         "Finished to print ticket summaries"
  'Japanese "サマリバッファへのチケット出力が完了しました")

(defun bts::summary-add-entry (tickets bufnm finish-p)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary add entry. tickets[%s] bufnm[%s] finish-p[%s]"
                  (length tickets) bufnm finish-p)
      (if (not (buffer-live-p (get-buffer bufnm)))
          (yaxception:throw 'bts:error
                            :msg (format "Failed summary open : buffer[%s] is not alive" bufnm))
        (with-current-buffer (get-buffer bufnm)
          (bts:show-message (bts:get-message 'bts-summary-started))
          (let* ((buffer-read-only nil)
                 (entry-id (tabulated-list-get-id))
                 (saved-col (current-column))
                 (saved-pt (point-min))
                 (queryid (car bts::summary-query-idlist))
                 (query (copy-sequence (bts:query-get-config-by-id queryid)))
                 (proj (copy-sequence (bts:query-get-project query)))
                 (sys (bts:project-get-system proj))
                 (layout (funcall (bts:system-summary-format sys) proj query))
                 (registed-tics (bts::summary-regist-entries tickets))
                 (curr-entry-number (count-lines (point-min) (point-max)))
                 (allcounts (+ curr-entry-number (length registed-tics))))
            (goto-char (point-max))
            (bts::summary-with-make-tabulated-entry elt registed-tics layout
              (when (and entry-id
                         (equal entry-id (car elt)))
                (setq saved-pt (point)))
              (apply tabulated-list-printer elt)
              (bts:show-message (bts:get-message 'bts-summary-printed
                                                 (incf curr-entry-number)
                                                 allcounts)))
            (bts::summary-update-mode-line finish-p (not finish-p))
            (when finish-p
              (bts:show-message (bts:get-message 'bts-summary-finished)))
            (set-buffer-modified-p nil)
            (goto-char saved-pt)
            (move-to-column saved-col)
            (when (> saved-pt (point-min))
              (recenter))
            (setq tabulated-list-entries nil)
            (hl-line-highlight)))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary add entry : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary add entry : %s" (yaxception:get-text e)))
    (yaxception:finally
      ;; Restore max-specpdl-size because it might happens to change that globally.
      (when bts::max-specpdl-size-bkup
        (setq max-specpdl-size bts::max-specpdl-size-bkup)))))

(defvar bts::default-summary-buffer-name "*BTS: Summary*")
(defun bts::summary-init-buffer (queries)
  (with-current-buffer (or (bts::summary-find-buffer queries)
                           (generate-new-buffer bts::default-summary-buffer-name))
    (if bts::summary-query-idlist
        (let ((buffer-read-only nil))
          (bts--debug "start clear summary buffer : %s" (buffer-name))
          (erase-buffer)
          (remove-overlays)
          (setq tabulated-list-entries nil)
          (setq bts::summary-ticket-hash (make-hash-table :test 'equal)))
      (let* ((query (copy-sequence (car queries)))
             (proj (copy-sequence (bts:query-get-project query)))
             (sys (bts:project-get-system proj))
             (layout (funcall (bts:system-summary-format sys) proj query))
             (bts::summary-format (bts::summary-make-tabulated-format layout))
             (bts::summary-sort-key nil))
        (bts--debug "start init summary buffer : %s" (buffer-name))
        (bts:summary-mode)
        (setq bts::summary-query-idlist (loop for q in queries collect (plist-get q :bts-id)))
        (setq bts::summary-ticket-hash (make-hash-table :test 'equal))
        (buffer-disable-undo)
        (hl-line-mode 1)))
    (bts::summary-update-mode-line nil t)
    (current-buffer)))

(defun bts::summary-start-buffer (queries &optional initialize)
  (bts--debug* "start summary start buffer. initialize[%s]\nqueries... %s"
               initialize
               (mapconcat (lambda (q) (bts:query-get-unique-string q t))
                          queries
                          ", "))
  (let* ((buf (or (when initialize (bts::summary-init-buffer queries))
                  (bts::summary-find-buffer queries)
                  (progn
                    (setq initialize t)
                    (bts::summary-init-buffer queries))))
         (cb `(lambda (tickets finishp)
                (bts::summary-add-entry tickets ,(buffer-name buf) finishp))))
    (when initialize
      (bts:ticket-fetch queries cb :limit "2min" :wait-all-finished nil))
    buf))

(defadvice tabulated-list-print (before bts:summary-ensure-tabulated-list-entries activate)
  (when (and bts::summary-query-idlist
             (not tabulated-list-entries))
    (setq tabulated-list-entries
          (save-excursion
            (loop initially (goto-char (point-min))
                  while (not (eobp))
                  for sum-id = (tabulated-list-get-id)
                  for entry = (tabulated-list-get-entry)
                  collect (list sum-id entry)
                  do (forward-line 1))))))

(defvar bts:summary-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "j")     'next-line)
    (define-key map (kbd "k")     'previous-line)
    (define-key map (kbd "h")     'bts:summary-left-column)
    (define-key map (kbd "l")     'bts:summary-right-column)
    (define-key map (kbd "n")     'next-line)
    (define-key map (kbd "p")     'previous-line)
    (define-key map (kbd "b")     'bts:summary-left-column)
    (define-key map (kbd "f")     'bts:summary-right-column)
    (define-key map (kbd "SPC")   'scroll-up)
    (define-key map (kbd "RET")   'bts:summary-view-ticket)
    (define-key map (kbd "o")     'bts:summary-view-ticket)
    (define-key map (kbd "m")     'bts:summary-mark-ticket)
    (define-key map (kbd "M")     'bts:summary-mark-all-tickets)
    (define-key map (kbd "u")     'bts:summary-unmark-ticket)
    (define-key map (kbd "U")     'bts:summary-unmark-all-tickets)
    (define-key map (kbd "M-DEL") 'bts:summary-unmark-all-tickets)
    (define-key map (kbd "t")     'bts:summary-toggle-ticket-marking)
    (define-key map (kbd "T")     'bts:summary-toggle-all-tickets-marking)
    (define-key map (kbd "d")     'bts:summary-delete-ticket)
    (define-key map (kbd "D")     'bts:summary-delete-marked-tickets)
    (define-key map (kbd "r")     'bts:summary-reload-ticket)
    (define-key map (kbd "g")     'bts:summary-reload-ticket)
    (define-key map (kbd "R")     'bts:summary-reload-all)
    (define-key map (kbd "G")     'bts:summary-reload-all)
    map)
  "Keymap for summary buffer.")

(define-derived-mode bts:summary-mode tabulated-list-mode "BTS:Summary"
  "Major mode for browsing a list of ticket summaries.

\\<bts:summary-mode-map>
\\{bts:summary-mode-map}"
  (set (make-local-variable 'bts::summary-query-idlist) nil)
  (set (make-local-variable 'bts::summary-ticket-hash) (make-hash-table :test 'equal))
  (setq truncate-lines t)
  (setq tabulated-list-format bts::summary-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key bts::summary-sort-key)
  (setq tabulated-list-entries nil)
  (setq tabulated-list-use-header-line t)
  (tabulated-list-init-header))

(defun bts:summary-right-column ()
  "Move to right column."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary right column.")
      (let* ((currnm (bts::summary-get-column-label-at))
             (curridx (when currnm
                        (bts::summary-get-column-index-by-label currnm)))
             (nextidx (cond (curridx                    (1+ curridx))
                            ((= (point) (point-at-bol)) 0)))
             (nextnm (when nextidx
                       (bts::summary-get-column-label-by-index nextidx)))
             (nextpt (when nextnm
                       (bts::summary-get-column-start-by-label nextnm))))
        (bts:aif nextpt
            (goto-char it)
          (goto-char (point-at-eol)))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary right column : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary right column : %s" (yaxception:get-text e)))))

(defun bts:summary-left-column ()
  "Move to left column."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary left column.")
      (let* ((currnm (bts::summary-get-column-label-at))
             (curridx (when currnm
                        (bts::summary-get-column-index-by-label currnm)))
             (previdx (cond (curridx                    (1- curridx))
                            ((= (point) (point-at-eol)) (1- (length tabulated-list-format)))))
             (prevnm (when previdx
                       (bts::summary-get-column-label-by-index previdx)))
             (prevpt (when prevnm
                       (bts::summary-get-column-start-by-label prevnm))))
        (when prevpt
          (goto-char prevpt))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary left column : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary left column : %s" (yaxception:get-text e)))))

(defun bts:summary-reload-all ()
  "Re fetch and setup current buffer."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary reload all.")
      (bts:awhen bts::summary-query-idlist
        (bts::summary-start-buffer (mapcar 'bts:query-get-config-by-id it) t)))
    (yaxception:catch 'error e
      (bts--fatal "failed summary reload all : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary reload all : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-summary-ticket-not-found
  t         "Any ticket is not found"
  'Japanese "処理対象のチケットが見つかりません")

(defun bts:summary-reload-ticket ()
  "Update marked entries or current entry to the latest data."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary reload ticket.")
      (let ((tickets (-filter 'bts:ticket-p
                              (or (loop for ov in (bts::summary-get-marking-in (point-min) (point-max))
                                        collect (bts::summary-get-ticket-at (overlay-start ov)))
                                  (list (bts::summary-get-ticket-at))))))
        (if (not tickets)
            (bts:show-message (bts:get-message 'bts-summary-ticket-not-found))
          (dolist (tic tickets)
            (yaxception:$
              (yaxception:try
                (bts::summary-update-entry (bts:ticket-get-latest tic)))
              (yaxception:catch 'bts:error e
                (bts--error "failed summary reload ticket : %s\n%s"
                            (yaxception:get-text e) (yaxception:get-stack-trace-string e))
                (bts:show-message (yaxception:get-text e))
                (bts::summary-update-entry tic t))))
          (bts::summary-update-mode-line t nil))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary reload ticket : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary reload ticket : %s" (yaxception:get-text e)))))

(defun bts:summary-view-ticket ()
  "Popup a widget buffer for updating marked entries or current entry."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary view ticket.")
      (let ((tickets (-filter 'bts:ticket-p
                              (or (loop for ov in (bts::summary-get-marking-in (point-min) (point-max))
                                        collect (bts::summary-get-ticket-at (overlay-start ov)))
                                  (list (bts::summary-get-ticket-at)))))
            (bts:ticket-multi-view-preferred (if current-prefix-arg
                                                 (not bts:ticket-multi-view-preferred)
                                               bts:ticket-multi-view-preferred)))
        (if (not tickets)
            (bts:show-message (bts:get-message 'bts-summary-ticket-not-found))
          (bts:ticket-open-update-view tickets))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary view ticket : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary view ticket : %s" (yaxception:get-text e)))))

(defun bts:summary-mark-ticket ()
  "Put a mark to current entry."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary mark ticket.")
      (when (not (bts::summary-marked-at))
        (bts::summary-mark-at-point)
        (bts::summary-update-mode-line nil))
      (forward-line 1))
    (yaxception:catch 'error e
      (bts--fatal "failed summary mark ticket : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary mark ticket : %s" (yaxception:get-text e)))))

(defun bts:summary-unmark-ticket ()
  "Remove a mark of current entry."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary unmark ticket.")
      (when (bts::summary-marked-at)
        (bts::summary-unmark-at-point)
        (bts::summary-update-mode-line nil))
      (forward-line 1))
    (yaxception:catch 'error e
      (bts--fatal "failed summary unmark ticket : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary unmark ticket : %s" (yaxception:get-text e)))))

(defun bts:summary-toggle-ticket-marking ()
  "Switch a mark situation of current entry."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary toggle ticket marking.")
      (bts::summary-toggle-marking-at-point)
      (bts::summary-update-mode-line nil)
      (forward-line 1))
    (yaxception:catch 'error e
      (bts--fatal "failed summary toggle ticket marking : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary toggle ticket marking : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-summary-ticket-no-eraser
  t         "Not provide ticket eraser for %s"
  'Japanese "%sのチケット削除機能は提供されていません")

(bts:regist-message 'bts-summary-ticket-erase-confirm
  t         "Really delete this ticket?"
  'Japanese "このチケットを削除してよろしいですか？")

(defun bts:summary-delete-ticket ()
  "Delete current entry."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary delete ticket.")
      (let* ((ticket (bts::summary-get-ticket-at))
             (proj (bts:ticket-get-project ticket))
             (sys (bts:project-get-system proj))
             (func (bts:system-ticket-eraser sys)))
        (cond ((not (bts:ticket-p ticket))
               (bts:show-message (bts:get-message 'bts-summary-ticket-not-found)))
              ((not (functionp func))
               (bts:show-message (bts:get-message 'bts-summary-ticket-no-eraser)))
              ((y-or-n-p (bts:get-message 'bts-summary-ticket-erase-confirm))
               (funcall func proj ticket)
               (tabulated-list-delete-entry)
               (bts:show-message "Done"))
              (t
               (bts:show-message "Quit")))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary delete ticket : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary delete ticket : %s" (yaxception:get-text e)))))

(defun bts:summary-mark-all-tickets ()
  "Put marks to all entries."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary mark all tickets.")
      (save-excursion
        (loop initially (goto-char (point-min))
              while (not (eobp))
              if (not (bts::summary-marked-at))
              do (bts::summary-mark-at-point)
              do (forward-line 1)))
      (bts::summary-update-mode-line nil))
    (yaxception:catch 'error e
      (bts--fatal "failed summary mark all tickets : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary mark all tickets : %s" (yaxception:get-text e)))))

(defun bts:summary-unmark-all-tickets ()
  "Remove the marks of all entries."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary unmark all tickets.")
      (bts::summary-unmark (point-min) (point-max))
      (bts::summary-update-mode-line nil))
    (yaxception:catch 'error e
      (bts--fatal "failed summary unmark all tickets : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary unmark all tickets : %s" (yaxception:get-text e)))))

(defun bts:summary-toggle-all-tickets-marking ()
  "Switch the mark situations of all entries."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary toggle all tickets marking.")
      (save-excursion
        (loop initially (goto-char (point-min))
              while (not (eobp))
              do (bts::summary-toggle-marking-at-point)
              do (forward-line 1)))
      (bts::summary-update-mode-line nil))
    (yaxception:catch 'error e
      (bts--fatal "failed summary toggle all tickets marking : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary toggle all tickets marking : %s" (yaxception:get-text e)))))

(bts:regist-message 'bts-summary-ticket-all-erase-confirm
  t         "Really delete all marked tickets?"
  'Japanese "選択されたチケットを削除してよろしいですか？")

(bts:regist-message 'bts-summary-ticket-no-erasers
  t         "Not deleted %i tickets : they are not provided eraser"
  'Japanese "%i件のチケットが削除機能が提供されていないため削除できませんでした")

(defun bts:summary-delete-marked-tickets ()
  "Delete marked entries."
  (interactive)
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary delete marked tickets.")
      (let ((tickets (-filter 'bts:ticket-p
                              (loop for ov in (bts::summary-get-marking-in (point-min) (point-max))
                                    collect (bts::summary-get-ticket-at (overlay-start ov))))))
        (cond ((not tickets)
               (bts:show-message (bts:get-message 'bts-summary-ticket-not-found)))
              ((not (y-or-n-p (bts:get-message 'bts-summary-ticket-all-erase-confirm)))
               (bts:show-message "Quit"))
              (t
               (save-excursion
                 (loop with no-eraser = 0
                       for ticket in tickets
                       for proj = (bts:ticket-get-project ticket)
                       for sys = (bts:project-get-system proj)
                       for func = (bts:system-ticket-eraser sys)
                       if (functionp func)
                       do (progn (funcall func proj ticket)
                                 (bts::summary-goto-line ticket)
                                 (tabulated-list-delete-entry))
                       else
                       do (incf no-eraser)
                       finally (when (> no-eraser 0)
                                 (bts:show-message (bts:get-message 'bts-summary-ticket-no-erasers)))))
               (bts:show-message "Done")))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary marked tickets : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary marked tickets : %s" (yaxception:get-text e)))))

(defun bts:summary-open (queries)
  "Setup a summary buffer for QUERIES and switch current buffer to that."
  (interactive
   (list (bts::query-select :multiple t)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start summary open.")
      ;; Backup max-specpdl-size because it might happens to change that globally.
      (when (not bts::max-specpdl-size-bkup)
        (setq bts::max-specpdl-size-bkup max-specpdl-size))
      (let ((queries-hash (make-hash-table :test 'equal))
            swbuf)
        ;; Gather same layout queries
        (loop for q in queries
              for proj = (copy-sequence (bts:query-get-project q))
              for sys = (bts:project-get-system proj)
              for layout = (progn
                             (bts--debug "call summary format of %s" (bts:system-name sys))
                             (funcall (bts:system-summary-format sys) proj (copy-sequence q)))
              for pairs = (gethash layout queries-hash)
              do (push q pairs)
              do (puthash layout pairs queries-hash))
        ;; Start to setup buffer for each of the gathered queries.
        (dolist (pair-queries (loop for v being the hash-values in queries-hash collect v))
          (let ((buf (bts::summary-start-buffer pair-queries)))
            (when (not (equal buf (current-buffer)))
              (setq swbuf buf))))
        ;; Show the buffer
        (bts:awhen swbuf
          (switch-to-buffer it))))
    (yaxception:catch 'error e
      (bts--fatal "failed summary open : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed summary open : %s" (yaxception:get-text e)))))


(defun bts:ticket-new (project)
  "Regist a new ticket of PROJECT.

PROJECT is a plist as project configuration."
  (interactive
   (list (bts::project-select)))
  (yaxception:$
    (yaxception:try
      (bts--debug "start ticket new.")
      (bts:ticket-open-add-view project))
    (yaxception:catch 'error e
      (bts--fatal "failed ticket new : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (bts:show-message "Failed ticket new : %s" (yaxception:get-text e)))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Complex Condition

(defstruct bts:complex-condition and-p members)

(yaxception:deferror 'bts:invalid-condition 'bts:error
                     "Invalid condition : %s at '%s'" 'msg 'value)

(defvar bts:complex-condition-help
  "Describe a match condition using &,|,().

Examples:
 hoge
 hoge|fuga
 ( hoge & fuga ) | bar
 (hoge & (fuga|bar)) | (foo&baz)")

(bts:regist-message 'bts-ccond-empty-brace
  t         "Empty brace"
  'Japanese "空のリスト")

(bts:regist-message 'bts-ccond-unbalance-brace
  t         "Unbalance brace"
  'Japanese "括弧の対応不正")

(defun bts::ccond-lex-brace (v)
  (bts--trace "start ccond lex brace : %s" v)
  (with-temp-buffer
    (insert v)
    (let ((braced-values (loop initially (goto-char (point-min))
                               while (search-forward "(" nil t)
                               for begin = (point)
                               for end = (progn
                                           (forward-char -1)
                                           (yaxception:$~
                                             (yaxception:try (forward-list 1))
                                             (yaxception:catch 'error e
                                               (yaxception:throw
                                                 'bts:invalid-condition
                                                 :msg (bts:get-message 'bts-ccond-unbalance-brace)
                                                 :value v)))
                                           (forward-char -1)
                                           (point))
                               for currv = (buffer-substring-no-properties begin end)
                               for currv = (s-trim currv)
                               do (bts--debug "found braced value : %s" currv)
                               if (string= currv "")
                               do (yaxception:throw 'bts:invalid-condition
                                                    :msg (bts:get-message 'bts-ccond-empty-brace)
                                                    :value v)
                               collect currv
                               do (delete-region begin end))))
      (list (buffer-string) braced-values))))

(bts:regist-message 'bts-ccond-mix-operator
  t         "Mixed &,| in a list"
  'Japanese "リスト内に&,|が混在")

(bts:regist-message 'bts-ccond-empty-member
  t         "Empty value in a list"
  'Japanese "リスト内に空の要素")

(defun bts:complex-condition-compile (v)
  "Return a `bts:complex-condition' object or a string made from V.

V is a string as a complex condition includes some special words (eg. |, &).
If return value is a string, it's a simple condition string not includes any special words."
  (yaxception:$
    (yaxception:try
      (bts--debug "start ccond compile : %s" v)
      (multiple-value-bind (v braced-values) (bts::ccond-lex-brace v)
        (bts--trace "finished lex brace. braces[%s]\nresult... %s" (length braced-values) v)
        (loop with member-getter = (lambda (v)
                                     (let ((v (s-trim v)))
                                       (cond ((string= v "()") (pop braced-values))
                                             (t                v))))
              for (operator . invalid-regexp) in '(("&" . "|")
                                                   ("|" . "&"))
              for member-values = (split-string v operator)
              if (> (length member-values) 1)
              return (bts:aif (string-match invalid-regexp v)
                         (yaxception:throw 'bts:invalid-condition
                                           :msg (bts:get-message 'bts-ccond-mix-operator)
                                           :value v)
                       (bts--debug "got a complex condition. operator[%s]\nmembers... %s"
                                   operator member-values)
                       (make-bts:complex-condition
                        :and-p (string= operator "&")
                        :members (loop for m in member-values
                                       for mv = (funcall member-getter m)
                                       for ccond = (bts:complex-condition-compile mv)
                                       if (not ccond)
                                       do (yaxception:throw
                                            'bts:invalid-condition
                                            :msg (bts:get-message 'bts-ccond-empty-member)
                                            :value v)
                                       collect ccond)))
              finally return (let ((v (funcall member-getter v)))
                               (when (and v (not (string= v "")))
                                 (bts--debug "got a not complex condition : %s" v)
                                 v)))))
    (yaxception:catch 'bts:invalid-condition e
      (bts--error "failed complex condition compile : %s" (yaxception:get-text e))
      (yaxception:throw e))
    (yaxception:catch 'error e
      (bts--error "failed complex condition compile : %s\n%s"
                  (yaxception:get-text e) (yaxception:get-stack-trace-string e))
      (yaxception:throw 'bts:invalid-condition
                        :msg (yaxception:get-text e)
                        :value v))))

(defun* bts:complex-condition-match-to-list (check-values condition &key ignore-case)
  "Whether to match any entry of CHECK-VALUES to CONDITION.

CHECK-VALUES is a list of checked string.
CONDITION is a string or a `bts:complex-condition' object.
If IGNORE-CASE is non-nil, the different of character case between their strings is ignored.
For the compare method of their strings, `equal' is used."
  (cond ((not (bts:complex-condition-p condition))
         (when (member (downcase condition) (mapcar 'downcase check-values))
           t))
        ((bts:complex-condition-and-p condition)
         (loop for m in (bts:complex-condition-members condition)
               always (bts:complex-condition-match-to-list check-values m :ignore-case ignore-case)))
        (t
         (loop for m in (bts:complex-condition-members condition)
               if (bts:complex-condition-match-to-list check-values m :ignore-case ignore-case)
               return t))))

(defun* bts:complex-condition-match-to-string (check-value condition &key ignore-case)
  "Whether to match CHECK-VALUE to CONDITION.

CHECK-VALUE is a checked string.
CONDITION is a string or a `bts:complex-condition' object.
If IGNORE-CASE is non-nil, the different of character case between their strings is ignored.
For the compare method of their strings, `s-index-of' is used."
  (cond ((not (bts:complex-condition-p condition))
         (when (s-index-of condition check-value ignore-case)
           t))
        ((bts:complex-condition-and-p condition)
         (loop for m in (bts:complex-condition-members condition)
               always (bts:complex-condition-match-to-string check-value m :ignore-case ignore-case)))
        (t
         (loop for m in (bts:complex-condition-members condition)
               if (bts:complex-condition-match-to-string check-value m :ignore-case ignore-case)
               return t))))

(defun bts:complex-condition-validation (ctx value &rest args)
  "Validate widget value is a complex condition."
  (yaxception:$
    (yaxception:try
      (bts:complex-condition-compile value)
      nil)
    (yaxception:catch 'bts:invalid-condition e
      (yaxception:get-prop e 'msg))))


;;;;;;;;;;;;;;;;
;; For widget

(defvar bts::wid-handle-leave-action nil)

(defadvice widget-move (before bts:handle-leave-action activate)
  (when (and (wmvc:context-p wmvc:context)
             bts::wid-handle-leave-action)
    (let* ((bts::wid-handle-leave-action nil)
           (ctx wmvc:context)
           (w (widget-at))
           (found (when w
                    (-find (lambda (e) (eql (cdr e) w)) (wmvc:context-widget-map ctx))))
           (wname (when found
                    (car found)))
           (leave-action (when wname
                           (assoc-default wname (bts:widget-get-local-attribute 'bts-leave))))
           (leave-action (cond ((functionp leave-action)
                                leave-action)
                               (leave-action
                                (assoc-default leave-action (wmvc:context-action-map ctx))))))
      (when leave-action
        (bts--debug "call widget leave action of %s : %s" wname leave-action)
        (funcall leave-action (wmvc:bind-from-widgets ctx))))))

(defmacro bts::wid-def-leave-handler (command)
  (declare (indent 0))
  `(defadvice ,command (around bts:handle-leave-action activate)
     (let ((bts::wid-handle-leave-action (called-interactively-p 'interactive)))
       ad-do-it)))

(bts::wid-def-leave-handler widget-forward)
(bts::wid-def-leave-handler widget-backward)
(bts::wid-def-leave-handler widget-field-activate)
;; (bts::wid-def-leave-handler widget-button-press)
(bts::wid-def-leave-handler bts:widget-forward)
(bts::wid-def-leave-handler bts:widget-backward)
(bts::wid-def-leave-handler bts:widget-jump)


;;;;;;;;;;;;;;;;;;;;;;;
;; For widget-mvc.el

(defadvice wmvc:tmpl-build-buffer (before bts:widget-setup-buffer activate)
  (when bts::wid-buffer-p
    (bts--debug "start widget setup buffer before.")
    (with-current-buffer (ad-get-arg 0)
      (set (make-local-variable 'widget-menu-minibuffer-flag) bts:widget-menu-minibuffer-flag)
      (set (make-local-variable 'widget-documentation-face)   'bts:widget-documentation-face)
      (set (make-local-variable 'widget-button-face)          'bts:widget-button-face)
      (set (make-local-variable 'widget-button-pressed-face)  'bts:widget-button-pressed-face)
      (set (make-local-variable 'widget-mouse-face)           'bts:widget-mouse-face)
      (set (make-local-variable 'widget-push-button-prefix)   "")
      (set (make-local-variable 'widget-push-button-suffix)   "")
      (set (make-local-variable 'widget-link-prefix)          "")
      (set (make-local-variable 'widget-link-suffix)          "")
      (set (make-local-variable 'truncate-partial-width-windows) nil)
      (setq truncate-lines nil))))

(defadvice wmvc:tmpl-build-buffer (after bts:widget-setup-buffer activate)
  (when bts::wid-buffer-p
    (bts--debug "start widget setup buffer after.")
    (with-current-buffer ad-return-value
      ;; Major mode seems to be not available after widget-setup.
      ;; So, run configuration without using major mode.
      (use-local-map bts:widget-keymap)
      ;; (add-hook 'widget-edit-functions 'bts::wid-edit-handler t t)
      )))

(defadvice wmvc:validate-fields (around bts:widget-buffer-p activate)
  (let ((bts::wid-buffer-p (when (ignore-errors (bts:widget-get-local-attribute 'bts-layout))
                             t)))
    ad-do-it))


(provide 'bts)
;;; bts.el ends here
