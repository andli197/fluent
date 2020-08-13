;;; fluent --- Small framework for adding commands to be executed later
;;; Commentary:

;;; Code:
(defvar fluent-command '() "List of commands to be executed.")

(require 'compile)
;; (setq-default display-buffer-reuse-frames t)

(defun fluent-message (str &rest vars)
  "Display a fluent status message."
  (message (apply #'format (concat "fluent [%s]: " str) (cons (current-time-string) vars))))

(defun fluent-add (command)
  "Add the COMMAND to the fluent execution command list."
  (fluent-message "command \"%s\" added to execution list." command)
  (push command fluent-command))

(defvar fluent-add-interactive-history '() "History for the interactive add.")
(defun fluent-add-interactive ()
  "Prompts for a command to appent to the execution list."
  (interactive)
  (let ((command (read-string "command: " (or (car fluent-add-interactive-history) "") 'fluent-add-interactive-history)))
    (fluent-add command)))

(defun fluent-remove-command ()
  "Select a command from the execution list and remove it."
  (interactive)
  (let ((selected-command (ido-completing-read "remove: " fluent-command)))
    (setq fluent-command (remove selected-command fluent-command))))

(defun fluent-clear ()
  "Clears the current list of commands inserted to the fluent execution."
  (interactive)
  (setq fluent-command '())
  (fluent-message "commands cleared"))

(defun fluent-modify ()
  "Select command in `fluent-command' and modify it."
  (interactive)
  (let* ((selected-command (ido-completing-read "select command to modify: " fluent-command))
         (new-command (read-string "modification: " selected-command)))
    (setcar (nthcdr (seq-position fluent-command selected-command) fluent-command) new-command)
    (fluent-message "command modified: %s -> %s" selected-command new-command)))

(defun fluent-compile ()
  "Run compile on the current commands list."
  (interactive)
  (fluent--compile-and-log fluent-command))

(defvar fluent--last-command '() "Last executed commands.")
(defun fluent--compile-and-log (arguments)
  "Run compile on the given ARGUMENTS fluent commands."
  (setq prepend-command "")
  (if fluent-prepend-compilation-commands
      (setq prepend-command (user/compile-append-to-command (fluent-evaluate-pre-compilation-commands))))
  (setq parsed-arguments (user/compile-append-to-command arguments))
  (setq fluent--last-command arguments)
  (setq full-command-list (list prepend-command parsed-arguments))
  (setq non-empty-commands
	(seq-remove
	 (lambda (str) (or (eq str "") (eq str nil)))
	 full-command-list))
  (setq full-command (mapconcat 'identity non-empty-commands " && "))
  (fluent-message "compiling '%s'" full-command)
  (setq compilation-always-kill t)
  (compile full-command))

(defvar fluent-prepend-compilation-commands '()
  "Commands called and append result from before the current `fluent-command'")

(defun fluent-evaluate-pre-compilation-commands ()
  "Evaluates the commands in `fluent-prepend-compilation-commands' and concatinates them with \"&&\""
  (mapconcat (lambda (fn) (funcall fn)) fluent-prepend-compilation-commands " && "))

(defun user/compile-append-to-command (input)
  "Applies `user/lisp-interpret-list' and concatinates the list with &&"
  (if (stringp input) (setq input (list input)))
  (mapconcat 'identity (user/interpret-lisp input) " && "))

(defun user/interpret-lisp (input)
  "Applies the apropriate interpretation method depending on the input."
  (cond ((listp input) (user/lisp-interpret-list input))
        ((stringp input) (user/lisp-interpret-string input))
        (t) ""))

(defun user/lisp-interpret-list (input)
  "Tries to evaluate each element of the INPUT as a lisp expression and return list of evaluated expressions."
  (mapcar 'user/lisp-interpret-string input))

(defvar user/lisp-expression-finder-regexp "{\\(.+?\\)}")
(defun user/lisp--get-replace-map (command)
  "Create a map from COMMAND to (SYMBOL . EVALUATED) pair where SYMBOL is a symbol in the COMMAND and EVALUATED is the value for which the symbol is to be replaced with."
  (seq-map
   (lambda (item) (cons item (eval (read (format "%s" (progn (string-match user/lisp-expression-finder-regexp item) (match-string 1 item)))))))
   (re-seq user/lisp-expression-finder-regexp command)))

(defun user/lisp-interpret-string (command)
  "Tries to convert EXPR to string assuming everything compound in {} is lisp code (returning strings). Everything not surrounded by {} is assumed to be pure strings"
  (let ((replace-pairs (user/lisp--get-replace-map command)))
    (seq-do
     (lambda (pair)
       (setq command (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) command)))
     replace-pairs)
    )
  command)


;; (defvar fluent-empty-command-list-callback '()
;;   "Hook called when adding a command and the `fluent-command' is empty.")

;; (defun fluent-add (command)
;;   "Add the COMMAND to the fluent execution commands list."
;;   (fluent--add-command command))

;; ;; (mapconcat (lambda (fn) (funcall fn)) fluent-empty-command-list-callback " && ")

;; (defun fluent--add-command (command)
;;   "Internal method for add COMMAND to the `fluent-command' list."
;;   (if (and (not fluent-command)
;; 	   fluent-empty-command-list-callback)
;;       (push
;;        (mapcar (lambda (fn) (concat "{(" fn ")}")) fluent-empty-command-list-callback)
;;        fluent-command))
;;   (push command fluent-command)
;;   (fluent--message "%s added to fluent execution" command))


;; (defun fluent-switch-two-commands ()
;;   "Select two commands from the execution list and switch them."
;;   (interactive)
;;   (let ((first (ido-completing-read "first: " fluent-command))
;;         (second (ido-completing-read "second: " fluent-command)))
;;     (nthcar (seq-position fluent-command first) fluent-command)
;;     (message "%s <-> %s" first second)))

;; (defun fluent-execute-single-command ()
;;   "Select a single command from the execution list and execute it."
;;   (interactive)
;;   (let ((selected-command (ido-completing-read "execute: " fluent-command)))
;;     (fluent-compile-and-log selected-command)))

;; (defun fluent-add-command-with-input ()
;;   "Prompt for user input and add it to the fluent execution."
;;   (interactive)
;;   (fluent-add (read-string "command: ")))

;; (defvar fluent-commands-history '())
;; (add-to-list 'fluent-commands-history "ssh {build-host} \"{commands}\"")

;; (defvar fluent-remote-build-history '())
;; (defun fluent-execute ()
;;   "Give user possibility to compose a custom elisp-expression with the commands to be executed."
;;   (interactive)
;;   (let ((execution-string
;; 	 (read-string
;; 	  "command: "
;; 	  (or (car fluent-commands-history) "{commands}")
;; 	  'fluent-commands-history)))
;;     (if (string-match-p (regexp-quote "{build-host}") execution-string)
;;         (let ((build-server (read-string "build host: "
;;                                          (or (car fluent-remote-build-history) "")
;;                                          'fluent-remote-build-history)))
;;           (setq execution-string
;;                 (replace-regexp-in-string (regexp-quote "{build-host}") build-server execution-string))))
    
;;     (setq command (replace-regexp-in-string
;;                    (regexp-quote "{commands}")
;;                    (mapconcat (lambda (item) item) (reverse fluent-command) " && ")
;;                    execution-string))
;;     (fluent-compile-and-log command)))

;; (defun fluent-compile-and-log (arguments)
;;   "Run compile on the given ARGUMENTS fluent commands."
;;   (if (not (stringp arguments)) (error "Input to fluent-compile-and-log was not string!"))
;;   (setq parsed-arguments (user/compile-append-to-command arguments))
;;   (fluent--message "fluently compiling %s" parsed-arguments)
;;   (setq fluent--last-command arguments)
;;   (setq compilation-always-kill t)
;;   (compile parsed-arguments))

;; (defun fluent--message (str &rest vars)
;;   (message (apply #'format (concat "fluent [%s]: " str) (cons (current-time-string) vars))))

;; (defvar fluent--last-command '() "The last command that fluent executed.")
;; (defun fluent-recompile ()
;;   "Invoke the remopilation."
;;   (interactive)
;;   (fluent--set-compile-flags)
;;   (fluent-compile-and-log fluent--last-command))

;; (defun fluent--set-compile-flags ()
;;   "Set the compilation flags to the desired behavior."
;;   (setq compilation-always-kill t
;;         compilation-scroll-output t
;;         compilation-auto-jump-to-first-error '()))

;; (use-package ansi-color
;;   :config
;;   (defun user/buildsystem-colorize-compilation-buffer ()
;;     (read-only-mode '())
;;     (ansi-color-apply-on-region compilation-filter-start (point))
;;     (read-only-mode t))
;;   (add-hook 'compilation-filter-hook 'user/buildsystem-colorize-compilation-buffer)
;;   )

;; (fluent--set-compile-flags)

;; (global-set-key (kbd "<f5>") 'fluent-recompile)
;; (global-set-key (kbd "C-<f5>") 'fluent-add-command-with-input)
;; (global-set-key (kbd "<f6>") 'fluent-execute)
;; (global-set-key (kbd "C-<f6>") 'fluent-execute-single-command)
;; (global-set-key (kbd "<f7>") 'fluent-modify)
;; (global-set-key (kbd "C-<f7>") 'fluent-remove-command)
;; (global-set-key (kbd "<f8>") 'fluent-clear)

;; (provide 'fluent)
;; ;;; fluent ends here

(provide 'fluent)
