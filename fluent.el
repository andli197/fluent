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
  (fluent--generate-compilation-command)
  (fluent-message "compiling '%s'" full-command)
  (setq compilation-always-kill t)
  (compile full-command))

(defun fluent--generate-compilation-command ()
  "Generates the compilation command and assign to `fluent--last-command'"
  (setq prepend-command "")
  (if fluent-prepend-compilation-commands
      (setq prepend-command (user/compile-append-to-command (fluent-evaluate-pre-compilation-commands))))
  (setq parsed-arguments (user/compile-append-to-command (reverse arguments)))
  (setq fluent--last-command arguments)
  (setq full-command-list (list prepend-command parsed-arguments))
  (setq non-empty-commands
	(seq-remove
	 (lambda (str) (or (eq str "") (eq str nil)))
	 full-command-list))
  (setq full-command (mapconcat 'identity non-empty-commands " && ")))

(defvar fluent-prepend-compilation-commands '()
  "Commands called and append result from before the current `fluent-command'")

(defun fluent-evaluate-pre-compilation-commands ()
  "Evaluates the commands in `fluent-prepend-compilation-commands' and concatinates them with \"&&\" and prepend to the execution."
  (mapconcat (lambda (fn) (funcall fn)) fluent-prepend-compilation-commands " && "))

(require 'cl-lib)
(defun fluent-switch-two-commands ()
  "Select two commands from the execution list and switch them."
  (interactive)
  (let ((first-pos
	 (seq-position fluent-command
		       (ido-completing-read "first: " fluent-command)))
	(second-pos
	 (seq-position fluent-command
		       (ido-completing-read "second: " fluent-command))))
    (setq tmp (seq-elt fluent-command first-pos))
    (setf (seq-elt fluent-command first-pos) (seq-elt fluent-command second-pos))
    (setf (seq-elt fluent-command second-pos) tmp)
    ))

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

(defun fluent-recompile ()
  "Invoke the remopilation."
  (interactive)
  (fluent--set-compile-flags)
  (fluent--compile-and-log fluent--last-command))

(defun fluent--set-compile-flags ()
  "Set the compilation flags to the desired behavior."
  (setq compilation-always-kill t
        compilation-scroll-output t
        compilation-auto-jump-to-first-error '()))

(require 'ansi-color)
(defun fluent-colorize-compilation-buffer ()
  (read-only-mode '())
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode t))
(add-hook 'compilation-filter-hook 'fluent-colorize-compilation-buffer)

(fluent--set-compile-flags)

(global-set-key (kbd "<f5>") 'fluent-recompile)
(global-set-key (kbd "C-<f5>") 'fluent-compile)
(global-set-key (kbd "<f6>") 'fluent-add-interactive)
(global-set-key (kbd "C-<f6>") 'fluent-switch-two-commands)
(global-set-key (kbd "<f7>") 'fluent-modify)
(global-set-key (kbd "C-<f7>") 'fluent-remove-command)
(global-set-key (kbd "<f8>") 'fluent-clear)

(provide 'fluent)
;;; fluent ends here
