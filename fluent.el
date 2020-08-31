;;; fluent --- Small framework for executing commands using compile

;; Author: Andreas Lindblad
;; Keywords: extensions, tools, compile
;; URL: http://github.com/andli197/fluent
;; Emacs: GNU Emacs 24 (or later)
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 0.1.0

;;; Commentary:

;; Small tool for executing compile on a list of commands

;; ## Usage
;; `C-c r t' : Toggle remote compile option
;; `C-c r b' : Manually set remote compilation host
;; `C-c s x' : Compile single command from command list
;; `C-c s s' : Toggle single command execution. Tells the system not to add commands to the execution list but execute them direct.

;; Code:
(defvar fluent-command '() "List of commands to be executed.")

(require 'compile)
;; (setq-default display-buffer-reuse-frames t)

(defun fluent-message (str &rest vars)
  "Display a fluent status message."
  (message (apply #'format (concat "fluent [%s]: " str)
		  (cons (current-time-string)
			vars))))

(defun fluent-add (command)
  "Add the COMMAND to the fluent execution command list."
  (if fluent--single-command-execution
      (fluent--compile-and-log (list command))
    (progn
      (fluent-message "command \"%s\" added to execution list." command)
      (push command fluent-command))))

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
  (let* ((selected-command (ido-completing-read "modify: " fluent-command))
         (new-command (read-string "modification: " selected-command)))
    (setcar (nthcdr (seq-position fluent-command selected-command) fluent-command) new-command)
    (fluent-message "command modified: %s -> %s" selected-command new-command)))

(defun fluent-compile ()
  "Run compile on the current commands list."
  (interactive)
  (fluent--compile-and-log fluent-command))

(defvar fluent--remote-compilation '() "Flag for compiling remote or local.")
(defun fluent-toggle-remote-compile ()
  "Switch between remote- and local compilation."
  (interactive)
  (setq fluent--remote-compilation (not fluent--remote-compilation))
  (setq status (if fluent--remote-compilation "ON" "OFF"))
  (fluent-message "Turning remote compilation %s" status))

(defvar fluent--remote-build-host "localhost" "Machine for remote compilation.")
(defun fluent-set-remote-host (host)
  "Set the remote build machine to HOST"
  (interactive)
  (setq fluent--remote-build-host host)
  (fluent-message "Remote host set to \"%s\"" host))

(defvar fluent--remote-build-host-history '() "History for the build hosts.")
(defun fluent-set-remote-host-interative ()
  "Prompt user for remote host and set it."
  (interactive)
  (let ((host (read-string "host: " (or (car fluent--remote-build-host-history) "localhost") 'fluent--remote-build-host-history)))
    (fluent-set-remote-host host)))

(defvar fluent--single-command-execution '() "Flag for executing command direct or not.")
(defun fluent-toggle-single-command-execution ()
  "Switch between single command execution and building full command before execution."
  (interactive)
  (setq fluent--single-command-execution (not fluent--single-command-execution))
  (setq status (if fluent--single-command-execution "ON" "OFF"))
  (fluent-message "Turning single command execution %s" status))

(defvar fluent--lisp-expression-finder-regexp "{\\(.+?\\)}")
(defun fluent--get-all-elisp-expressions-from-string (command)
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match fluent--lisp-expression-finder-regexp command pos)
        (push (match-string 0 command) matches)
        (setq pos (match-end 0)))
      matches)))

(defun fluent-evaluate-elisp-commands-and-replace-in-string (string)
  "Locate all places with \"{}\" in the STRING and evaluate it as elisp and replace its value with the original value in the string"
  (let ((expressions (fluent--get-all-elisp-expressions-from-string string)))
    (seq-do
     (lambda (expression)
       (let* ((elisp-value
	       (eval
		(read
		 (format "%s"
			 (substring expression 1 (- (length expression) 1)))))))
       (setq string
	     (replace-regexp-in-string
	      (regexp-quote expression)
	      elisp-value
	      string))))
     expressions))
  string)

(defun fluent-compile-single-command ()
  "Prompt user for command in command list and execute it."
  (interactive)
  (let ((selected-command (ido-completing-read "execute: " fluent-command)))
    (fluent--compile-and-log (list selected-command))))

(defvar fluent--last-command '() "Last executed commands.")
(defun fluent--compile-and-log (arguments)
  "Run compile on the given ARGUMENTS fluent commands."
  (setq full-command (fluent--generate-full-compilation-command arguments))
  (fluent-message "compiling '%s'" full-command)
  (setq compilation-always-kill t)
  (compile full-command))

(defun fluent--generate-full-compilation-command (arguments)
  "Generates the full compilation command with remote host."
  (setq full-command (fluent--generate-compilation-command arguments))
  (if fluent--remote-compilation
      (setq full-command
	    (concat "ssh "
		    fluent--remote-build-host
		    " \"" full-command "\"")))
  (fluent-evaluate-elisp-commands-and-replace-in-string full-command))

(defun fluent--generate-compilation-command (arguments)
  "Generates the compilation command and assign to `fluent--last-command'"
  (setq fluent--last-command arguments)
  (setq prepend-command (fluent-evaluate-pre-compilation-commands))
  (setq parsed-arguments (mapconcat 'identity (reverse arguments) " && "))
  (setq full-command-list (list prepend-command parsed-arguments))
  (setq non-empty-commands
	(seq-remove
	 (lambda (str) (or (eq str "") (eq str nil)))
	 full-command-list))
  (mapconcat 'identity non-empty-commands " && "))

(defvar fluent-prepend-compilation-commands '()
  "Commands called and append result from before the current `fluent-command'")

(defun fluent-evaluate-pre-compilation-commands ()
  "Evaluates the commands in `fluent-prepend-compilation-commands' and concatinates them with \"&&\" to prepend to the execution."
  (mapconcat (lambda (fn) (funcall fn)) (reverse fluent-prepend-compilation-commands) " && "))

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
    (cl-rotatef
     (seq-elt fluent-command first-pos)
     (seq-elt fluent-command second-pos))))

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
(global-set-key (kbd "C-c r t") 'fluent-toggle-remote-compile)
(global-set-key (kbd "C-c r b") 'fluent-set-remote-host-interative)
(global-set-key (kbd "C-c s x") 'fluent-compile-single-command)
(global-set-key (kbd "C-c s s") 'fluent-toggle-single-command-execution)

(eval-when-compile
  (load-file "test/fluent-test.el")
  (ert "fluent"))

(provide 'fluent)
;;; fluent ends here
