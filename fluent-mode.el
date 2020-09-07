;;; fluent --- Small framework for executing commands using compile

;; Author: Andreas Lindblad
;; Keywords: extensions, tools, compile
;; URL: http://github.com/andli197/fluent-mode
;; Emacs: GNU Emacs 24 (or later)
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 0.1.0

;;; Commentary:

;; Small tool for executing compile on a list of commands

;; ## Usage
;; Add commands using 'fluent-add or 'fluent-add-interactive and call
;; 'fluent-compile for compiling the current list of commands in 'compile,
;; stacked with double ampersands. The commands are executed in the order
;; they were added. 
;; for instance:
;; (fluent-add "cmake --build . --target all")
;; (fluent-add "ctest -j8")
;; (fluent-compile)
;; would result in calling compile with
;; "cmake --build . --target all && ctest -j8"

;; If a command needs modification, invoke 'fluent-modify for interactive
;; selection and modification of the command. Two commands may be switched
;; with 'fluent-switch-two-commands. To remove commandsm invoke
;; 'fluent-remove-command and to clear the execution
;; list invoke 'fluent-clear.

;; It is possible to add hooks to functions to call generating strings to
;; be appended before the compilation, adding callbacks to
;; 'fluent-prepend-compilations-commands. Adding:
;; (push "whoami" 'fluent-prepend-compilations-commands)
;; (push "uptime" 'fluent-prepend-compilations-commands)
;; would result in the command "whoami && uptime" would be prepended to
;; the full compilation string. Useful if working in an environment
;; using modules for instance.

;; It is also possible to make the compilation call ssh {build-host} and
;; give the full command as argument to ssh. Setting build host is done
;; using 'fluent-set-remote-host-interative or 'fluent-set-remote-host
;; and toggling the option is done using 'fluent-toggle-remote-compile

;; A command may contain an expression surrounded with {}. Those expressions
;; will be interpreted as elisp expressions and evaluated at compile time.

;; When compiling a command that command will be stored in 'fluent--last-command
;; and the fluent-recompile will invoke compilation using the
;; 'fluent--last-command as input. The pre-compilation parts and remote parts
;; are not stored there, so they is re-evaluated at compilation. This means
;; that a command that is executed over ssh may be invoked locally using
;; recompile. Also, the last command is storing the elisp-expression that is
;; to be evaluated at compilation. This is useful when, for instance,
;; compiling code using cmake and different build-types. Then the build-type
;; may be stored as a lisp-function and that function is returning the
;; build type. That way the command for compiling different build types
;; can be the same.

;; Single commands stored in the execution list may be executed using
;; 'fluent-compile-single-command.
;; It is possible to use fluent in single-execution-mode which is enabled using
;; 'fluent-toggle-single-command-execution. This is causing adding of commands
;; to be invoked direct instead of beeing stored in the execution list.

;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'compile)

;;;###autoload
(define-minor-mode fluent-mode
  "Minor mode for fluent execution model."
  :lighter " fluent"
  :keymap (let ((map (make-sparse-keymap)))
            ;;; IDE-style keybindings
            ;; (define-key map (kbd "<f5>") 'fluent-compile)
            ;; (define-key map (kbd "C-<f5>") 'fluent-recompile)
            ;; (define-key map (kbd "<f6>") 'fluent-add-interactive)
            ;; (define-key map (kbd "C-<f6>") 'fluent-switch-two-commands)
            ;; (define-key map (kbd "<f7>") 'fluent-modify)
            ;; (define-key map (kbd "C-<f7>") 'fluent-remove-command)
            ;; (define-key map (kbd "<f8>") 'fluent-clear)

            (define-key map (kbd "C-c c x") 'fluent-clear)
            (define-key map (kbd "C-c c c") 'fluent-compile)
            (define-key map (kbd "C-c c r") 'fluent-recompile)
            (define-key map (kbd "C-c c a") 'fluent-add-interactive)
            (define-key map (kbd "C-c m m") 'fluent-modify)
            (define-key map (kbd "C-c m s") 'fluent-switch-two-commands)
            (define-key map (kbd "C-c m r") 'fluent-remove-command)

            (define-key map (kbd "C-c r t") 'fluent-toggle-remote-compile)
            (define-key map (kbd "C-c r b") 'fluent-set-remote-host-interative)
            (define-key map (kbd "C-c s x") 'fluent-compile-single-command)
            (define-key map (kbd "C-c s s") 'fluent-toggle-single-command-execution)            
            map)
  :global t)

(defvar fluent-command
  '()
  "List of commands to be executed.")

(defvar fluent-add-interactive-history
  '()
  "History for the interactive add.")

(defvar fluent--remote-compilation
  '()
  "Flag for compiling remote or local.")

(defvar fluent--remote-build-host
  "localhost"
  "Machine for remote compilation.")

(defvar fluent--remote-build-host-history
  '()
  "History for the build hosts.")

(defvar fluent--single-command-execution
  '()
  "Flag for executing command direct or not.")

(defvar fluent--lisp-expression-finder-regexp
  "{\\(.+?\\)}"
  "Regular expression for how to decide what is to be interpreted as elisp.")

(defvar fluent--last-command
  '()
  "Last executed commands.")

(defvar fluent-prepend-compilation-commands
  '()
  "Functions to call and append result from to the current `fluent-command'")

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
    (setcar
     (nthcdr
      (seq-position fluent-command selected-command)
      fluent-command)
     new-command)
    (fluent-message "command modified: %s -> %s" selected-command new-command)))

(defun fluent-compile ()
  "Run compile on the current commands list."
  (interactive)
  (fluent--compile-and-log fluent-command))

(defun fluent-toggle-remote-compile ()
  "Switch between remote- and local compilation."
  (interactive)
  (setq fluent--remote-compilation (not fluent--remote-compilation))
  (setq status (if fluent--remote-compilation "ON" "OFF"))
  (fluent-message "Turning remote compilation %s" status))

(defun fluent-set-remote-host (host)
  "Set the remote build machine to HOST"
  (setq fluent--remote-build-host host)
  (fluent-message "Remote host set to \"%s\"" host))

(defun fluent-set-remote-host-interative ()
  "Prompt user for remote host and set it."
  (interactive)
  (let ((host (read-string "host: "
                           (or (car fluent--remote-build-host-history)
                               "localhost")
                           'fluent--remote-build-host-history)))
    (fluent-set-remote-host host)))

(defun fluent-toggle-single-command-execution ()
  "Switch between single command execution and building full command before execution."
  (interactive)
  (setq fluent--single-command-execution (not fluent--single-command-execution))
  (setq status (if fluent--single-command-execution "ON" "OFF"))
  (fluent-message "Turning single command execution %s" status))

(defun fluent--get-all-elisp-expressions-from-string (command)
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match fluent--lisp-expression-finder-regexp command pos)
        (push (match-string 0 command) matches)
        (setq pos (match-end 0)))
      matches)))

(defun fluent-evaluate-elisp-expression (input)
  "Evaluate lisp expression, either string or list as input. Results are concatinated with double ampersand."
  (cond ((stringp input)
         (fluent-evaluate-elisp-expression-string input))
        ((listp input)
         (mapconcat
          'fluent-evaluate-elisp-expression-string
          input
          " && "))
        (t (error "Invalid input to fluent-evaluate-elisp-expression: Input not a list or string"))))



(defun fluent-evaluate-elisp-expression-string (string)
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
              (fluent-evaluate-elisp-expression elisp-value)
              string))
       ))
     expressions))
  string)

(defun fluent-compile-single-command ()
  "Prompt user for command in command list and execute it."
  (interactive)
  (let ((selected-command (ido-completing-read "execute: " fluent-command)))
    (fluent--compile-and-log (list selected-command))))

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
  (fluent-evaluate-elisp-expression-string full-command))

(defun fluent--generate-compilation-command (arguments)
  "Generates the compilation command and assign to `fluent--last-command'"
  (setq fluent--last-command arguments)
  (setq prepend-command (fluent--evaluate-pre-compilation-commands))
  (setq parsed-arguments (mapconcat 'identity (reverse arguments) " && "))
  (setq full-command-list (list prepend-command parsed-arguments))
  (setq non-empty-commands
        (seq-remove
         (lambda (str) (or (eq str "") (eq str nil)))
         full-command-list))
  (fluent-message "non-empty commands %s" non-empty-commands)
  (mapconcat 'identity non-empty-commands " && "))


(defun fluent--evaluate-pre-compilation-commands ()
  "Evaluates the commands in `fluent-prepend-compilation-commands' and concatinates them with \"&&\" to prepend to the execution."
  (mapconcat (lambda (fn) (funcall fn))
             (reverse fluent-prepend-compilation-commands)
             " && "))

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

(defvar fluent-compile-custom-history
  '("{fluent-command}")
  "History for the custom compilation")
(add-to-list 'fluent-compile-custom-history "{fluent-command}")

(defun fluent-compile-custom ()
  "Give user possibility to compose a custom elisp-expression with the command to be executed. The commands in fluent is available in 'fluent-command. This means that the remote execution could be build in this command as 'ssh {fluent--remote-build-host} \"{fluent-command}\"'"
  (interactive)
  (let ((custom-command
         (read-string "command: "
                      (car fluent-compile-custom-history)
                      'fluent-compile-custom-history)))
    (fluent--compile-and-log (list custom-command))))

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

(defun fluent-colorize-compilation-buffer ()
  (read-only-mode '())
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode t))
(add-hook 'compilation-filter-hook 'fluent-colorize-compilation-buffer)

(fluent--set-compile-flags)
(setq-default display-buffer-reuse-frames t)

(eval-when-compile
  (load-file (expand-file-name "test/fluent-mode-test.el"))
  (ert "fluent"))

(provide 'fluent-mode)
;;; fluent-mode.el ends here