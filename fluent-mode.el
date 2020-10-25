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
;; Add commands using 'fluent-add and call 'fluent-compile for compiling
;; the current list of commands in 'compile, stacked with double ampersands.
;; The commands are executed in the order they were added. 
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
;; 'fluent-prepend-compilation-commands. Adding:
;; (push "whoami" 'fluent-prepend-compilation-commands)
;; (push "uptime" 'fluent-prepend-compilation-commands)
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

(defgroup fluent-mode '()
  "fluent execution mode"
  :group 'tools
  :tag "fluent mode")

;;;###autoload
(define-minor-mode fluent-mode
  "Minor mode for fluent execution model."
  :lighter " fluent"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-f c x") 'fluent-clear)
            (define-key map (kbd "C-c C-f c c") 'fluent-compile)
            (define-key map (kbd "C-c C-f c r") 'fluent-recompile)
            (define-key map (kbd "C-c C-f m a") 'fluent-add)
            (define-key map (kbd "C-c C-f m m") 'fluent-modify)
            (define-key map (kbd "C-c C-f m s") 'fluent-switch-two-commands)
            (define-key map (kbd "C-c C-f m x") 'fluent-remove-command)

            (define-key map (kbd "C-c C-f r t") 'fluent-toggle-remote-compile)
            (define-key map (kbd "C-c C-f r b") 'fluent-set-remote-host)
            (define-key map (kbd "C-c C-f x s") 'fluent-compile-single-command)
            (define-key map (kbd "C-c C-f x t") 'fluent-toggle-single-command-execution)            
            map)
  :global t
  :group 'fluent-mode)


(defcustom fluent--remote-build-host
  "localhost"
  "Machine for remote compilation."
  :type 'string
  :group 'fluent-mode)

(defcustom fluent--single-command-execution
  '()
  "Flag for executing command direct or not."
  :type 'boolean
  :group 'fluent-mode)

(defcustom fluent-compilation-buffer-name
  "*fluent-compilation*"
  "Name of the compilation buffer to use for fluent"
  :type 'string
  :group 'fluent-mode)

(defcustom fluent-single-compilation-mode
  t
  "If set to '(), multiple ongoing compilations will be allowed, using the `fluent-compilation-buffer-name' as base for the created buffers"
  :type 'boolean
  :group 'fluent-mode)

(defvar fluent-command
  '()
  "List of commands to be executed.")

(defvar fluent-add-interactive-history
  '()
  "History for the interactive add.")

(defvar fluent--remote-compilation
  '()
  "Flag for compiling remote or local.")

(defvar fluent--remote-build-host-history
  '()
  "History for the build hosts.")

(defvar fluent--lisp-expression-finder-regexp
  "{\\(.+?\\)}"
  "Regular expression for how to decide what is to be interpreted as elisp.")

(defvar fluent--last-command
  '()
  "Last executed commands.")

(defvar fluent-prepend-compilation-commands
  '()
  "Functions to call and append result from to the current `fluent-command'.")

(defvar fluent-compile-custom-history
  '("{fluent-command}")
  "History for the custom compilation. 
The default value is the current `fluent-command'.")

(defun fluent-message (str &rest vars)
  "Display a fluent status message."
  (message (apply #'format (concat "fluent [%s]: " str)
                  (cons (current-time-string)
                        vars))))

(defun fluent-add (command)
  "Add the COMMAND to the fluent execution command list."
  (interactive
   (let ((command (read-string-default-first-in-history "command: " 'fluent-add-interactive-history)))
     (list command)))
  (if fluent--single-command-execution
      (fluent--compile-and-log (list command))
    (progn
      (fluent-message "command \"%s\" added to execution list." command)
      (push command fluent-command))))

(defun read-string-default-first-in-history (prompt history)
  "Read string with history and use the first value for default value."
  (read-string prompt (car (eval history)) history))

(defun fluent-completing-read (prompt choises)
  "Completing read with requirement on selecting match to the items."
  (let ((require-match t)
        (predicate '()))
    (ido-completing-read prompt choises predicate require-match)))

(defun fluent-remove-command ()
  "Select a command from the execution list and remove it."
  (interactive)
  (let ((selected-command (fluent-completing-read "remove: " fluent-command)))
    (setq fluent-command (remove selected-command fluent-command))))

(defun fluent-clear ()
  "Clears the current list of commands inserted to the fluent execution."
  (interactive)
  (setq fluent-command '())
  (fluent-message "commands cleared"))

(defun fluent-modify ()
  "Select command in `fluent-command' and modify it."
  (interactive)
  (let* ((selected-command (fluent-completing-read "modify: " fluent-command))
         (selected-position (seq-position fluent-command selected-command))
         (new-command (read-string "modification: " selected-command)))
    (setcar (nthcdr selected-position fluent-command) new-command)
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
  (interactive
   (let ((host (read-string-default-first-in-history "host: " 'fluent--remote-build-host-history)))
     (list host)))
  (setq fluent--remote-build-host host)
  (fluent-message "Remote host set to \"%s\"" host))

(defun fluent-toggle-single-command-execution ()
  "Switch between single command execution and building full command before execution."
  (interactive)
  (setq fluent--single-command-execution (not fluent--single-command-execution))
  (let ((status (if fluent--single-command-execution "ON" "OFF")))
    (fluent-message "Turning single command execution %s" status)))

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
         (mapconcat 'fluent-evaluate-elisp-expression-string input " && "))
        (t
         (error
          "Invalid input to fluent-evaluate-elisp-expression: Input not a list or string"))))



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
  (let ((selected-command (fluent-completing-read "execute: " fluent-command)))
    (fluent--compile-and-log (list selected-command))))


(defun rename-that-buffer (buffer-or-name new-name &optional kill-pre-existing-buffer)
  "Rename the buffer or name to the new-name. Does not affect the current buffer."
  (let ((cbuf (get-buffer (buffer-name)))
        (new-buffer-name (generate-new-buffer-name new-name)))
    (if (and kill-pre-existing-buffer
             (not (string-equal new-buffer-name new-name)))
        (progn
          (kill-buffer new-name)
          (setq new-buffer-name new-name)))
    (if (get-buffer buffer-or-name)
        (progn
          (set-buffer buffer-or-name)
          (rename-buffer new-buffer-name)
          (set-buffer cbuf)
          new-buffer-name)
      nil)))

(defun fluent--compile-and-log (arguments)
  "Run compile on the given ARGUMENTS fluent commands."
  (let ((full-command (fluent--generate-full-compilation-command arguments))
        (pre-existing-compilation-buffer
         (rename-that-buffer "*compilation*" "tmp")))
    (fluent-message "compiling: '%s'" full-command)
    (if fluent-single-compilation-mode
        (rename-that-buffer fluent-compilation-buffer-name "*compilation*"))
    (compile full-command)
    (rename-that-buffer
     "*compilation*"
     fluent-compilation-buffer-name
     fluent-single-compilation-mode)
    (if pre-existing-compilation-buffer
        (rename-that-buffer pre-existing-compilation-buffer "*compilation*"))))

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
  (let* ((prepend-command
          (seq-map (lambda (fn) (funcall fn))
                   (reverse fluent-prepend-compilation-commands)))
         (full-command-list
          (seq-concatenate 'list prepend-command (reverse arguments))))
    (mapconcat 'identity full-command-list " && ")))

(defun fluent-switch-two-commands ()
  "Select two commands from the execution list and switch them."
  (interactive)
  (let ((first-pos
         (seq-position
          fluent-command (fluent-completing-read "first: " fluent-command)))
        (second-pos
         (seq-position
          fluent-command (fluent-completing-read "second: " fluent-command))))
    (cl-rotatef
     (seq-elt fluent-command first-pos)
     (seq-elt fluent-command second-pos))))

(defun fluent-compile-custom ()
  "Give user possibility to compose a custom elisp-expression with the command to be executed. The commands in fluent is available in 'fluent-command. This means that the remote execution could be build in this command as 'ssh {fluent--remote-build-host} \"{fluent-command}\"'"
  (interactive)
  (let ((custom-command
         (read-string-default-first-in-history
          "command: "
          'fluent-compile-custom-history)))
    (fluent--compile-and-log (list custom-command))))

(defun fluent-recompile ()
  "Invoke the remopilation."
  (interactive)
  (fluent--compile-and-log fluent--last-command))

(defun fluent-colorize-compilation-buffer ()
  (read-only-mode '())
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode t))
(add-hook 'compilation-filter-hook 'fluent-colorize-compilation-buffer)

(setq compilation-always-kill t
      compilation-scroll-output t
      compilation-auto-jump-to-first-error '())
(setq-default display-buffer-reuse-frames t)

;; (eval-when-compile
;;   (load-file (expand-file-name "./test/fluent-mode-test.el"))
;;   (ert "fluent"))

(provide 'fluent-mode)
;;; fluent-mode.el ends here
