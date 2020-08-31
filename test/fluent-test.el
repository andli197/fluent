;;; fluent-test --- Tests for fluent mode
;;; Commentary:

;; Regression testing for package fluent.

;;; Code:
;; (require 'fluent-execution)

(defun fluent--default-reset ()
  (setq fluent-command '()
	fluent-add-interactive-history '()
	fluent--last-command '()
	fluent--remote-compilation '()
	fluent--remote-build-host '()
	fluent-prepend-compilation-commands '()
	fluent--single-command-execution '()
	))

(ert-deftest fluent-add--can-add-custom-command-to-execution-list ()
  (fluent--default-reset)
  (fluent-add "test command")
  (should (= (length fluent-command) 1))
  (should (string-equal (car fluent-command) "test command")))

(ert-deftest fluent-clear--is-removing-all-commands ()
  (fluent--default-reset)
  (fluent-add "first")
  (fluent-add "second")
  (fluent-add "third")
  (should (= (length fluent-command) 3))
  (fluent-clear)
  (should (= (length fluent-command) 0)))

(ert-deftest fluent-toggle-remote-compile--can-toggle-remote-compilation ()
  (fluent--default-reset)
  (fluent-toggle-remote-compile)
  (should (eq fluent--remote-compilation t))
  (fluent-toggle-remote-compile)
  (should (eq fluent--remote-compilation '())))

(ert-deftest fluent--remote-build-host--can-be-custom-set ()
  (fluent--default-reset)
  (fluent-set-remote-host "abcdef")
  (should (string-equal fluent--remote-build-host "abcdef")))

(ert-deftest fluent--generate-compilation-command--empty-command-result-in-empty-string ()
  (fluent--default-reset)
    (should
     (string-equal (fluent--generate-compilation-command '()) "")))

(ert-deftest fluent--generate-compilation-command--separates-list-arguments-by-dual-ampersands-and-reverses-order ()
  (fluent--default-reset)
  (should
   (string-equal
    (fluent--generate-compilation-command '("first" "second" "third"))
    "third && second && first")))

(ert-deftest fluent--generate-compilation-command--stores-last-command ()
  (fluent--default-reset)
  (fluent--generate-compilation-command '("first" "second" "third"))
  (should (equal fluent--last-command '("first" "second" "third"))))

(ert-deftest fluent--generate-full-compilation-command--is-not-adding-ssh-when-disabled ()
  (fluent--default-reset)
  (should
   (string-prefix-p
    "test"
    (fluent--generate-full-compilation-command '("test")))))

(ert-deftest fluent--generate-full-compilation-command--is-adding-the-ssh-command-when-set-to-remote ()
  (fluent--default-reset)
  (fluent-set-remote-host "localhost")
  (setq fluent--remote-compilation t)
  (should
   (string-prefix-p
    "ssh localhost"
    (fluent--generate-full-compilation-command '("test")))))

(ert-deftest fluent--last-command--does-not-store-ssh-or-host ()
  (fluent--default-reset)
  (fluent-set-remote-host "123.456.789.0")
  (setq fluent--remote-compilation t)
  (fluent--generate-full-compilation-command '("c" "b" "a"))
  (should
   (equal fluent--last-command '("c" "b" "a"))))

(ert-deftest fluent-prepend-compilation-commands--are-used-when-building-command-but-not-included-in-last-command ()
  (fluent--default-reset)
  (push '(lambda () "first") fluent-prepend-compilation-commands)
  (push '(lambda () "second") fluent-prepend-compilation-commands)
  (should
   (string-prefix-p
    "first && second"
    (fluent--generate-full-compilation-command '("c" "b" "a"))))
  (should (equal fluent--last-command '("c" "b" "a"))))

(ert-deftest fluent--get-all-elisp-expressions-from-string ()
  (fluent--default-reset)
  (should
   (equal
    (fluent--get-all-elisp-expressions-from-string "")
    '()))
  (should
   (equal
    (fluent--get-all-elisp-expressions-from-string "ls -la")
    '()))
  (should
   (equal
    (fluent--get-all-elisp-expressions-from-string "{foobar}")
    '("{foobar}")))
  (should
   (equal
    (seq-difference
     (fluent--get-all-elisp-expressions-from-string "{foo}{bar}")
     '("{bar}" "{foo}"))
    '())))

(setq test-variable "variable value")
(defun test-function () "function value")

(ert-deftest fluent-evaluate-elisp-commands-and-replace-in-string ()
  (fluent--default-reset)
  (should
   (equal (fluent-evaluate-elisp-commands-and-replace-in-string "")
	  ""))
  (should
   (equal (fluent-evaluate-elisp-commands-and-replace-in-string "foobar")
	  "foobar"))
  (should
   (equal
    (fluent-evaluate-elisp-commands-and-replace-in-string "{test-variable}")
    "variable value"))
  (should
   (equal
    (fluent-evaluate-elisp-commands-and-replace-in-string "{(test-function)}")
    "function value"))
  (should
   (equal
    (fluent-evaluate-elisp-commands-and-replace-in-string
     "{(test-function)} and {test-variable}")
    "function value and variable value"))
  (should
   (equal
    (fluent-evaluate-elisp-commands-and-replace-in-string
     "{(string-join (list (test-function) test-variable) \", \")}")
    "function value, variable value"))
  )

(defvar test-tmp "first")
(defun test-mutable-function () (format "function: %s" test-tmp))
(defun uptime-hook () "uptime")
(defvar test-host "192.168.0.1")
(defun remote-build-host () (format "%s" test-host))

(ert-deftest fluent-compile-accepts-lisp-functions-and-variables-and-evaluates-it-at-compilation ()
  (fluent--default-reset)
  (push 'uptime-hook fluent-prepend-compilation-commands)
  (setq test-tmp "first")
  (should
   (equal
    (fluent--generate-full-compilation-command '("{(test-mutable-function)}"))
    "uptime && function: first"))
  (setq test-tmp "second")
  (should
   (equal
    (fluent--generate-full-compilation-command '("{(test-mutable-function)}"))
    "uptime && function: second"))
  (should
   (equal
    (fluent--generate-full-compilation-command '("{test-tmp}"))
    "uptime && second"))
  (fluent-set-remote-host "{(remote-build-host)}")
  (setq fluent--remote-compilation t)
  (should
   (equal
    (fluent--generate-full-compilation-command '("{test-tmp}"))
    "ssh 192.168.0.1 \"uptime && second\""))
  )

(ert-deftest fluent-execute-commands-direct-option ()
  (fluent--default-reset)
  (fluent-toggle-single-command-execution)
  (should (equal fluent--single-command-execution t)))

(provide 'fluent-test)
;;; fluent-test ends here
