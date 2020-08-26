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

(ert-deftest fluent-remote-compile--can-toggle-remote-compilation ()
  (fluent--default-reset)
  (fluent-remote-compile)
  (should (eq fluent--remote-compilation t))
  (fluent-remote-compile)
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

(provide 'fluent-test)
;;; fluent-test ends here
