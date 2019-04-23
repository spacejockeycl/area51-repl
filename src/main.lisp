(in-package #:cl-repl)

(defconstant +version+ '0.6.4)

(defvar *versions*
  (format nil "cl-repl ~a on ~a ~a"
          +version+
          (lisp-implementation-type)
          (lisp-implementation-version)))

;; TODO use xdg's ~/.config folder instead
(defvar *site-init-path* #P"~/.replrc")

(defun site-init ()
  (unless (probe-file *site-init-path*)
    (return-from site-init))
  (handler-case (load *site-init-path*)
    (error (c)
      (progn
        (format *error-output* "Failed to load ~a, quitting.~%[~a]~%" *site-init-path* c)
        (uiop:quit 1)))))

(defparameter *repl-flush-screen-p* nil
  "Do you want to flush the screen before and after the repl is run?")

(defun setup-readline ()
  (cffi:load-foreign-library 'cl-readline:readline)
  (enable-syntax)
  (rl:register-function :complete #'completer)
  (install-inspector))

(defun main ()
  (setup-readline)
  (site-init)
  (when *repl-flush-screen-p* (flush-screen))
  (with-cursor-hidden
    (format t "~a~2%" *versions*))
  ;; (in-package :cl-user)
  (unwind-protect
    (conium:call-with-debugger-hook #'debugger #'repl)
    (rl:deprep-terminal))
  (when *repl-flush-screen-p* (flush-screen)))


