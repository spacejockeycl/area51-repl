(in-package #:cl-repl)

(defun version ()
  (asdf/component:component-version
   (asdf/find-system:find-system '#:cl-repl)))

(defvar *versions*
  (format nil "cl-repl ~a on ~a ~a"
          (version)
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

(defun setup-readline ()
  (cffi:load-foreign-library 'cl-readline:readline)
  (enable-syntax)
  (rl:register-function :complete #'completer)
  (init-keymap))

(defun main ()
  (setup-readline)
  (site-init)
  (unwind-protect
    (repl)
    (rl:deprep-terminal)))


