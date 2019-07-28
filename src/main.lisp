(in-package #:cl-repl)

(defun version ()
  (asdf/component:component-version
   (asdf/find-system:find-system '#:cl-repl)))

(defvar *versions*
  (format nil "cl-repl ~a on ~a ~a"
          (version)
          (lisp-implementation-type)
          (lisp-implementation-version)))

(defun setup-readline ()
  (cffi:load-foreign-library 'cl-readline:readline)
  (enable-syntax)
  (rl:register-function :complete #'complete)
  (init-keymap))

(defun main ()
  (setup-readline)
  (unwind-protect
    (repl)
    (rl:deprep-terminal)))


