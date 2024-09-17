(in-package #:area51-repl)

(defun version ()
  (asdf/component:component-version
   (asdf/find-system:find-system '#:area51-repl)))

(defvar *versions*
  (format nil "area51-repl ~a on ~a ~a"
          (version)
          (lisp-implementation-type)
          (lisp-implementation-version)))

(defun setup-readline ()
  (cffi:load-foreign-library 'cl-readline:readline)
  (rl:register-function :complete #'complete)
  (init-keymap))

(defun start ()
  (setup-readline)
  (unwind-protect
    (repl)
    (rl:deprep-terminal)))


