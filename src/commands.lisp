(in-package #:area51-repl)

(define-command cd (&optional (destination (uiop:getenv "HOME")))
  "Change working directory."
  (handler-case
      (progn
        (setf destination (truename destination))
        (uiop:chdir destination)
        (setf *default-pathname-defaults* destination)
        (format t "~s" destination))
    (error () (message "No such directory."))))

(define-command pwd ()
    "Show current working directory."
  (format t "~s~%" *default-pathname-defaults*))

(define-command time (&rest forms)
  "Alias to (time <form>)."
  (let ((code (format nil "(time ~{ ~a~})" forms)))
    (if (line-continue-p code)
        (message "Error: Unexpected EOF.")
        code)))

(define-command expand (&rest forms)
  "Alias to (macroexpand-1 (quote <form>))"
  (let ((code (format nil "(macroexpand-1 '~{ ~a~})" forms)))
    (if (line-continue-p code)
        (message "Error: Unexpected EOF.")
        code)))

(define-command package (&optional (package "cl-user"))
  "Alias to (in-package <package>)."
  (handler-case
      (let ((p (current-package)))
        (setf *package* (find-package (read-from-string package)))
        (message "Package changed.: From ~(`~a` into `~a`~)" p (current-package)))
    (error () (message "Failed to change package."))))


(define-command doc (target)
  "Show description of given object."
  (handler-case
      (let ((s (make-array '(0)
                           :element-type 'base-char
                           :fill-pointer 0
                           :adjustable t)))
        (with-output-to-string (sb s)
          (describe (read-from-string target) sb))
        (invoke-pager s)
        "nil")
    (error () (message "No description given on `~a.`" target))))

(define-command help (&rest args)
    "List available command commands and usages."
  (declare (ignore args))
  (let* ((commands *commands*)
         (keys (sort (hash-table-keys commands) #'string<))
         (max-name-length (loop :for name :being :the :hash-key :of commands
                                :maximize (length name))))
    (loop :for name :in keys
          :for command = (gethash name commands)
          :do (format t ".~v,,a~a~%"
                      (min 16 (+ 2 max-name-length))
                      name
                      (first-line (command-description command))))))

(define-command swank ()
    "load swank and create a server"
    (require "swank")
  (uiop:symbol-call :swank :create-server :dont-close t))

#+quicklisp
(define-command quickload (&rest systems)
    "load systems with quicklisp"
    (ql:quickload systems))

