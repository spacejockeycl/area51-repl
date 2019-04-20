(in-package #:cl-repl)

(define-command run (filename &rest args)
  "Execute file in current enviroment."
  (declare (ignore args))
  (if (probe-file filename)
      (let ((code (format nil "(progn ~a )" (read-file-into-string filename))))
        (if (line-continue-p code)
            (message "Error: Unexpected EOF.")
            code))
      (message "Error: File not found.")))

(defun edit-file-and-read (editor filename)
  (message "Openning file: ~a~%" filename)
  (uiop:run-program (list editor filename)
                    :input :interactive
                    :output :interactive)
  (message "Evaluating edited code...~%")
  (invoke-command "%run" filename))

(define-command edit (&optional filename &rest args)
  "Edit code with text editor specified by $EDITOR."
  (declare (ignore args))
  (let ((editor (uiop:getenv "EDITOR")))
    (if (null filename)
        (uiop:with-temporary-file
            (:stream s :pathname p :type "lisp" :prefix "cl-repl-edit" :suffix "")
          (setf filename (namestring p))
          (message "CL-REPL will make a temporary file named: ~a~%" filename)
          (format s "#|-*- mode:lisp -*-|#~2%")
          (close s)
          (edit-file-and-read editor filename))
        (edit-file-and-read editor filename))))

(define-command cd (&optional (dest (uiop:getenv "HOME")) &rest args)
  "Change working directory."
  (declare (ignore args))
  (handler-case
      (progn
        (setf dest (truename dest))
        (uiop:chdir dest)
        (setf *default-pathname-defaults* dest)
        (format nil "~s"dest))
    (error () (message "No such directory."))))

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

(define-command inspect (object &rest args)
  "Alias to (inspect <object>)."
  (declare (ignore args))
  (let ((code (format nil "(inspect ~a)" object)))
    (if (line-continue-p code)
        (message "Error: Unexpected EOF.")
        code)))

(define-command step (&rest forms)
  "Alias to (step <form>)."
  (let ((code (format nil "(step ~{ ~a~})" forms)))
    (if (line-continue-p code)
        (message "Error: Unexpected EOF.")
        code)))

#+quicklisp
(define-command load (&rest systems)
  "Alias to (ql:quickload '(<system>...) :silent t)."
  (loop :repeat (length systems)
        :for system := (pop systems) :while system
        :do (handler-case
                (progn
                  (ql:quickload (intern system :keyword) :silent t)
                  (message "Loaded.: `~a`" system))
              (error (c) (message "Failed to load system.: `~a`: ~a" system c)))
        :when (car systems) :do (terpri)
        :finally (return "nil")))

(define-command package (&optional (package "cl-user") &rest args)
  "Alias to (in-pacakge <package>)."
  (declare (ignore args))
  (handler-case
      (let ((p (current-package)))
        (setf *package* (find-package (read-from-string package)))
        (message "Package changed.: From ~(`~a` into `~a`~)" p (current-package)))
    (error () (message "Failed to change package."))))


(define-command doc (target &rest args)
  "Show description of given object."
  (declare (ignore args))
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

(define-command clear (&rest args)
  "Clear screen."
  (declare (ignore args))
  (uiop:run-program "clear" :output *standard-output*)
  (when (> *debugger-level* 0)
    (debugger-banner))
  (read-input))

#+out-of-date
(define-command help (&rest args)
  "List available command commands and usages."
  (declare (ignore args))
  (loop :for (name body) :in *command*
        :do (format t "~16,,a~a~%" name (documentation body 'function)))
  "nil")

