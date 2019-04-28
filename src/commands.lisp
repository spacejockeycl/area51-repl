(in-package #:cl-repl)

(define-command run (filename)
  "Execute file in current enviroment."
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

(define-command edit-string (string)
    "Edit string with text editor specified by $EDITOR.
Creates a temporary file with the string's content.
Returns a new string."
  (let ((editor (uiop:getenv "EDITOR")))
    (uiop:with-temporary-file
        (:stream output
         :pathname pathname
         :type "lisp"
         :prefix "cl-repl-edit"
         :suffix "")
      (write-string string output)
      (close output)
      (edit-file-and-read editor pathname))))

(define-command edit (&optional filename)
  "Edit code with text editor specified by $EDITOR."
  (let ((editor (uiop:getenv "EDITOR")))
    (if (null filename)
        (uiop:with-temporary-file
            (:stream output
             :pathname pathname
             :type "lisp"
             :prefix "cl-repl-edit"
             :suffix "")
          (message "~&editing temporary file \"~a\"~%" (namestring pathname))
          (format output  "#|-*- mode:lisp -*-|#~2%")
          (close output)
          (edit-file-and-read editor filename))
        (edit-file-and-read editor filename))))

(define-command cd (&optional (dest (uiop:getenv "HOME")))
  "Change working directory."
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

(define-command package (&optional (package "cl-user"))
  "Alias to (in-pacakge <package>)."
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
         (max-name-length (loop :for name :being :the :hash-key :of commands
                            :maximize (length name))))
    (loop :for name :being :the :hash-key :of commands
            :using (hash-value command)
          :do (format t "~v,,a~a~%"
                      (min 16 (+ 2 max-name-length))
                      name
                      (first-line (command-description command))))))

(define-command swank ()
    "load swank and create a server"
    (require "swank")
  (uiop:symbol-call :swank :create-server :dont-close t))

