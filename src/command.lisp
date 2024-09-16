(in-package #:area51-repl)

(defstruct command
  name
  description
  function)

(defvar *commands*
  (make-hash-table :test #'equal)
  "Contains the commands that can be run at the toplevel repl with the \"%\" prefix.")

(defmacro define-command (name args description &body body)
  "Define a command, it's just like a function."
  (multiple-value-bind (actual-body declarations documentation)
      (parse-body body)
      (once-only ((description description)
                  (symbol (if (symbolp name)
                              (list 'quote name)
                              (symbolicate name))))
        `(progn
            ;; This check is made at runtime to be able to programmatically
            ;; generate commands with proper description
            (check-type ,description string)
            (setf (gethash (string-downcase ,symbol) *commands*)
              (make-command 
                :name ,symbol
                :description ,description
                :function #'(lambda ,args
                              ,@declarations
                              ,(or documentation description)
                              ,@actual-body)))))))

(defun message (message &rest args)
  "Print a message on standard output, with color"
  (progn
    (format t (color *message-color* message) args)
    (finish-output)
    "nil"))

(defun find-command (name)
  ;; name has the form ".name"
  (gethash (subseq name 1) *commands*))

(defun find-command-function (name)
  "Find the command's function."
  (command-function (find-command name)))

(defun invoke-command (name &rest args)
    (if (find-command name)
        (apply (find-command-function name) args)
        (message "Command not found: ~a" name)))

(defun command-p (&optional input)
  "Is the given input a command?"
  (starts-with #\. input))

