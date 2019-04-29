(in-package #:cl-repl)

;; Having commands is a pretty common pattern, for inspiration, take a look at:
;;
;; SBCL ACL REPL
;; https://github.com/sbcl/sbcl/blob/419bef671943cd71ef6c4a097551c893742dddde/contrib/sb-aclrepl/repl.lisp
;;
;; StumpWM
;; https://github.com/stumpwm/stumpwm/blob/4653857b5039d29ef2b0fedfe3e4c656bd000c41/command.lisp
;;
;; Climacs (v2)
;; https://github.com/robert-strandh/Second-Climacs/blob/ed1dda65ce51a713207d88915c71ae56e15b7c23/Command/command.lisp


(defstruct command
  name
  description
  function)

(defvar *commands*
  (make-hash-table :test #'equal)
  "Contains the commands that can be run at the toplevel repl with the \"%\" prefix.")

(parse-body '((declare (ignore args))
              (if (probe-file filename)
                  (let ((code (format nil "(progn ~a )" (read-file-into-string filename))))
                    (if (line-continue-p code)
                        (message "Error: Unexpected EOF.")
                        code))
                  (message "Error: File not found.")))
            :documentation "Asdf")

(defmacro define-command (name args description &body body)
  "Define a command, it's just like a function."
  (multiple-value-bind (actual-body declarations documentation)
      (parse-body body)
      (once-only ((description description)
                  (symbol (if (symbolp name)
                              (list 'quote name)
                              (symbolicate name))))
        `(progn
           (check-type ,description string)
           (setf (gethash (string-downcase ,symbol) *commands*)
                 (make-command :name ,symbol
                               :description ,description
                               :function #'(lambda ,args
                                             ,@declarations
                                             ,(or documentation description)
                                             ,@actual-body)))))))

;; TODO use a function instead
(defmacro message (message &rest args)
  "Print a message on the standard ouput, with color"
  `(progn
     (format t ,(color *message-color* message) ,@args)
     (finish-output)
     "nil"))

(defun find-command (name)
  ;; name has the form "%name"
  (gethash (subseq name 1) *commands*))

(defun find-command-function (name)
  "Find the command's function."
  (command-function (find-command name)))

(defun invoke-command (name &rest args)
    (if (find-command name)
        (apply (find-command-function name) args)
        (message "Command not found.: ~a" name)))

(defun command-p (&optional input)
  "Is the given input a command?"
  (starts-with #\% input))

