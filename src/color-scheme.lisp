(in-package :area51-repl)

;; States
(defparameter *default-prompt-color* nil)
(defparameter *output-indicator-color* nil)
(defparameter *splash-color* nil)
(defparameter *condition-color* nil)
(defparameter *section-color* nil)
(defparameter *message-color* nil)

;; Syntaxic coloring
(defparameter *string-syntax-color* nil)
(defparameter *variable-syntax-color* nil)
(defparameter *constant-syntax-color* nil)
(defparameter *lambda-syntax-color* nil)
(defparameter *definition-syntax-color* nil)
(defparameter *keyword-syntax-color* nil)
(defparameter *special-syntax-color* nil)
(defparameter *function-syntax-color* nil)
(defparameter *boolean-syntax-color* nil)
(defparameter *normal-syntax-color*  nil)


(defvar *color-schemes* (make-hash-table :test #'equal)
  "hash-table of the color-schemes, keyed by their name as string")

(defun find-color-scheme (name)
  (check-type name string)
  (gethash name *color-schemes*))

(defmacro define-color-scheme (name (&optional parent) &body specs)
  (check-type name string)
  `(setf (gethash ,name *color-schemes*) (cons ,parent ',specs)))

;; FIXME code smell: eval
(defun set-color (spec color)
  (eval (read-from-string (format nil "(setf area51-repl::*~a-color* ~a)" spec color))))

(defun color-scheme (name)
  (let ((scheme (find-color-scheme name)))
    (when (car scheme)
      (color-scheme (car scheme)))
    (loop :for (spec color) :in (cdr scheme)
          :do (set-color spec color))))

(define-color-scheme "default" ()
  ("string-syntax" +yellow+)            ; 184
  ("variable-syntax" +chartreuse+)      ; 118
  ("constant-syntax" +chartreuse+)
  ("lambda-syntax" +deep-blue+)
  ("definition-syntax" +chartreuse+)
  ("keyword-syntax" +deep-blue+)
  ("special-syntax" +deep-pink+)        ; 197
  ("function-syntax" +deep-pink+)
  ("boolean-syntax" +deep-pink+)
  ("normal-syntax" nil)
  ("default-prompt" +green+)
  ("output-indicator" +red+)
  ("condition" +red+)
  ("section" +blue+)                    ; 21
  ("message" +grey18+))

(color-scheme "default")
