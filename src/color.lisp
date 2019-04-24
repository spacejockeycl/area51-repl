(in-package #:cl-repl)

(defun color (color string)
  "Wrap string in escape code for the given color, or return the string unchanged if color is nil."
  (if (null color)
      string
      (progn
        (check-type color (integer 0 255))
        (format nil "~c[38;5;~am~a~c[0m" #\ESC color string #\ESC))))

(defmacro define-color (number name)
  (let ((symbol (symbolicate #\+ name #\+)))
    `(progn
       (defconstant ,symbol ,number)
       (export ',symbol))))

(defmacro define-colors (&body color-definition-list)
  `(progn
     ,@(loop for (number name) on color-definition-list by #'cddr
             collect `(define-color ,number ,name))))

