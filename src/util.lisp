(in-package :cl-repl)

(defun split-lines (text)
  (ppcre:split "\\n+" text))

(defun split-space (text)
  (ppcre:split "\\s+" text))

;; TODO use string-case
(defmacro string-case (str &rest forms)
  (let ((test (gensym)))
    `(let ((,test ,str))
       (cond
         ,@(loop :for (s  f) :in forms
                 :if (stringp s) :collect `((string= ,test ,s) ,f)
                   :else :if (string= s 'otherwise) :collect `(t ,f)
                           :else :collect `((eql ,test ,s) ,f))))))

(defmacro with-cursor-hidden (&body body)
  `(unwind-protect
     (progn
       (format t "~c[?25l" #\esc)
       (finish-output)
       ,@body)
     (progn
       (format t "~c[?25h" #\esc)
       (finish-output))))

(defun flush-screen ()
  (with-cursor-hidden
    (format t "~c[2J~@*~c[;H" #\esc)
    (finish-output)))

