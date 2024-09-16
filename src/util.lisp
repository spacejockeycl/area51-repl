(in-package :area51-repl)

(defun split-lines (text)
  (ppcre:split "\\n+" text))

(defun first-line (string)
  (first (split-lines string)))

(defun split-space (text)
  (ppcre:split "\\s+" text))

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

