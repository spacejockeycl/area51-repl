(in-package #:cl-repl)

(macroexpand-1
 '(define-color 0 black))
;; => (PROGN (DEFCONSTANT +BLACK+ 0) (EXPORT '+BLACK+))

(macroexpand-1
 '(define-colors
   0 black
   1 white))
;; => (PROGN (DEFINE-COLOR 0 BLACK) (DEFINE-COLOR 1 WHITE))

(color +blue+ "blue")
;; => "[38;5;12mblue[0m"





