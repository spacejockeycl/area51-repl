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

;; RENDU
(find-readline-keymap nil) ;; should return *rl-default-keymap*
(find-readline-keymap 'default) ;; should get-or-create

(find-keymap 'default)
(build-readline-keymap (find-keymap 'default)) ;; should build it even if it exists

;; Free and  remove readline-keymap
;; (rl:set-keymap *rl-default-keymap*)
;; (rl:free-keymap (keymap-rl-keymap (find-keymap 'default)))
;; (setf (keymap-rl-keymap (find-keymap 'default)) nil)



