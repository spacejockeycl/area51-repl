(in-package :cl-user)

(defpackage :cl-repl
  (:use :cl #:alexandria)
  (:export #:main

           ;; Command
           #:define-command
           #:message

           ;; Coloring
           #:define-color-scheme
           #:color-scheme
           #:disable-syntax

           ;; Paging settings
           #:*pager-command*
           #:*pager-minimum-line-count*
           #:*pager-flush-screen*

           ;; Prompt settings
           #:*default-prompt-function*
           #:*debugger-prompt-function*
           #:*output-indicator-function*

           ;; Why is this exported?
           #:*debugger-level*

           ;; Buffering
           #:*repl-flush-screen*
           #:*debugger-flush-screen*
           #:*inspector-flush-screen*))

(defpackage :repl-user
  (:use :cl :cl-repl))
