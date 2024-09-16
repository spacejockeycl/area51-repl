(in-package :cl-user)

(defpackage :area51-repl
  (:use :cl
    #:alexandria
    #:magic-ed)
  (:export
    #:start

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
    #:*output-indicator-function*))