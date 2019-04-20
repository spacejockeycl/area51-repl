
(defsystem cl-repl
  :version "0.6.4"
  :author "TANI Kojiro, Francis St-Amour"
  :license "GPLv3"
  :depends-on (#:alexandria
               #:uiop
               #:unix-opts
               #:conium
               #:cl-ppcre
               #:cl-readline)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "util")
                             (:file "color")
                             ;; (:file "color-constants")
                             (:file "color-scheme")
                             (:file "highlight")
                             (:file "keymap")
                             (:file "pager")
                             (:file "command")
                             (:file "commands")
                             (:file "shell")
                             (:file "completer")
                             (:file "debugger")
                             (:file "inspector")
                             (:file "input")
                             (:file "repl")
                             (:file "main"))))
  :description "A full-featured repl implementation."
  :long-description "A full-featured repl implementation.")

