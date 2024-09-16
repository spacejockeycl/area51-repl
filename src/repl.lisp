(in-package :area51-repl)

(unless (boundp '+exit+)
  (defconstant +exit+ '#:exit))

(defvar *default-prompt-function*
  #'(lambda ()
      (format nil "~a> " (current-package))))

(defvar *output-indicator-function*
  #'(lambda () "[OUT]: "))

(defun current-package ()
  "Return the current-package-name or its first nickname if it exists."
  (or (first (package-nicknames *package*)) ;; TODO Choose the shortest nickname.
      (package-name *package*)))

(defun prompt (&key (multiline-p nil))
  "Return a prompt string with color appropriate for the current context."
  (let ((prompt-string (funcall *default-prompt-function*)))
    (color *default-prompt-color*
      (if multiline-p
        (format nil "~V@{.~} " (1- (length prompt-string)) '#:dummy)
        prompt-string))))

(defun line-continue-p (string)
  "Returns true when the string is an incomplete expression.
Useful for multiline entries."
  (and 
    (stringp string)
    (let ((*read-eval* nil))
      (handler-case (progn (read-from-string string) nil)
        (end-of-file () t)))))

(defun preprocess-input (input)
  "handles !, % and EOF"
  (cond
    ;; TODO replace split-space by something that handles quotes
    ((command-p input) (apply #'invoke-command (split-space input)) nil)
    ;; TODO remove this
    ((shell-command-p input) (run-shell-command input) nil)
    ((string= "" input) nil)
    ((null input) (confirm-exit))
    (t input)))

(defun read-input1 (&key (multiline-p nil))
  "readline"
  (unless multiline-p
    (fresh-line))
  (finish-output)
  (rl:readline :prompt (prompt :multiline-p multiline-p) :add-history t))

(defun read-input ()
  "read form"
  (loop
    :with input = (preprocess-input (read-input1))
    :while (line-continue-p input)
    :for more-input = (read-input1 :multiline-p t)
    :do
      (unless more-input
        (terpri))
      (setf input (format nil "~a~%~a" input more-input))
    :finally (return-from read-input input)))

(defun confirm-exit ()
  (finish-output)
  (when (y-or-n-p "Do you really want to exit?")
    +exit+))

(defun print-result (values)
  (format t "~&~a~{~s ~}~%"
    (color *output-indicator-color* (funcall *output-indicator-function*)) values)
  (finish-output) t)

(defun eval-print (form)
  (let ((values (multiple-value-list (eval form))))
    (setq +++ ++
          /// //
          *** (car ///)
          ++ +
          // /
          ** (car //)
          + form
          / values
          * (car /))
    (print-result values)))

(defmacro with-extra-restarts (form &body restarts)
  `(restart-case ,form
      (*abort () :report "Reduce debugger level." t)
      (*exit () :report "Exit Area51-REPL." (throw 'exit nil))
      ,@restarts))

(defun read-eval-print ()
  (with-extra-restarts
      (let ((input (read-input)))
        (cond
          ((member input (list nil +exit+)) input)
          ((stringp input)
            (eval-print (setq - (read-from-string input))))))
    (*retry () :report "Try evaluating again."
            (with-extra-restarts (eval-print -)))))

(defun repl ()
  (catch 'exit
    (loop :when (not (eq *keymap* 'default))
            :do (set-keymap 'default)
          :until (eq +exit+ (read-eval-print)))))

