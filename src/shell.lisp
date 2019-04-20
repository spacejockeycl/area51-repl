(in-package :cl-repl)

(defun shell-command-p (&optional input)
  "Is the given input a shell command?"
  (starts-with #\! input))

(defun run-shell-command (cmd)
  (uiop:run-program (subseq cmd 1)
                    :output *standard-output*
                    :ignore-error-status t))


(defun probe-command (name)
  "Check if a shell command is available"
  (handler-case
      (progn
        (uiop:run-program
         (format nil "command -v ~a" name))
        t)
    (error () nil)))

(defun wrap-shell-command (name args)
  "Call a shell command interactively and return its exit status."
  (write-to-string
   (multiple-value-bind
         (output error-output status)
       (uiop:run-program
        (format nil "~a \"~{~a ~}\"" name
                (mapcar #'(lambda (a)
                            (ppcre:regex-replace-all "\"" a "\\\""))
                        args))
        :output :interactive
        :input :interactive
        :error-output :interactive
        :ignore-error-status t)
     (declare (ignore output error-output))
     status)))

(defun maybe-wrap-shell-command (name flag)
  (when (probe-command name)
    (define-command name (&rest args)
        (format nil "Execute line with ~a." name)
      (wrap-shell-command (format nil "~a ~a" name flag) args))))

(maybe-wrap-shell-command "python" "-c")
(maybe-wrap-shell-command "ruby" "-e")
(maybe-wrap-shell-command "perl" "-e")

