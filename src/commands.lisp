(in-package #:area51-repl)

(define-command cd (&optional (destination (uiop:getenv "HOME")))
  "Change directory."
  (handler-case
      (progn
        (setf destination (truename destination))
        (uiop:chdir destination)
        (setf *default-pathname-defaults* destination)
        (format t "~s" destination))
    (error () (message "No such directory."))))

(define-command clear () 
  "Clear the screen."
  (flush-screen))

(define-command dir ()
  "Current directory."
  (format t "~s~%" *default-pathname-defaults*))

(define-command doc (target)
  "Describe the object."
  (handler-case
      (let ((s (make-array '(0)
                           :element-type 'base-char
                           :fill-pointer 0
                           :adjustable t)))
        (with-output-to-string (sb s)
          (describe (read-from-string target) sb))
        (invoke-pager s)
        "nil")
    (error () (message "No description given on `~a.`" target))))

(define-command edit (file)
  "Edit file with $EDITOR."
  (magic-ed:magic-ed file))

(define-command help (&rest args)
  "List REPL commands."
  (declare (ignore args))
  (let* ((commands *commands*)
         (keys (sort (hash-table-keys commands) #'string<))
         (max-name-length (loop :for name :being :the :hash-key :of commands
                                :maximize (length name))))
    (loop :for name :in keys
          :for command = (gethash name commands)
          :do (format t ".~v,,a~a~%"
                      (min 16 (+ 2 max-name-length))
                      name
                      (first-line (command-description command))))))
