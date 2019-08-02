(in-package :cl-repl)

(defun get-package-for-search (text)
  (let ((position))
    (cond
      ((setf position (search "::" text))
       (list  (subseq text  (+ position 2)) (subseq text 0 position) nil))
      ((setf position (position #\: text))
       (if (zerop position)
         (list text nil t)
         (list (subseq text (1+ position)) (subseq text 0 position) t)))
      (t (list text nil  t)))))

(defun list-external-symbols (symbol-name package-name)
  (loop :for symbol :being :the :external-symbols :of (find-package package-name)
        :collect (format nil "~(~a:~a~)" package-name symbol)))

(defun list-internal-symbols (symbol-name package-name)
  (loop :for symbol :being :the :symbols :of (find-package package-name)
        :collect (format nil "~(~a::~a~)" package-name symbol)))

(defun list-symbols-and-packages (symbol-name)
  (concatenate 'list
    (loop :for package :in (list-all-packages)
          :append (loop :for name :in (package-nicknames package)
                        :collect (format nil "~(~a:~)" name))
          :collect (format nil "~(~a:~)" (package-name package)))
    (loop :for symbol :being :the :symbols :of *package*
          :collect (string-downcase symbol))
    (loop :for kw :being :the :symbols :of (find-package "KEYWORD")
          :collect (format nil ":~(~a~)" kw))))

(defun select-completions (text items)
  (let ((prefix-matches (loop :for item :in items
                              :when (starts-with-subseq text item)
                                :collect item)))
    (if (null (cdr prefix-matches))
        ;; There's 1 or 0 match
        (progn
          (setf rl:*completion-append-character*
                (if (ends-with #\: (car prefix-matches)) #\nul #\space))
          prefix-matches)
        (cons
         (subseq (first prefix-matches) 0
                 (loop :for item :in (rest prefix-matches)
                       :minimize (or (mismatch (first prefix-matches) item) (length item))))
         prefix-matches))))

(defun complete-symbol (text)
  (destructuring-bind (symbol-name package-name external-p)
      (get-package-for-search (string-upcase text))
    (if (and package-name (not (find-package package-name)))
        nil
        (select-completions
         (string-downcase text)
         (cond
           ((zerop (length package-name)) (list-symbols-and-packages symbol-name))
           (external-p (list-external-symbols symbol-name package-name))
           (t (list-internal-symbols symbol-name package-name)))))))

(defun complete-command (text)
  (select-completions
   (string-downcase text)
   (loop :for name :in (hash-table-keys *commands*)
         :collect (concatenate 'string "%" name))))

(defun %complete (text start end)
  (declare (ignore start end))
  (unless (zerop (length text))
    (or (complete-symbol text)
        (complete-command text))))

;; This function is used by readline, if it is updated, you need to call
;; rl:register-funciton again. To prevent that, we call another function
;; that is the actual implementation
(defun complete (text start end)
  (%complete text start end))

