(in-package :cl-repl)

(defun get-package-for-search (text)
  (let ((pos))
    (cond
      ((setf pos (search "::" text))
       (list  (subseq text  (+ pos 2)) (subseq text 0 pos) nil))
      ((setf pos (position #\: text))
       (if (zerop pos)
         (list text nil t)
         (list (subseq text (1+ pos)) (subseq text 0 pos) t)))
      (t (list text nil  t)))))

(defun list-external-symbols (sym-name pkg-name)
  (loop :for sym :being :the :external-symbols :of (find-package pkg-name)
        :collect (format nil "~(~a:~a~)" pkg-name sym)))

(defun list-internal-symbols (sym-name pkg-name)
  (loop :for sym :being :the :symbols :of (find-package pkg-name)
        :collect (format nil "~(~a::~a~)" pkg-name sym)))

(defun list-symbols-and-packages (sym-name)
  (concatenate 'list
    (loop :for pkg :in (list-all-packages)
          :append (loop :for name :in (package-nicknames pkg)
                        :collect (format nil "~(~a:~)" name))
          :collect (format nil "~(~a:~)" (package-name pkg)))
    (loop :for sym :being :the :symbols :of *package*
          :collect (string-downcase sym))
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
  (destructuring-bind (sym-name pkg-name external-p)
      (get-package-for-search (string-upcase text))
    (if (and pkg-name (not (find-package pkg-name)))
        nil
        (select-completions
         (string-downcase text)
         (cond
           ((zerop (length pkg-name)) (list-symbols-and-packages sym-name))
           (external-p (list-external-symbols sym-name pkg-name))
           (t (list-internal-symbols sym-name pkg-name)))))))

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

