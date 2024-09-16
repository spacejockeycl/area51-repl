(in-package :area51-repl)

(defvar *keymap* nil)

(defvar *keymaps* (make-hash-table)
  "all the keymaps, by name")

(defvar *rl-default-keymap* nil
  "the pointer to readline's default keymap")

(defstruct keymap
  name parent bindings
  rl-keymap)

(defmacro define-keymap (name (&optional parent) &body bindings)
  "create a keymap with bindings as an hash-table and register the keymap."
  (with-gensyms (keymap)
    `(progn
       (let ((,keymap (make-keymap
                       :name ',name
                       :parent ',parent
                       :bindings (make-hash-table :test 'equal))))
         ;; set the bindings hash-table
         ,@(loop :for (key function) :in bindings
                 :collect
                 `(setf (gethash ,key (keymap-bindings ,keymap)) ,function))
         (setf (gethash ',name *keymaps*) ,keymap)))))

(defun find-keymap (name)
  "find a keymap by name"
  (when name
    (check-type name symbol)
    (gethash name *keymaps*)))

(defun get-readline-keymap-by-name (name)
  (if name
      (keymap-rl-keymap (find-keymap name))
      *rl-default-keymap*))

(defun build-readline-keymap (keymap)
  (check-type keymap keymap)
  ;; parent keymap is never nil
  (let* ((parent (find-readline-keymap (keymap-parent keymap)))
         (rl-keymap (rl:copy-keymap parent)))
    (setf (keymap-rl-keymap keymap) rl-keymap)
    (loop :for key :being :the :hash-key :of (keymap-bindings keymap)
            :using (hash-value function)
          :do
             (check-type key (or string character))
             (if (stringp key)
                 (rl:bind-keyseq key function :keymap rl-keymap)
                 (rl:bind-key key function :keymap rl-keymap)))
    rl-keymap))

(defun find-readline-keymap (name)
  (if name
      (let* ((keymap (find-keymap name)))
        (unless keymap
          (error "undefinded keymap \"~A\"" name))
        (or (keymap-rl-keymap keymap)
            (build-readline-keymap keymap)))
      *rl-default-keymap*))


(define-keymap default ()
  ("\\C-l" #'(lambda (count key)
               (declare (ignore count key))
               (flush-screen))))

(defun set-keymap (name)
  (let ((keymap (find-keymap name)))
    (unless (null keymap)
      (setf *keymap* name)
      ;; (warn "set-keymap: not implmented")
      (rl:set-keymap (find-readline-keymap name)))))

(defun init-keymap ()
  (setf *rl-default-keymap* (rl:get-keymap))
  (set-keymap 'default))

