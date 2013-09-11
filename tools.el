;; tools

(defun os-is (name)
  (string-match name system-configuration))

(defmacro define-multiple-keys (mapname &rest difinitions)
  "define key settings of arbitary key-map"
  `(progn
     ,@(mapcar (lambda (d) `(define-key ,mapname (kbd ,(first d)) ,(second d)))
                      difinitions)))

(defmacro require-when-exist (reqlis &rest body)
  (declare (indent 0))
  (let ((lib-name (gensym)))
    `(let ((,lib-name ,(format "%s" (second (second reqlis)))))
       (if (locate-library ,lib-name)
           (progn ,reqlis
                  ,@body)
         (message (format "cannot find `%s' and skip it." ,lib-name))))))

(defmacro add-to-add-hook (hooks &rest body)
  `(progn
     ,@(loop for hook in hooks
                  collect
                       `(add-hook ,hook ,@body))))

(defun split-at (lis num)
  ;(split-at '(1 2 3 4 5) 2) => ((1 2) (3 4 5))
  (labels ((rec (n lis acc)
                (if (or (<= n 0) (null lis))
                    (values (reverse acc) lis)
                  (rec (1- n) (cdr lis) (cons (car lis) acc)))))
    (rec num lis nil)))


(defun group (lis num)
  ;(group '(1 2 3 4 5) 2) =>  ((1 2) (3 4) (5))
  (labels ((rec (lis acc)
                (if (null lis)
                    (reverse acc)
                  (multiple-value-bind (r f) (split-at lis num)
                    (rec f (cons r acc))))))
    (rec lis nil)))
