;;;; util.lisp

(in-package #:deluge)

(defun dump-hash-table (ht &optional (depth 0))
  "dumps a hash table that is explorable in org-mode"
  (loop 
     for k being the hash-keys in ht
     using (hash-value v)
     do 
       (dotimes (i (1+ depth)) (format t "*"))
       (format t " ~a:~%" k)
       (if (not (hash-table-p v))
           (format t "~a~%" v)
           (dump-hash-table v (1+ depth)))))

(defun readable-bytes (bytes)
  "transforms number of bytes into a human-readable form, e.g. '3.14 GB'"
  ;; Well, I'm not torrenting anything larger than 1024 exabytes...
  ;; At least not yet, anyway...
  (let ((magnitude '((0 . "B")  (1 .  "KB") (2 . "MB") (3 . "GB")
                     (4 . "TB") (5 . "PB") (6 . "XB"))))
    (loop 
       for (e . sym) in magnitude
       for n = (/ bytes (expt 1023 e))
       do (when (< n 1023) 
            (return 
              (with-output-to-string (str)
                (format str "~$ ~a" (float n) sym)))))))

(defmacro gethashes (keys hash-table &optional default)
  (let ((expansion hash-table))
    (dolist (key keys)
      (setf expansion (list 'gethash key expansion default)))
    expansion))


(defmacro with-hash-table-values ((hash-table) &body body)
  "based on the body, bind the necessary values from the hash-table"
  (let ((ht (gensym)))
    `(let* ((,ht ,hash-table)
            ,@(mapcar #'(lambda (v)
                          `(,v (gethash ,(to-key v) ,ht nil)))
                      (vars-in body)))
       ,@body)))

(defun to-key (sym)
  "transforms a symbol of the form >foo_bar to a string of the form \"foo_bar\""
  (substitute #\_ #\- (string-downcase (subseq (symbol-name sym) 1))))

(defun vars-in (expr)
  "modified from Paul Graham's version in _On Lisp_"
  (labels ((var? (x)
             (and (symbolp x)
                  (eq (char (symbol-name x) 0) #\>))))
    (if (atom expr)
        (if (var? expr) (list expr))
        (union (vars-in (car expr))
               (vars-in (cdr expr))))))
