;;;; tree.lisp

(in-package #:deluge-client)

(defstruct node val left right)

(defun bst-insert (bst val lt)
  (if (not (node-val bst))
      (setf (node-val bst) val)
      (if (funcall lt val (node-val bst))
          (if (null (node-left bst))
              (setf (node-left bst) (make-node :val val))
              (bst-insert (node-left bst) val lt))
          (if (null (node-right bst))
              (setf (node-right bst) (make-node :val val))
              (bst-insert (node-right bst) val lt)))))

(defun bst-traverse (bst &optional (fn #'print))
  (when (and bst (node-val bst))
    (bst-traverse (node-left bst) fn)
    (funcall fn (node-val bst))
    (bst-traverse (node-right bst) fn)))
