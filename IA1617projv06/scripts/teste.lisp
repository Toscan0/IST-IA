(load "datastructures.lisp")

(defun insert (item lst &optional (key #'<))
  (if (null lst)
    (list item)
    (if (funcall key (node-f item) (node-f (car lst))
          (cons item lst) 
          (cons (car lst) (insert item (cdr lst) key)))))

(defun insertion-sort (lst &optional (key #'<))
  (if (null lst)
    lst
(insert (car lst) (insertion-sort (cdr lst) key) key)))