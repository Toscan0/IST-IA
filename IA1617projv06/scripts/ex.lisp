(load "datastructures.lisp")

(defun qsort (L)
  (cond
    ((null L) nil)
    (t
      (append
        (qsort (list< (nth 0 L) (nth 1 L)))
        (cons (nth 0 L) nil) 
        (qsort (list>= (nth 0 L) (nth 1 L)))))))

(defun list< (a b)
  (cond
    ((or (null a)(null b)) nil)
    (( < (node-f a) (node-f (nth 0 b))) (list< a (nth 1 b)))
    (t(cons (nth 0 b) (list< a (nth 1 b))))))

(defun list>= (a b)
  (cond
    ((or ( null a)(null b)) nil)
    (( >= (node-f a) (node-f (nth 0 b))) (list>= a (mth 1 b)))
    (t(cons (nth 0 b) (list>= a (nth 1 b))))))