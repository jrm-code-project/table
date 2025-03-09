;;; -*- Lisp -*-

(in-package "TABLE")

(defun alist? (thing)
  (and (consp thing)
       (every #'consp thing)))

(defun plist? (thing)
  (or (null thing)
      (and (consp thing)
           (atom (car thing))
           (consp (cdr thing))
           (plist? (cddr thing)))))

;;; Some basic operations on table-like things.

(defun alist-keys   (alist) (mapcar #'car alist))
(defun alist-values (alist) (mapcar #'cdr alist))

(defun plist-maximum (plist)
  (plist-fold-left (lambda (max key value)
                     (declare (ignore value))
                     (if (greater key max) key max))
                   (car plist)
                   (cddr plist)))

(defun plist-minimum (plist)
  (plist-fold-left (lambda (min key value)
                     (declare (ignore value))
                     (if (less key min) key min))
                   (car plist)
                   (cddr plist)))

(defun plist-pop-maximum (plist)
  (let ((min (plist-maximum plist)))
    (values min (getf plist min) (remove-from-plist plist min))))

(defun plist-pop-minimum (plist)
  (let ((min (plist-minimum plist)))
    (values min (getf plist min) (remove-from-plist plist min))))

(defun plist-pop-maximum! (plist)
  (let ((min (plist-minimum plist)))
    (values min (getf plist min) (delete-from-plist plist min))))

(defun plist-pop-minimum! (plist)
  (let ((min (plist-minimum plist)))
    (values min (getf plist min) (delete-from-plist plist min))))

(defun plist-keys   (plist)
  (plist-fold-right
   (lambda (key value acc) (declare (ignore value)) (cons key acc))
   plist
   nil))

(defun plist-values (plist)
  (plist-fold-right
   (lambda (key value acc) (declare (ignore key)) (cons value acc))
   plist
   nil))

(defun symbol-plist-pop-maximum (symbol)
  (let ((plist (symbol-plist symbol))
        (sym* (gensym (concatenate 'string (symbol-name symbol) "-"))))
    (multiple-value-bind (key value plist*) (plist-pop-maximum plist)
      (setf (symbol-plist sym*) plist*)
      (values key value sym*))))

(defun symbol-plist-pop-minimum (symbol)
  (let ((plist (symbol-plist symbol))
        (sym* (gensym (concatenate 'string (symbol-name symbol) "-"))))
    (multiple-value-bind (key value plist*) (plist-pop-minimum plist)
      (setf (symbol-plist sym*) plist*)
      (values key value sym*))))

(defun symbol-plist-pop-maximum! (symbol)
  (let ((plist (symbol-plist symbol)))
    (multiple-value-bind (key value plist*) (plist-pop-maximum! plist)
      (setf (symbol-plist symbol) plist*)
      (values key value))))

(defun symbol-plist-pop-minimum! (symbol)
  (let ((plist (symbol-plist symbol)))
    (multiple-value-bind (key value plist*) (plist-pop-minimum! plist)
      (setf (symbol-plist symbol) plist*)
      (values key value))))

