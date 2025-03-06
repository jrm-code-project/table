;;; -*- Lisp -*-

(in-package "TABLE")

;;; Some basic operations on table-like things.

(defun alist-fold-left (fn init alist)
  (fold-left (lambda (acc entry)
               (funcall fn acc (car entry) (cdr entry)))
             init
             alist))

(defun alist-fold-right (fn alist final)
  (fold-left (lambda (acc entry)
                (funcall fn (car entry) (cdr entry) acc))
              final
              (reverse alist)))

(defun alist-keys   (alist) (mapcar #'car alist))
(defun alist-values (alist) (mapcar #'cdr alist))


(defun hash-table-fold-left (fn init hashtable)
  (with-hash-table-iterator (next hashtable)
    (let iter ((acc init))
      (multiple-value-bind (entry? key value) (next)
        (if entry?
            (iter (funcall fn acc key value))
            acc)))))

(defun hash-table-fold-right (fn hashtable final)
  (with-hash-table-iterator (next hashtable)
    (let iter ((acc final))
      (multiple-value-bind (entry? key value) (next)
        (if entry?
            (iter (funcall fn key value acc))
            acc)))))

(defun plist-fold-left (fn init plist)
  (cond ((consp plist) (if (consp (cdr plist))
                           (plist-fold-left fn (funcall fn init (car plist) (cadr plist))
                                            (cddr plist))
                           (error "Improper plist.")))
        ((null plist) init)
        (t (error "Improper plist."))))

(defun plist-fold-right (fn plist final)
  (plist-fold-left (lambda (acc value key)
                     (funcall fn key value acc))
                   final
                   (reverse plist)))

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




