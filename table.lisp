;;; -*- Lisp -*-

(in-package "TABLE")

(defmacro do-keys ((key table &optional (return-value nil)) &body body)
  (let ((discard (gensym))
        (value (gensym)))
    `(progn (fold-table (lambda (,discard ,key ,value) (declare (ignore ,discard ,value)) ,@body) nil ,table)
            ,return-value)))

(defmacro do-table ((key value table &optional (return-value nil)) &body body)
  (let ((discard (gensym)))
    `(progn (fold-table (lambda (,discard ,key ,value) (declare (ignore ,discard)) ,@body) nil ,table)
            ,return-value)))

(defmacro do-values ((value table &optional (return-value nil)) &body body)
  (let ((discard (gensym))
        (key (gensym)))
    `(progn (fold-table (lambda (,discard ,key ,value) (declare (ignore ,discard ,key)) ,@body) nil ,table)
            ,return-value)))

(defun table/empty? (table)
  (zerop (table/size table)))

(defun table/equal? (left right &optional (test #'eql))
  (and (table/subset?  left right test)
       (table/subset? right  left test)))

;;; Coercions
(defmethod table->alist ((table hash-table))   (hash-table-alist (representation table)))

(defmethod table->alist ((table plist-table))  (plist-alist (representation table)))

(defmethod table->alist ((table wttree-table)) (node-alist (representation table)))

(defmethod table->hashtable ((table alist-table)) (alist-hash-table (representation table)))

(defmethod table->hashtable ((table plist-table)) (plist-hash-table (representation table)))

(defmethod table->hashtable ((table wttree-table)) (node-hash-table (representation table) :test (test table)))

(defmethod table->plist ((table alist-table)) (alist-plist (representation table)))

(defmethod table->plist ((table hash-table)) (hash-table-plist (representation table)))

(defmethod table->plist ((table wttree-table)) (node-plist (representation table)))

(defmethod table->node ((table alist-table)) (hash-table-alist (representation table)))

(defmethod table->node ((table hash-table)) (hash-table-node (representation table)))

(defmethod table->node ((table plist-table)) (plist-node (representation table)))

(defmethod update-instance-for-different-class :after ((previous alist-table) (current hash-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (alist-hash-table (representation previous))))

(defmethod update-instance-for-different-class :after ((previous alist-table) (current plist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (alist-plist (representation previous))))

(defmethod update-instance-for-different-class :after ((previous alist-table) (current wttree-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (alist-node (representation previous))))

(defmethod update-instance-for-different-class :after ((previous hash-table) (current alist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (hash-table-alist (representation previous))))

(defmethod update-instance-for-different-class :after ((previous hash-table) (current plist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (hash-table-plist (representation previous))))

(defmethod update-instance-for-different-class :after ((previous hash-table) (current wttree-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (hash-table-node (representation previous))))

(defmethod update-instance-for-different-class :after ((previous plist-table) (current alist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (plist-alist (representation previous))))

(defmethod update-instance-for-different-class :after ((previous plist-table) (current hash-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (plist-hash-table (representation previous))))

(defmethod update-instance-for-different-class :after ((previous plist-table) (current wttree-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (plist-node (representation previous))))

(defmethod update-instance-for-different-class :after ((previous wttree-table) (current alist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (node-alist (representation previous))))

(defmethod update-instance-for-different-class :after ((previous wttree-table) (current hash-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (node-hash-table (representation previous) :test (test current))))

(defmethod update-instance-for-different-class :after ((previous wttree-table) (current plist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current) (node-plist (representation previous))))

