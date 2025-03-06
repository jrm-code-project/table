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
(defmethod update-instance-for-different-class :after ((previous alist-table) (current hash-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-alist (representation previous))
          (collect-hash keys values))))

(defmethod update-instance-for-different-class :after ((previous alist-table) (current plist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-alist (representation previous))
          (collect-plist keys values))))

(defmethod update-instance-for-different-class :after ((previous alist-table) (current wttree-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-alist (representation previous))
          (collect-node keys values #'less))))

(defmethod update-instance-for-different-class :after ((previous hash-table) (current alist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-hash (representation previous))
          (collect-alist keys values))))

(defmethod update-instance-for-different-class :after ((previous hash-table) (current plist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-hash (representation previous))
          (collect-plist keys values))))

(defmethod update-instance-for-different-class :after ((previous hash-table) (current wttree-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-hash (representation previous))
          (collect-node keys values (if (eql (hash-table-test (representation previous)) 'equalp)
                                        #'lessp
                                        #'less)))))

(defmethod update-instance-for-different-class :after ((previous plist-table) (current alist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-plist (representation previous))
          (collect-alist keys values))))

(defmethod update-instance-for-different-class :after ((previous plist-table) (current hash-table) &rest initargs)
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-plist (representation previous))
          (collect-hash keys values :test (getf initargs :test 'eql)))))

(defmethod update-instance-for-different-class :after ((previous plist-table) (current wttree-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-plist (representation previous))
          (collect-node keys values))))

(defmethod update-instance-for-different-class :after ((previous wttree-table) (current alist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-node (representation previous))
          (collect-alist keys values))))

(defmethod update-instance-for-different-class :after ((previous wttree-table) (current hash-table) &rest initargs)
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-node (representation previous))
          (collect-hash keys values :test (or (getf initargs :test)
                                              (if (member (test previous) (list #'lessp 'lessp))
                                                  'equalp
                                                  'equal))))))

(defmethod update-instance-for-different-class :after ((previous wttree-table) (current plist-table) &rest initargs)
  (declare (ignore initargs))
  (setf (representation current)
        (multiple-value-bind (keys values) (scan-node (representation previous))
          (collect-plist keys values))))

(defmethod table->alist ((table hash-table))
  (multiple-value-bind (keys values) (scan-hash-table table)
    (collect-alist keys values)))

(defmethod table->alist ((table plist-table))
  (multiple-value-bind (keys values) (scan-plist-table table)
    (collect-alist keys values)))

(defmethod table->alist ((table wttree-table))
  (multiple-value-bind (keys values) (scan-wttree-table table)
    (collect-alist keys values)))

(defmethod table->hashtable ((table alist-table))
  (multiple-value-bind (keys values) (scan-alist-table table)
    (collect-hash keys values)))

(defmethod table->hashtable ((table plist-table))
  (multiple-value-bind (keys values) (scan-plist-table table)
    (collect-hash keys values)))

(defmethod table->hashtable ((table wttree-table))
  (multiple-value-bind (keys values) (scan-wttree-table table)
    (collect-hash keys values :test (if (member (test table) (list #'lessp 'lessp))
                                        'equalp
                                        'equal))))

(defmethod table->plist ((table alist-table))
  (multiple-value-bind (keys values) (scan-alist-table table)
    (collect-plist keys values)))

(defmethod table->plist ((table hash-table))
  (multiple-value-bind (keys values) (scan-plist-table table)
    (collect-hash keys values)))

(defmethod table->plist ((table wttree-table))
  (multiple-value-bind (keys values) (scan-wttree-table table)
    (collect-alist keys values)))

(defmethod table->node ((table alist-table))
  (multiple-value-bind (keys values) (scan-alist-table table)
    (collect-node keys values)))

(defmethod table->node ((table hash-table))
  (multiple-value-bind (keys values) (scan-hash-table table)
    (collect-node keys values (if (eql (hash-table-test table) 'equalp)
                                  #'lessp
                                  #'less))))

(defmethod table->node ((table plist-table))
  (multiple-value-bind (keys values) (scan-plist-table table)
    (collect-node keys values)))
