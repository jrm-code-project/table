;;; -*- Lisp -*-

(in-package "TABLE")

;;; HASH-TABLE
;; A table implemented as a hash table.  The test function is used to compare the keys.
(defun collect-hash-table (key-series value-series)
  (declare (optimizable-series-function))
  (make-instance 'hash-table :representation (collect-hash key-series value-series)))

(defun scan-hash-table (hash-table)
  (declare (optimizable-series-function 2))
  (scan-hash (representation hash-table)))

(defmethod table/copy ((table hash-table))
  (make-instance 'hash-table
                 :representation (copy-hash-table (representation table))
                 :metadata (copy-list (metadata table))))

(defmethod fold-table (procedure base (table hash-table))
  (multiple-value-bind (keys values) (scan-hash (representation table))
    (collect-last
        (collecting-fn 't
                       (lambda () base)
                       (lambda (base key value) (funcall procedure base key value))
                       keys
                       values))))

(defmethod table/clear ((table hash-table))
  (make-instance 'hash-table
                 :representation (make-hash-table :test (hash-table-test table))
                 :metadata (copy-list (metadata table))))

(defmethod table/clear! ((table hash-table))
  (clrhash (representation table)))

(defmethod table/delete ((table hash-table) key)
  (remhash key (representation table)))

(defmethod table/insert ((table hash-table) key value)
  (let ((copy (copy-hash-table (representation table))))
    (setf (gethash key copy) value)
    (make-instance 'hash-table :representation copy :metadata (copy-list (metadata table)))))

(defmethod table/insert! ((table hash-table) key value)
  (setf (gethash key (representation table)) value))

(defmethod table/keys ((table hash-table))
  (hash-table-keys (representation table)))

(defmethod table/lookup ((table hash-table) key &optional default)
  (gethash key (representation table) default))

(defmethod (setf table/lookup) (value (table hash-table) key)
  (table/insert! table key value))

(defmethod table/maximum ((table hash-table))
  (unless (representation table)
    (error "Empty table."))
  (let ((alist (hash-table-alist (representation table))))
    (let iter ((min-key (caar alist))
               (min-value (cdar alist))
               (rest (cdr alist)))
      (if (null rest)
          (values min-key min-value)
          (let ((key (caar rest))
                (value (cdar rest)))
            (if (greater key min-key)
                (iter key value (cdr rest))
                (iter min-key min-value (cdr rest))))))))

(defmethod table/minimum ((table hash-table))
  (unless (representation table)
    (error "Empty table."))
  (let ((alist (hash-table-alist (representation table))))
    (let iter ((min-key (caar alist))
               (min-value (cdar alist))
               (rest (cdr alist)))
      (if (null rest)
          (values min-key min-value)
          (let ((key (caar rest))
                (value (cdar rest)))
            (if (less key min-key)
                (iter key value (cdr rest))
                (iter min-key min-value (cdr rest))))))))

(defmethod table/pop-maximum ((table hash-table))
  (unless (representation table)
    (error "Empty table."))
  (multiple-value-bind (max-key max-value) (table/maximum table)
    (values max-key max-value (table/remove table max-key))))

(defmethod table/pop-maximum! ((table hash-table))
  (unless (representation table)
    (error "Empty table."))
  (multiple-value-bind (max-key max-value) (table/maximum table)
    (table/delete table max-key)
    (values max-key max-value)))

(defmethod table/pop-minimum ((table hash-table))
  (unless (representation table)
    (error "Empty table."))
  (multiple-value-bind (min-key min-value) (table/minimum table)
    (values min-key min-value (table/remove table min-key))))

(defmethod table/pop-minimum! ((table hash-table))
  (unless (representation table)
    (error "Empty table."))
  (multiple-value-bind (min-key min-value) (table/minimum table)
    (table/delete table min-key)
    (values min-key min-value)))

(defmethod table/remove ((table hash-table) key)
  (let ((copy (copy-hash-table (representation table))))
    (remhash key copy)
    (make-instance 'hash-table :representation copy :metadata (copy-list (metadata table)))))

(defmethod table/remove! ((table hash-table) key)
  (remhash key (representation table)))

(defmethod table/size ((table hash-table))
  (hash-table-count (representation table)))

(defmethod table/test ((table hash-table))
  (hash-table-test (representation table)))

(defmethod table/values ((table hash-table))
  (hash-table-values (representation table)))
