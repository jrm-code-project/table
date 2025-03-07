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
                 :representation (make-hash-table :test (hash-table-test (representation table)))
                 :metadata (copy-list (metadata table))))

(defmethod table/clear! ((table hash-table))
  (clrhash (representation table))
  table)

(defmethod table/copy ((table hash-table))
  (make-instance 'hash-table
                 :representation (copy-hash-table (representation table))
                 :metadata (copy-list (metadata table))))

(defmethod table/delete ((table hash-table) &rest keys)
  (dolist (key keys table)
    (remhash key (representation table))))

(defmethod table/delete-keys ((table hash-table) key-list)
  (dolist (key key-list table)
    (remhash key (representation table))))

(defmethod table/insert ((table hash-table) key value)
  (let ((copy (copy-hash-table (representation table))))
    (setf (gethash key copy) value)
    (make-instance 'hash-table :representation copy :metadata (copy-list (metadata table)))))

(defmethod table/insert! ((table hash-table) key value)
  (setf (gethash key (representation table)) value)
  table)

(defmethod table/keys ((table hash-table))
  (hash-table-keys (representation table)))

(defmethod table/lookup ((table hash-table) key &optional default)
  (gethash key (representation table) default))

(defmethod table/maximum ((table hash-table))
  (unless (and (representation table)
               (not (zerop (hash-table-count (representation table)))))
    (error "Empty table."))
  (with-hash-table-iterator (next (representation table))
    (let iter ((max-key nil)
               (max-val nil)
               (first   t))
      (multiple-value-bind (entryp key value) (next)
        (if (not entryp)
            (values max-key max-val)
            (if (or first
                    (greater key max-key))
                (iter key value nil)
                (iter max-key max-val nil)))))))

(defmethod table/minimum ((table hash-table))
  (unless (and (representation table)
               (not (zerop (hash-table-count (representation table)))))
    (error "Empty table."))
  (with-hash-table-iterator (next (representation table))
    (let iter ((min-key nil)
               (min-val nil)
               (first   t))
      (multiple-value-bind (entryp key value) (next)
        (format t "~&entryp: ~a, key: ~a, value: ~a~%" entryp key value)
        (force-output t)
        (if (not entryp)
            (values min-key min-val)
            (if (or first
                    (less key min-key))
                (iter key value nil)
                (iter min-key min-val nil)))))))

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

(defmethod table/remove ((table hash-table) &rest keys)
  (let ((copy (copy-hash-table (representation table))))
    (dolist (key keys) (remhash key copy))
    (make-instance 'hash-table :representation copy :metadata (copy-list (metadata table)))))

(defmethod table/remove! ((table hash-table) &rest keys)
  (dolist (key keys)
    (remhash key (representation table))))

(defmethod table/size ((table hash-table))
  (hash-table-count (representation table)))

(defmethod table/split-gt ((table hash-table) pivot)
  (let ((answer-table (make-hash-table :test (hash-table-test (representation table))))
        (predicate    (if (eql (hash-table-test (representation table)) 'equalp)
                          #'greaterp
                          #'greater)))

    (maphash (lambda (k v)
               (when (funcall predicate k pivot)
                 (setf (gethash k answer-table) v)))
             (representation table))

    (make-instance 'hash-table
                   :representation answer-table
                   :metadata '())))

(defmethod table/split-lt ((table hash-table) pivot)
  (let ((answer-table (make-hash-table :test (hash-table-test (representation table))))
        (predicate    (if (eql (hash-table-test (representation table)) 'equalp)
                          #'lessp
                          #'less)))

    (maphash (lambda (k v)
               (when (funcall predicate k pivot)
                 (setf (gethash k answer-table) v)))
             (representation table))

    (make-instance 'hash-table
                   :representation answer-table
                   :metadata '())))

(defmethod table/test ((table hash-table))
  (hash-table-test (representation table)))

(defmethod table/values ((table hash-table))
  (hash-table-values (representation table)))
