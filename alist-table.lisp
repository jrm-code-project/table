;;; -*- Lisp -*-

(in-package "TABLE")

;;; ALIST-TABLE
;; A table implemented as an association list.  The test function is used to compare the keys.
(defmethod make-table ((implementation (eql 'alist)) &rest initargs &key (test #'eql))
  (declare (ignore initargs))
  (make-instance 'alist-table :representation '() :test test))

(defmethod make-singleton-table ((implementation (eql 'alist)) key value &rest initargs &key (test #'eql))
  (declare (ignore initargs))
  (make-instance 'alist-table :representation (list (cons key value)) :test test))

(defun collect-alist-table (key-series value-series)
  (declare (optimizable-series-function))
  (make-instance 'alist-table :representation (collect-alist key-series value-series)))

(defun scan-alist-table (alist-table)
  (declare (optimizable-series-function 2))
  (scan-alist (representation alist-table)))

(defmethod table/copy ((table alist-table))
  (make-instance 'alist-table
                 :representation (mapcar (lambda (entry) (cons (car entry) (cdr entry)))
                                         (representation table))
                 :metadata (copy-list (metadata table))
                 :test (test table)))

(defmethod fold-table (procedure base (table alist-table))
  (fold-left (lambda (base entry) (funcall procedure base (car entry) (cdr entry)))
             base
             (representation table)))

(defmethod table/clear ((table alist-table))
  (make-instance 'alist-table :representation '() :metadata (copy-list (metadata table)) :test (test table)))

(defmethod table/clear! ((table alist-table))
  (setf (representation table) '()))

(defmethod table/delete ((table alist-table) key)
  (setf (representation table) (delete key (representation table) :test (test table) :key #'car)))

(defmethod table/insert ((table alist-table) key value)
  (make-instance 'alist-table
                 :metadata (copy-list (metadata table))
                 :representation (acons key value (remove key (representation table) :test (test table) :key #'car))
                 :test (test table)))

(defmethod table/insert! ((table alist-table) key value)
  (let ((probe (assoc key (representation table) :test (test table))))
    (if probe
        (setf (cdr probe) value)
        (setf (representation table) (acons key value (representation table))))
    value))

(defmethod table/keys ((table alist-table))
  (mapcar #'car (representation table)))

(defmethod table/lookup ((table alist-table) key &optional default)
  (let ((entry (assoc key (representation table) :test (test table))))
    (if entry (cdr entry) default)))

(defmethod (setf table/lookup) (value (table alist-table) key)
  (table/insert! table key value))

(defmethod table/maximum ((table alist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((max-key (caar (representation table)))
             (max-value (cdar (representation table)))
             (rest (cdr (representation table))))
    (if (null rest)
        (values max-key max-value)
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (greater key max-key)
              (iter key value (cdr rest))
              (iter max-key max-value (cdr rest)))))))

(defmethod table/minimum ((table alist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((min-key (caar (representation table)))
             (min-value (cdar (representation table)))
             (rest (cdr (representation table))))
    (if (null rest)
        (values min-key min-value)
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (less key min-key)
              (iter key value (cdr rest))
              (iter min-key min-value (cdr rest)))))))

(defmethod table/pop-maximum ((table alist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((max-key (caar (representation table)))
             (max-value (cdar (representation table)))
             (rest (cdr (representation table))))
    (if (null rest)
        (values max-key max-value (table/remove table max-key))
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (greater key max-key)
              (iter key value (cdr rest))
              (iter max-key max-value (cdr rest)))))))

(defmethod table/pop-maximum! ((table alist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((max-key (caar (representation table)))
             (max-value (cdar (representation table)))
             (rest (cdr (representation table))))
    (if (null rest)
        (progn
          (setf (representation table)
                (delete max-key (representation table) :test (test table) :key #'car))
          (values max-key max-value))
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (greater key max-key)
              (iter key value (cdr rest))
              (iter max-key max-value (cdr rest)))))))

(defmethod table/pop-minimum ((table alist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((min-key (caar (representation table)))
             (min-value (cdar (representation table)))
             (rest (cdr (representation table))))
    (if (null rest)
        (values min-key min-value (table/remove table min-key))
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (less key min-key)
              (iter key value (cdr rest))
              (iter min-key min-value (cdr rest)))))))

(defmethod table/pop-minimum! ((table alist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((min-key (caar (representation table)))
             (min-value (cdar (representation table)))
             (rest (cdr (representation table))))
    (if (null rest)
        (progn
          (setf (representation table)
                (delete min-key (representation table) :test (test table) :key #'car))
          (values min-key min-value))
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (less key min-key)
              (iter key value (cdr rest))
              (iter min-key min-value (cdr rest)))))))

(defmethod table/remove ((table alist-table) key)
  (make-instance 'alist-table
                 :metadata (copy-list (metadata table))
                 :representation (remove key (representation table) :test (test table) :key #'car)
                 :test (test table)))

(defmethod table/remove! ((table alist-table) key)
  (setf (representation table) (delete key (representation table) :test (test table) :key #'car)))

(defmethod table/size ((table alist-table))
  (length (representation table)))

(defmethod table/test ((table alist-table))
  (cond ((eql (test table) #'eql) 'eql)
        ((eql (test table) #'equal) 'equal)
        ((eql (test table) #'equalp) 'equalp)
        (t (error "Unknown test function: ~s" (test table)))))

(defmethod table/values ((table alist-table))
  (mapcar #'cdr (representation table)))
