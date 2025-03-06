;;; -*- Lisp -*-

(in-package "TABLE")

;;; PLIST-TABLE
;; A table implemented as a property list.  Eql is used to compare the keys.

(defun collect-plist-table (key-series value-series)
  (declare (optimizable-series-function))
  (make-instance 'plist-table
                 :representation (collect-plist key-series value-series)))

(defun scan-plist-table (plist-table)
  (declare (optimizable-series-function 2))
  (scan-plist (representation plist-table)))

(defmethod table/copy ((table plist-table))
  (make-instance 'plist-table
                 :representation (copy-list (representation table))
                 :metadata (copy-list (metadata table))))

(defmethod fold-table (procedure base (table plist-table))
  (multiple-value-bind (keys values) (scan-plist (representation table))
    (collect-last
        (collecting-fn 't
                       (lambda () base)
                       (lambda (base key value) (funcall procedure base key value))
                       keys
                       values))))

(defmethod table/clear ((table plist-table))
  (make-instance 'plist-table
                 :representation '()
                 :metadata (copy-list (metadata table))))

(defmethod table/clear! ((table plist-table))
  (setf (representation table) '()))

(defmethod table/delete ((table plist-table) key)
  (setf (representation table) (delete-from-plist (representation table) key)))

(defmethod table/insert ((table plist-table) key value)
  (make-instance 'plist-table
                 :representation (list* key value (remove-from-plist (representation table) key))
                 :metadata (copy-list (metadata table))))

(defmethod table/insert! ((table plist-table) key value)
  (let* ((not-found (cons nil nil))
         (probe (getf (representation table) key not-found)))
    (if (eq probe not-found)
        (progn
          (setf (representation table) (list* key value (representation table)))
          value)
        (setf (cadr probe) value))))

(defmethod table/keys ((table plist-table))
  (do ((keys '() (cons (car plist) keys))
       (plist (representation table) (cddr plist)))
      ((null plist) keys)))

(defmethod table/lookup ((table plist-table) key &optional default)
  (getf (representation table) key default))

(defmethod table/maximum ((table plist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((max-key (car (representation table)))
             (max-value (cadr (representation table)))
             (rest (cddr (representation table))))
    (if (null rest)
        (values max-key max-value)
        (let ((key (car rest))
              (value (cadr rest)))
          (if (greater key max-key)
              (iter key value (cddr rest))
              (iter max-key max-value (cddr rest)))))))

(defmethod table/minimum ((table plist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((min-key (car (representation table)))
             (min-value (cadr (representation table)))
             (rest (cddr (representation table))))
    (if (null rest)
        (values min-key min-value)
        (let ((key (car rest))
              (value (cadr rest)))
          (if (less key min-key)
              (iter key value (cddr rest))
              (iter min-key min-value (cddr rest)))))))

(defmethod table/pop-maximum ((table plist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((max-key (car (representation table)))
             (max-value (cadr (representation table)))
             (rest (cddr (representation table))))
    (if (null rest)
        (values max-key max-value (table/remove table max-key))
        (let ((key (car rest))
              (value (cadr rest)))
          (if (greater key max-key)
              (iter key value (cddr rest))
              (iter max-key max-value (cddr rest)))))))

(defmethod table/pop-maximum! ((table plist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((max-key (car (representation table)))
             (max-value (cadr (representation table)))
             (rest (cddr (representation table))))
    (if (null rest)
        (progn
          (setf (representation table)
                (delete-from-plist (representation table) max-key))
          (values max-key max-value))
        (let ((key (caar rest))
              (value (cdar rest)))
          (if (greater key max-key)
              (iter key value (cdr rest))
              (iter max-key max-value (cdr rest)))))))

(defmethod table/pop-minimum ((table plist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((min-key (car (representation table)))
             (min-value (cadr (representation table)))
             (rest (cddr (representation table))))
    (if (null rest)
        (values min-key min-value (table/remove table min-key))
        (let ((key (car rest))
              (value (cadr rest)))
          (if (less key min-key)
              (iter key value (cddr rest))
              (iter min-key min-value (cddr rest)))))))

(defmethod table/pop-minimum! ((table plist-table))
  (unless (representation table)
    (error "Empty table."))
  (let iter ((min-key   (car (representation table)))
             (min-value (cadr (representation table)))
             (rest      (cddr (representation table))))
    (if (null rest)
        (progn
          (setf (representation table)
                (delete-from-plist (representation table) min-key))
          (values min-key min-value))
        (let ((key (car rest))
              (value (cadr rest)))
          (if (less key min-key)
              (iter key value (cddr rest))
              (iter min-key min-value (cddr rest)))))))

(defmethod table/remove ((table plist-table) key)
  (make-instance 'plist-table
                 :representation (remove-from-plist (representation table) key)
                 :metadata (copy-list (metadata table))))

(defmethod table/remove! ((table plist-table) key)
  (setf (representation table) (delete-from-plist (representation table) key)))

(defmethod table/split-gt ((table plist-table) pivot)
  (labels ((recur (rest)
             (cond ((null rest) '())
                   ((greater (car rest) pivot)
                    (list* (car rest) (cadr rest) (recur (cddr rest))))
                   (t (recur (cddr rest))))))
    (make-instance 'plist-table
                   :representation (recur (representation table)))))

(defmethod table/split-lt ((table plist-table) pivot)
  (labels ((recur (rest)
             (cond ((null rest) '())
                   ((less (car rest) pivot)
                    (list* (car rest) (cadr rest) (recur (cddr rest))))
                   (t (recur (cddr rest))))))
    (make-instance 'plist-table
                   :representation (recur (representation table)))))

(defmethod table/size ((table plist-table))
  (floor (length (representation table)) 2))

(defmethod table/test ((table plist-table))
  'eql)

(defmethod table/values ((table plist-table))
  (do ((vals '() (cons (cadr plist) vals))
       (plist (representation table) (cddr plist)))
      ((null plist) vals)))
