;;; -*- Lisp -*-

(in-package "TABLE")

(defun collect-wttree-table (key-series value-series &optional (test #'less))
  (declare (optimizable-series-function))
  (make-instance 'wttree-table :representation (collect-node key-series value-series test)))

(defun scan-wttree-table (table)
  (declare (optimizable-series-function 2))
  (scan-node (representation table)))

(defmethod table/copy ((table wttree-table))
  (make-instance 'wttree-table :metadata (copy-list (metadata table))
                               :representation (representation table)
                               :test (test table)))

(defmethod fold-table (procedure initial (table wttree-table))
  (node/inorder-fold procedure initial (representation table)))

(defmethod table/clear ((table wttree-table))
  (make-instance 'wttree-table :metadata (copy-list (metadata table)) :representation nil :test (test table)))

(defmethod table/clear! ((table wttree-table))
  (setf (representation table) nil))

(defmethod table/copy ((table wttree-table))
  (make-instance 'wttree-table :metadata (copy-list (metadata table))
                               :representation (node/copy (representation table))
                               :test (test table)))

(defmethod table/delete ((table wttree-table) key &rest keys)
  (setf (representation table)
        (fold-left (lambda (node key)
                     (node/remove (test table) node key))
                   (representation table)
                   (cons key keys))))

(defmethod table/delete-keys ((table wttree-table) key-list)
  (setf (representation table)
        (fold-left (lambda (node key)
                     (node/remove (test table) node key))
                   (representation table)
                   key-list)))

(defmethod table/insert ((table wttree-table) key value)
  (make-instance 'wttree-table
                 :metadata (copy-list (metadata table))
                 :representation (node/add (test table) (representation table) key value)
                 :test (test table)))

(defmethod table/insert! ((table wttree-table) key value)
  (setf (representation table) (node/add (test table) (representation table) key value))
  value)

(defmethod table/keys ((table wttree-table))
  (node/keys (representation table)))

(defmethod table/lookup ((table wttree-table) key &optional default)
  (let ((node (node/find (test table) (representation table) key)))
    (if node (node/v node) default)))

(defmethod table/maximum ((table wttree-table))
  (let ((node (node/maximum (representation table))))
    (values (node/k node) (node/v node))))

(defmethod table/minimum ((table wttree-table))
  (let ((node (node/minimum (representation table))))
    (values (node/k node) (node/v node))))

(defmethod table/pop-maximum ((table wttree-table))
  (multiple-value-bind (key value) (table/maximum table)
    (values key value (make-instance 'wttree-table
                                     :representation (node/remove-maximum (representation table))
                                     :test (test table)))))

(defmethod table/pop-maximum! ((table wttree-table))
  (multiple-value-bind (key value) (table/maximum table)
    (setf (representation table) (node/remove-maximum (representation table)))
    (values key value)))

(defmethod table/pop-minimum ((table wttree-table))
  (multiple-value-bind (key value) (table/minimum table)
    (values key value (make-instance 'wttree-table
                                     :representation (node/remove-minimum (representation table))
                                     :test (test table)))))

(defmethod table/pop-minimum! ((table wttree-table))
  (multiple-value-bind (key value) (table/minimum table)
    (setf (representation table) (node/remove-minimum (representation table)))
    (values key value)))

(defmethod table/remove ((table wttree-table) key)
  (make-instance 'wttree-table
                 :metadata (copy-list (metadata table))
                 :representation (node/remove (test table) (representation table) key)
                 :test (test table)))

(defmethod table/remove! ((table wttree-table) key)
  (setf (representation table) (node/remove (test table) (representation table) key)))

(defmethod table/size ((table wttree-table))
  (node/size (representation table)))

(defmethod table/split-gt ((table wttree-table) pivot)
  (make-instance 'wttree-table
                 :representation (node/split-gt (test table) (representation table) pivot)
                 :test (test table)))

(defmethod table/split-lt ((table wttree-table) pivot)
  (make-instance 'wttree-table
                 :representation (node/split-lt (test table) (representation table) pivot)
                 :test (test table)))

(defmethod table/subset? ((sub wttree-table) (super wttree-table) &optional (test #'eql))
  (and (eq (test sub) (test super))
       (node/subset? (test sub) (representation sub) (representation super) test)))

(defmethod table/test ((table wttree-table))
  (test table))

(defmethod table/union ((left wttree-table) (right wttree-table))
  (make-instance 'wttree-table
                 :representation (node/union (test left) (representation left) (representation right))
                 :test (test left)))

(defmethod table/union! ((left wttree-table) (right wttree-table))
  (setf (representation left) (node/union (test left) (representation left) (representation right))))

(defmethod table/union-merge ((left wttree-table) (right wttree-table) merge)
  (make-instance 'wttree-table
                 :representation (node/union-merge (test left) (representation left) (representation right) merge)
                 :test (test left)))

(defmethod table/union-merge! ((left wttree-table) (right wttree-table) merge)
  (setf (representation left) (node/union-merge (test left) (representation left) (representation right) merge)))

(defmethod table/values ((table wttree-table))
  (node/values (representation table)))
