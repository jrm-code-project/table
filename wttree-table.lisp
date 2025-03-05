;;; -*- Lisp -*-

(in-package "TABLE")

(defmethod make-table ((implementation (eql 'wttree)) &rest keys &key (metadata '()) (test #'less))
  (declare (ignore keys))
  (make-instance 'wttree-table :metadata metadata
                               :representation nil
                               :test test))

(defmethod make-singleton-table ((implementation (eql 'wttree)) key value &rest keys &key (metadata '()) (test #'less))
  (declare (ignore keys))
  (make-instance 'wttree-table :metadata metadata
                               :representation (make-node key value nil nil 1)
                               :test test))

(defun collect-wttree-table (key-series value-series &optional (test #'less))
  (declare (optimizable-series-function))
  (make-instance 'wttree-table :representation (collect-node key-series value-series test)))

(defun scan-wttree-table (table)
  (declare (optimizable-series-function 2))
  (scan-node (representation table)))

(defun table/copy ((table wttree-table))
  (make-instance 'wttree-table :metadata (copy-list (metadata table))
                               :representation (representation table)
                               :test (test table)))

(defmethod fold-table (procedure initial (table wttree-table))
  (node/inorder-fold procedure initial (representation table)))

(defmethod table/clear ((table wttree-table))
  (make-instance 'wttree-table :metadata (copy-list (metadata table)) :representation nil :test (test table)))

(defmethod table/clear! ((table wttree-table))
  (setf (representation table) nil))

(defmethod table/delete ((table wttree-table) key)
  (setf (representation table) (node/remove (test table) (representation table) key)))

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

(defmethod table/test ((table wttree-table))
  (test table))

(defmethod table/values ((table wttree-table))
  (node/values (representation table)))
