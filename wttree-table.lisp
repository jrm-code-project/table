;;; -*- Lisp -*-

(in-package "TABLE")

(defun collect-wttree-table (key-series value-series &optional (test #'less))
  (declare (optimizable-series-function))
  (make-instance 'wttree-table :representation (collect-node key-series value-series test)))

(defun collect-immutable-wttree (key-series value-series &optional (test #'less))
  (declare (optimizable-series-function))
  (make-instance 'immutable-wttree :representation (collect-node key-series value-series test)))

(defun scan-immutable-wttree (table)
  (declare (optimizable-series-function 2))
  (scan-node (representation table)))

(defun scan-wttree-table (table)
  (declare (optimizable-series-function 2))
  (scan-node (representation table)))

(defun test->node-test (test)
  (ecase test
    (equal  #'less)
    (equalp #'lessp)))

(defmethod fold-table (procedure initial (table wttree-table))
  (node/inorder-fold (lambda (accum node)
                       (funcall procedure accum (node/k node) (node/v node)))
                     initial (representation table)))

(defmethod table/clear ((table wttree-table))
  (make-instance (class-of table) :metadata (copy-list (metadata table)) :representation nil :test (test table)))

(defmethod table/clear! ((table wttree-table))
  (setf (representation table) nil)
  table)

(defmethod table/copy ((table wttree-table))
  (make-instance (class-of table) :metadata (copy-list (metadata table))
                               :representation (node/copy (representation table))
                               :test (test table)))

(defmethod table/delete ((table wttree-table) &rest keys)
  (setf (representation table)
        (fold-left (lambda (node key)
                     (node/remove (test->node-test (test table)) node key))
                   (representation table)
                   keys))
  table)

(defmethod table/delete-keys ((table wttree-table) key-list)
  (setf (representation table)
        (fold-left (lambda (node key)
                     (node/remove (test->node-test (test table)) node key))
                   (representation table)
                   key-list))
  table)

(defmethod table/insert ((table wttree-table) key value)
  (make-instance (class-of table)
                 :metadata (copy-list (metadata table))
                 :representation (node/add (test->node-test (test table)) (representation table) key value)
                 :test (test table)))

(defmethod table/insert! ((table wttree-table) key value)
  (setf (representation table) (node/add (test->node-test (test table)) (representation table) key value))
  table)

(defmethod table/keys ((table wttree-table))
  (node/keys (representation table)))

(defmethod table/lookup ((table wttree-table) key &optional default)
  (let ((node (node/find (test->node-test (test table)) (representation table) key)))
    (if node (node/v node) default)))

(defmethod table/maximum ((table wttree-table))
  (let ((node (node/maximum (representation table))))
    (values (node/k node) (node/v node))))

(defmethod table/minimum ((table wttree-table))
  (let ((node (node/minimum (representation table))))
    (values (node/k node) (node/v node))))

(defmethod table/pop-maximum ((table wttree-table))
  (let ((node (node/maximum (representation table))))
    (values (node/k node)
            (node/v node)
            (make-instance 'wttree-table
                           :representation (node/remove-maximum (representation table))
                           :test (test table)))))

(defmethod table/pop-maximum! ((table wttree-table))
  (let ((node (node/maximum (representation table))))
    (setf (representation table) (node/remove-maximum (representation table)))
    (values (node/k node) (node/v node) table)))

(defmethod table/pop-minimum ((table wttree-table))
  (let ((node (node/minimum (representation table))))
    (values (node/k node)
            (node/v node)
            (make-instance 'wttree-table
                           :representation (node/remove-minimum (representation table))
                           :test (test table)))))

(defmethod table/pop-minimum! ((table wttree-table))
  (let ((node (node/minimum (representation table))))
    (setf (representation table) (node/remove-minimum (representation table)))
    (values (node/k node) (node/v node) table)))

(defmethod table/remove ((table wttree-table) &rest keys)
  (make-instance (class-of table)
                 :metadata (copy-list (metadata table))
                 :representation (fold-left (lambda (node key)
                                              (node/remove (test->node-test (test table)) node key))
                                            (representation table)
                                            keys)
                 :test (test table)))

(defmethod table/remove! ((table wttree-table) &rest keys)
  (setf (representation table) (fold-left (lambda (node key)
                                              (node/remove (test->node-test (test table)) node key))
                                            (representation table)
                                            keys))
  table)

(defmethod table/size ((table wttree-table))
  (node/size (representation table)))

(defmethod table/split-gt ((table wttree-table) pivot)
  (make-instance (class-of table)
                 :representation (node/split-gt (test->node-test (test table)) (representation table) pivot)
                 :test (test table)))

(defmethod table/split-lt ((table wttree-table) pivot)
  (make-instance (class-of table)
                 :representation (node/split-lt (test->node-test (test table)) (representation table) pivot)
                 :test (test table)))

(defmethod table/subset? ((sub wttree-table) (super wttree-table) &optional (test #'eql))
  (and (eq (test sub) (test super))
       (node/subset? (test->node-test (test sub)) (representation sub) (representation super) test)))

(defmethod table/test ((table wttree-table))
  (test table))

(defmethod table/union ((left wttree-table) (right wttree-table))
  (make-instance (class-of left)
                 :representation (node/union (test->node-test (test left)) (representation left) (representation right))
                 :test (test left)))

(defmethod table/union! ((left wttree-table) (right wttree-table))
  (setf (representation left) (node/union (test->node-test (test left)) (representation left) (representation right)))
  left)

(defmethod table/union-merge ((left wttree-table) (right wttree-table) merge)
  (make-instance (class-of left)
                 :representation (node/union-merge (test->node-test (test left)) (representation left) (representation right) merge)
                 :test (test left)))

(defmethod table/union-merge! ((left wttree-table) (right wttree-table) merge)
  (setf (representation left) (node/union-merge (test->node-test (test left)) (representation left) (representation right) merge))
  left)

(defmethod table/values ((table wttree-table))
  (node/values (representation table)))
