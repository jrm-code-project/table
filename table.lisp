;;; -*- Lisp -*-

(in-package "TABLE")

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

;;;
(defun sample-table (representation)
  (let ((table (make-instance representation)))
    (dotimes (i 10 table)
      (table/insert! table i (format nil "~r" i)))))

(defun table/test-split-gt ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
      (let* ((table1 (sample-table rep1))
             (table2 (sample-table rep2))
             (table1* (table/split-gt table1 5))
             (table2* (table/split-gt table2 5)))
        (assert (table/equal? table1* table2* #'string=))))))

(defun table/test-split-lt ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
      (let* ((table1 (sample-table rep1))
             (table2 (sample-table rep2))
             (table1* (table/split-lt table1 5))
             (table2* (table/split-lt table2 5)))
          (assert (table/equal? table1* table2* #'equal))))))

(defun table/test-table (representation)
  (dolist (representation2 '(alist-table plist-table hash-table wttree-table))
    (let ((table1 (make-instance representation))
          (table2 (make-instance representation2)))
      (dotimes (i 10)
        (table/insert! table1 i (format nil "~r" i))
        (table/insert! table2 i (format nil "~r" i)))
      (assert (table/equal? table1 table2 #'string=))))

  (let ((mtable (make-instance representation)))
    (do ((itable (make-instance representation) (table/insert itable i (format nil "~r" i)))
         (i 0 (1+ i)))
        ((> i 10)
         (table/remove! mtable 5)
         (multiple-value-bind (min-key min-val) (table/pop-minimum! mtable)
           (assert (zerop min-key))
           (assert (string= min-val "zero")))
         (let ((itable* (table/remove itable 5)))
           (multiple-value-bind (min-key min-val itable**) (table/pop-minimum itable*)
             (assert (zerop min-key))
             (assert (string= min-val "zero"))
             (multiple-value-bind (min-key min-value) (table/minimum itable**)
               (assert (= min-key 1))
               (assert (string= min-value "one")))
             (multiple-value-bind (max-key max-value) (table/maximum mtable)
               (assert (= max-key 10))
               (assert (string= max-value "ten")))
             (assert (equal (sort (copy-list (table->alist itable**)) #'less)
                            (sort (copy-list (table->alist mtable)) #'less)))
             (assert (equal (sort (copy-list (table/keys itable**)) #'less)
                            (sort (copy-list (table/keys mtable)) #'less)))
             (dotimes (i 9)
               (format t  "~&~s ~d~%" representation i)
               (assert (string= (table/lookup itable* (+ i 1) "five") (format nil "~r" (+ i 1))))
               (assert (string= (table/lookup mtable (+ i 1) "five") (format nil "~r" (+ i 1))))))))
      (table/insert! mtable i (format nil "~r" i)))))

(defun table/test-all ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table))
    (table/test-table representation)))
