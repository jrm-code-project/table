;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package "TABLE")

;;;
(defun table-of-representation (representation n)
  (setf (symbol-plist 'foo)
        (copy-list '(0 "zero" 1 "one" 2 "two" 3 "three" 4 "four")))
  (setf (symbol-plist 'bar)
        (copy-list '(3 "three" 4 "four" 5 "five" 6 "six")))
  (ecase representation
    (alist-table (make-instance 'alist-table
                                :initial-contents
                                (copy-alist (if (zerop n)
                                                '((0 . "zero")
                                                  (1 . "one")
                                                  (2 . "two")
                                                  (3 . "three")
                                                  (4 . "four"))
                                                '((3 . "three")
                                                  (4 . "four")
                                                  (5 . "five")
                                                  (6 . "six"))))))
    (hash-table (make-instance 'hash-table
                               :initial-contents (if (zerop n)
                                                     '((0 . "zero")
                                                       (1 . "one")
                                                       (2 . "two")
                                                       (3 . "three")
                                                       (4 . "four"))
                                                     '((3 . "three")
                                                       (4 . "four")
                                                       (5 . "five")
                                                       (6 . "six")))))
    (plist-table (make-instance 'plist-table
                                :initial-contents (if (zerop n)
                                                      '((0 . "zero")
                                                        (1 . "one")
                                                        (2 . "two")
                                                        (3 . "three")
                                                        (4 . "four"))
                                                      '((3 . "three")
                                                        (4 . "four")
                                                        (5 . "five")
                                                        (6 . "six")))))
    (wttree-table (make-instance 'wttree-table
                                 :initial-contents (if (zerop n)
                                                       '((0 . "zero")
                                                         (1 . "one")
                                                         (2 . "two")
                                                         (3 . "three")
                                                         (4 . "four"))
                                                       '((3 . "three")
                                                         (4 . "four")
                                                         (5 . "five")
                                                         (6 . "six")))))
    (immutable-alist (make-instance 'immutable-alist
                                    :initial-contents
                                    (copy-alist (if (zerop n)
                                                          '((0 . "zero")
                                                            (1 . "one")
                                                            (2 . "two")
                                                            (3 . "three")
                                                            (4 . "four"))
                                                          '((3 . "three")
                                                            (4 . "four")
                                                            (5 . "five")
                                                            (6 . "six"))))))
    (immutable-hash-table (make-instance 'immutable-hash-table
                                         :initial-contents (if (zerop n)
                                                               '((0 . "zero")
                                                                 (1 . "one")
                                                                 (2 . "two")
                                                                 (3 . "three")
                                                                 (4 . "four"))
                                                               '((3 . "three")
                                                                 (4 . "four")
                                                                 (5 . "five")
                                                                 (6 . "six")))))
    (immutable-plist (make-instance 'immutable-plist
                                    :initial-contents (if (zerop n)
                                                          '((0 . "zero")
                                                            (1 . "one")
                                                            (2 . "two")
                                                            (3 . "three")
                                                            (4 . "four"))
                                                          '((3 . "three")
                                                            (4 . "four")
                                                            (5 . "five")
                                                            (6 . "six")))))
    (immutable-wttree (make-instance 'immutable-wttree
                                     :initial-contents (if (zerop n)
                                                           '((0 . "zero")
                                                             (1 . "one")
                                                             (2 . "two")
                                                             (3 . "three")
                                                             (4 . "four"))
                                                           '((3 . "three")
                                                             (4 . "four")
                                                             (5 . "five")
                                                             (6 . "six")))))
    (symbol (if (zerop n)
                'foo
                'bar))))

(defun sample-table (representation)
  (let ((table (make-instance representation)))
    (dotimes (i 10 table)
      (table/insert! table i (format nil "~r" i)))))

(defun table/test-delete ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol))
    (let ((table (table-of-representation representation 0)))
      (table/delete table 2 3)
      (assert (string= (table/lookup table 0 "nil") "zero"))
      (assert (string= (table/lookup table 1 "nil") "one"))
      (assert (string= (table/lookup table 2 "nil") "nil"))
      (assert (string= (table/lookup table 3 "nil") "nil"))
      (assert (string= (table/lookup table 4 "nil") "four")))))

(defun table/test-delete-keys ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol))
    (let ((table (table-of-representation representation 0)))
      (table/delete-keys table '(2 3))
      (assert (string= (table/lookup table 0 "nil") "zero"))
      (assert (string= (table/lookup table 1 "nil") "one"))
      (assert (string= (table/lookup table 2 "nil") "nil"))
      (assert (string= (table/lookup table 3 "nil") "nil"))
      (assert (string= (table/lookup table 4 "nil") "four")))))

(defun table/test-delete-if ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol))
    (let ((table (table-of-representation representation 0)))
      (table/delete-if table (lambda (key value) (declare (ignore value)) (evenp key)))
      (assert (string= (table/lookup table 0 "nil") "nil"))
      (assert (string= (table/lookup table 1 "nil") "one"))
      (assert (string= (table/lookup table 2 "nil") "nil"))
      (assert (string= (table/lookup table 3 "nil") "three"))
      (assert (string= (table/lookup table 4 "nil") "nil")))))

(defun table/test-delete-if-not ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table))
    (let ((table (table-of-representation representation 0)))
      (table/delete-if-not table (lambda (key value) (declare (ignore value)) (oddp key)))
      (assert (string= (table/lookup table 0 "nil") "nil"))
      (assert (string= (table/lookup table 1 "nil") "one"))
      (assert (string= (table/lookup table 2 "nil") "nil"))
      (assert (string= (table/lookup table 3 "nil") "three"))
      (assert (string= (table/lookup table 4 "nil") "nil")))))

(defun table/test-remove ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol
                            immutable-alist immutable-hash-table immutable-plist immutable-wttree))
    (let ((table (table-of-representation representation 0)))
      (let ((answer (table/remove table 2 3)))
        (assert (string= (table/lookup answer 0 "nil") "zero"))
        (assert (string= (table/lookup answer 1 "nil") "one"))
        (assert (string= (table/lookup answer 2 "nil") "nil"))
        (assert (string= (table/lookup answer 3 "nil") "nil"))
        (assert (string= (table/lookup answer 4 "nil") "four"))))))

(defun table/test-remove-keys ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol
                            immutable-alist immutable-hash-table immutable-plist immutable-wttree))
    (let ((table (table-of-representation representation 0)))
      (let ((answer (table/remove-keys table '(2 3))))
        (assert (string= (table/lookup answer 0 "nil") "zero"))
        (assert (string= (table/lookup answer 1 "nil") "one"))
        (assert (string= (table/lookup answer 2 "nil") "nil"))
        (assert (string= (table/lookup answer 3 "nil") "nil"))
        (assert (string= (table/lookup answer 4 "nil") "four"))))))

(defun table/test-remove-if ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol
                            immutable-alist immutable-hash-table immutable-plist immutable-wttree))
    (let ((table (table-of-representation representation 0)))
      (let ((answer (table/remove-if table (lambda (key value) (declare (ignore value)) (evenp key)))))
        (assert (string= (table/lookup answer 0 "nil") "nil"))
        (assert (string= (table/lookup answer 1 "nil") "one"))
        (assert (string= (table/lookup answer 2 "nil") "nil"))
        (assert (string= (table/lookup answer 3 "nil") "three"))
        (assert (string= (table/lookup answer 4 "nil") "nil"))))))

(defun table/test-remove-if-not ()
  (dolist (representation '(alist-table hash-table plist-table wttree-table symbol
                            immutable-alist immutable-hash-table immutable-plist immutable-wttree))
    (let ((table (table-of-representation representation 0)))
      (let ((answer (table/remove-if-not table (lambda (key value) (declare (ignore value)) (oddp key)))))
        (assert (string= (table/lookup answer 0 "nil") "nil"))
        (assert (string= (table/lookup answer 1 "nil") "one"))
        (assert (string= (table/lookup answer 2 "nil") "nil"))
        (assert (string= (table/lookup answer 3 "nil") "three"))
        (assert (string= (table/lookup answer 4 "nil") "nil"))))))

(defun table/test-difference ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table symbol))
    (dolist (rep2 '(alist-table plist-table hash-table wttree-table symbol
                    immutable-alist immutable-hash-table immutable-plist immutable-wttree))
      (let ((table1 (table-of-representation rep1 0))
            (table2 (table-of-representation rep2 1)))
        (let ((answer (table/difference table1 table2)))

          (assert (string= (table/lookup answer 0 "nil") "zero"))
          (assert (string= (table/lookup answer 1 "nil") "one"))
          (assert (string= (table/lookup answer 2 "nil") "two"))
          (assert (string= (table/lookup answer 3 "nil") "nil"))
          (assert (string= (table/lookup answer 4 "nil") "nil"))
          (assert (string= (table/lookup answer 5 "nil") "nil"))
          (assert (string= (table/lookup answer 6 "nil") "nil")))))))

(defun table/test-difference! ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
      (let ((table1 (make-instance rep1))
            (table2 (make-instance rep2)))
        (dotimes (i 7)
          (table/insert! table1 i (format nil "~r" i)))
        (dotimes (i 3)
          (table/insert! table2 (+ i 1) (format nil "~r" (+ i 1))))

        (table/difference! table1 table2)

          (assert (string= (table/lookup table1 0 "nil") "zero"))
          (assert (string= (table/lookup table1 1 "nil") "nil"))
          (assert (string= (table/lookup table1 2 "nil") "nil"))
          (assert (string= (table/lookup table1 3 "nil") "nil"))
          (assert (string= (table/lookup table1 4 "nil") "four"))
          (assert (string= (table/lookup table1 5 "nil") "five"))
          (assert (string= (table/lookup table1 6 "nil") "six"))))))

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

(defun table/test-union ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (let ((table1 (make-instance rep1)))
      (dotimes (i 7)
        (table/insert! table1 i (format nil "~r" i)))
      (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
        (let ((table2 (make-instance rep2)))
          (dotimes (i 5)
            (table/insert! table2 (+ i 5) (format nil "~r" (+ i 5))))
          (let ((table3 (table/union table1 table2)))
            (dotimes (i 10)
              (assert (string= (table/lookup table3 i "nil") (format nil "~r" i))))))))))

(defun table/test-union! ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (let ((table1 (make-instance rep1)))
      (dotimes (i 7)
        (table/insert! table1 i (format nil "~r" i)))
      (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
        (let ((table2 (make-instance rep2)))
          (dotimes (i 5)
            (table/insert! table2 (+ i 5) (format nil "~r" (+ i 5))))
          (table/union! table1 table2)
          (dotimes (i 10)
            (assert (string= (table/lookup table1 i "nil") (format nil "~r" i)))))))))

(defun table/test-union-merge ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
      (let ((table1 (make-instance rep1))
            (table2 (make-instance rep2)))
        (dotimes (i 7)
          (table/insert! table1 i (format nil "~r" i)))
        (dotimes (i 5)
          (table/insert! table2 (+ i 5) (format nil "~r" (+ i 5))))
        (let ((table3 (table/union-merge table1 table2 (lambda (k v1 v2) (declare (ignore k)) (cons v1 v2)))))
          (dotimes (i 5)
            (assert (string= (table/lookup table3 i "nil") (format nil "~r" i))))
          (dotimes (i 2)
            (assert (equal (table/lookup table3 (+ i 5) "nil")
                           (cons (format nil "~r" (+ i 5))
                                 (format nil "~r" (+ i 5))))))
          (dotimes (i 3)
            (assert (string= (table/lookup table3 (+ i 7) "nil") (format nil "~r" (+ i 7))))))))))

(defun table/test-union-merge! ()
  (dolist (rep1 '(alist-table plist-table hash-table wttree-table))
    (dolist (rep2 '(alist-table plist-table hash-table wttree-table))
      (let ((table1 (make-instance rep1))
            (table2 (make-instance rep2)))
        (dotimes (i 7)
          (table/insert! table1 i (format nil "~r" i)))
        (dotimes (i 5)
          (table/insert! table2 (+ i 5) (format nil "~r" (+ i 5))))
        (table/union-merge! table1 table2 (lambda (k v1 v2) (declare (ignore k)) (cons v1 v2)))
        (dotimes (i 5)
          (assert (string= (table/lookup table1 i "nil") (format nil "~r" i))))
        (dotimes (i 2)
          (assert (equal (table/lookup table1 (+ i 5) "nil")
                         (cons (format nil "~r" (+ i 5))
                               (format nil "~r" (+ i 5))))))
        (dotimes (i 3)
          (assert (string= (table/lookup table1 (+ i 7) "nil") (format nil "~r" (+ i 7)))))))))

(defun table/test-table (representation)
  (dolist (representation2 '(alist-table plist-table hash-table wttree-table))
    (let ((table1 (make-instance representation))
          (table2 (make-instance representation2)))
      (dotimes (i 10)
        (table/insert! table1 i (format nil "~r" i))
        (table/insert! table2 i (format nil "~r" i)))
      (assert (table/equal? table1 table2 #'string=))))

  (let ((mtable (make-instance representation)))
    (do ((itable (make-instance (ecase representation
                                  (alist-table 'immutable-alist)
                                  (hash-table 'immutable-hash-table)
                                  (plist-table 'immutable-plist)
                                  (wttree-table 'immutable-wttree)))
                 (table/insert itable i (format nil "~r" i)))
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
               ;; (format t  "~&~s ~d~%" representation i)
               (assert (string= (table/lookup itable* (+ i 1) "five") (format nil "~r" (+ i 1))))
               (assert (string= (table/lookup mtable (+ i 1) "five") (format nil "~r" (+ i 1))))))))
      (table/insert! mtable i (format nil "~r" i)))))

(defun table/test-all ()
  (table/test-delete)
  (table/test-delete-keys)
  (table/test-delete-if)
  (table/test-delete-if-not)
  (table/test-remove)
  (table/test-remove-keys)
  (table/test-remove-if)
  (table/test-remove-if-not)
  (table/test-difference)
  (table/test-difference!)
  (table/test-union)
  (table/test-union!)
  (table/test-union-merge)
  (table/test-union-merge!)
  (dolist (representation '(alist-table hash-table plist-table wttree-table))
    (table/test-table representation))
)
