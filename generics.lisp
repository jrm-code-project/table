;;; -*- Lisp -*-

(in-package "TABLE")

(define-condition attempted-mutation-error (error)
                  ((function-name :initarg :function-name
                                  :reader attempted-mutation-error-function-name)
                   (immutable-object :initarg :immutable-object
                                     :reader attempted-mutation-error-immutable-object))
  (:report (lambda (condition stream)
             (format stream "Attempt to mutate ~A in ~A."
                     (attempted-mutation-error-immutable-object condition)
                     (attempted-mutation-error-function-name condition)))))

(defun signal-attempted-mutation (function-name object)
  (error 'attempted-mutation-error
         :function-name function-name
         :immutable-object object))

;;;
;;; METADATA
;;;

(defgeneric metadata (object)
  (:documentation "Returns the metadata plist of object."))
(defgeneric (setf metadata) (table new-value)
  (:documentation "Sets the metadata plist of object."))

(defclass metadata ()
  ((metadata :accessor metadata
             :initarg :metadata
             :initform nil)))

;;;
;;; TABLE
;;;

(defgeneric representation (table)
  (:documentation "Returns the representation of the object."))

(defgeneric (setf representation) (new-value table)
  (:documentation "Sets the representation of the object."))

(defclass table (metadata)
  ((representation
    :reader representation
    :initarg :representation
    :initform nil)))

(defmethod (setf representation) (new-value (table table))
  (setf (slot-value table 'representation) new-value))

(defclass immutable-table (table)
  ())

(defmethod (setf representation) :before (new-value (table immutable-table))
  (signal-attempted-mutation '(setf representation) table))

(defgeneric table? (object)
  (:documentation "Returns T if object is a table, NIL otherwise.")
  (:method ((object t))   nil)
  (:method ((object table))  t))

(defgeneric tablep (object)
  (:documentation "Returns T if object is a table, NIL otherwise.")
  (:method ((object t))   nil)
  (:method ((object table))  t))

(defgeneric immutable-table? (object)
  (:documentation "Returns T if object is an immutable-table, NIL otherwise.")
  (:method ((object t))   nil)
  (:method ((object immutable-table)) t))

(defgeneric immutable-tablep (object)
  (:documentation "Returns T if object is an immutable-table, NIL otherwise.")
  (:method ((object t))   nil)
  (:method ((object immutable-table)) t))

(defmethod print-object ((table table) stream)
  (print-unreadable-object (table stream :type t)
    (format stream "~d ~a" (table/size table) (table/test table))))

(defgeneric table->alist (table)
  (:documentation "Returns an alist representation of the table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table->hashtable (table)
  (:documentation "Returns a hashtable representation of the table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table->plist (table)
  (:documentation "Returns a plist representation of the table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table->node (table)
  (:documentation "Returns a wttree-node representation of the table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric fold-table (function initial-value table)
  (:documentation "Folds the table with the given function and initial value.")
  (:method (function initial-value (table t))
    (error "Not a table: ~S" table))
  (:method (function initial-value (table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/clear (table)
  (:documentation "Removes all entries from the table (nondestructive).")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (make-instance (class-of table) :test (test table) :allow-other-keys t)))

(defgeneric table/clear! (table)
  (:documentation "Removes all entries from the table (destructive).")
  (:method :before ((table immutable-table))
    (signal-attempted-mutation 'table/clear! table))
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (fold-left (lambda (acc key)
                 (table/delete acc key)
                 acc)
               table
               (table/keys table))))

(defgeneric table/copy (table)
  (:documentation "Returns a new table that does not share storage with original table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/delete (table &rest keys)
  (:documentation "Destructive removal of keys from a table.  Returns the old table after it is modified.")
  (:method ((table t) &rest keys)
    (declare (ignore keys))
    (error "Not a table: ~S" table))
  (:method :before ((table immutable-table) &rest keys)
    (declare (ignore keys))
    (signal-attempted-mutation 'table/delete table))
  (:method ((table table) &rest keys)
    (table/delete-keys table keys)))

(defgeneric table/delete-if (table predicate)
  (:documentation "Destructive removal of entries from a table.  Returns the old table after it is modified.")
  (:method ((table t) predicate)
    (declare (ignore predicate))
    (error "Not a table: ~S" table))
  (:method :before ((table immutable-table) predicate)
    (declare (ignore predicate))
    (signal-attempted-mutation 'table/delete-if table))
  (:method ((table table) predicate)
    (setf (representation table)
          (representation (fold-table (lambda (acc key value)
                                        (if (funcall predicate key value)
                                            acc
                                            (table/insert! acc key value)))
                                      (table/clear table)
                                      table)))
    table))

(defgeneric table/delete-if-not (table predicate)
  (:documentation "Destructive removal of entries from a table.  Returns the old table after it is modified.")
  (:method ((table t) predicate)
    (declare (ignore predicate))
    (error "Not a table: ~S" table))
  (:method ((table immutable-table) predicate)
    (declare (ignore predicate))
    (signal-attempted-mutation 'table/delete-if-not table))
  (:method ((table table) predicate)
    (setf (representation table)
          (representation (fold-table (lambda (acc key value)
                                        (if (funcall predicate key value)
                                            (table/insert! acc key value)
                                            acc))
                                      (table/clear table)
                                      table)))
    table))

(defgeneric table/delete-keys (table key-list)
  (:documentation "Destructive removal of a set of entries from a table.  Returns the old table after it is modified.") 
  (:method ((table t) key-list) 
   (declare (ignore key-list))
    (error "Not a table: ~S" table))
  (:method ((table immutable-table) predicate)
    (declare (ignore predicate))
    (signal-attempted-mutation 'table/delete-keys table))
  (:method ((table table) key-list)
    (table/delete-if table (lambda (key value)
                             (declare (ignore value))
                             (member key key-list :test (table/test table))))))

(defgeneric table/difference (left right)
  (:documentation "Returns a table with the entries in left that do not have keys in right.")
  (:method ((left t) (right t))
    (error "Not tables: ~s" (list left right)))
  (:method ((left table) right)
    (error "Not a table: ~s" right))
  (:method (left (right table))
    (error "Not a table: ~s" left))
  (:method ((left table) (right table))
    (let ((predicate (let ((keys (table/keys right)))
                       (lambda (key value)
                         (declare (ignore value))
                         (member key keys :test (table/test left))))))
      (table/remove-if left predicate))))

(defgeneric table/difference! (left right)
  (:documentation "Delete from _left_ table the keys in _right.")
  (:method ((left t) (right t))
    (error "Not tables: ~s" (list left right)))
  (:method ((left table) right)
    (error "Not a table: ~s" right))
  (:method ((left immutable-table) right)
    (declare (ignore right))
    (error "Table is immutable."))
  (:method (left (right table))
    (error "Not a table: ~s" left))
  (:method ((left table) (right table))
    (let ((predicate (let ((keys (table/keys right)))
                       (lambda (key value)
                         (declare (ignore value))
                         (member key keys :test (table/test left))))))
      (table/delete-if left predicate))))

(defgeneric table/insert (table key value)
  (:documentation "Non-destructive insertion of an entry into a table.  Returns a new table with the entry inserted.")
  (:method ((table t) key value)
    (error "Not a table: ~S" table))
  (:method ((table table) key value)
    (error "Not implemented by table subclass.")))

(defgeneric table/insert! (table key value)
  (:documentation "Destructive insertion of an entry into a table.  Returns the value inserted.")
  (:method ((table t) key value)
    (declare (ignore key value))
    (error "Not a table: ~S" table))
  (:method :before ((table immutable-table) key value)
    (declare (ignore key value))
    (signal-attempted-mutation 'table/insert! table))
  (:method ((table table) key value)
    (error "Not implemented by table subclass.")))

(defgeneric table/intersection (left right)
  (:documentation "Remove all entries from left that do not have keys present in right.")
  (:method (left right)
    (declare (ignore left right))
    (error "Not tables."))
  (:method ((left table) right)
    (declare (ignore left right))
    (error "Right is not a table."))
  (:method (left (right table))
    (declare (ignore left right))
    (error "Left is not a table."))
  (:method ((left table) (right table))
    (let ((predicate (let ((keys (table/keys right)))
                       (lambda (key value)
                         (declare (ignore value))
                         (member key keys :test (table/test left))))))
      (table/remove-if-not left predicate))))

(defgeneric table/intersection! (left right)
  (:documentation "Remove all entries from left that do not have keys present in right.")
  (:method (left right)
    (declare (ignore left right))
    (error "Not tables."))
  (:method ((left table) right)
    (declare (ignore left right))
    (error "Right is not a table."))
  (:method ((left immutable-table) right)
    (declare (ignore right))
    (error "Table is immutable."))
  (:method (left (right table))
    (declare (ignore left right))
    (error "Left is not a table."))
  (:method ((left table) (right table))
    (let ((predicate (let ((keys (table/keys right)))
                       (lambda (key value)
                         (declare (ignore value))
                         (member key keys :test (table/test left))))))
      (table/delete-if-not left predicate))))
 
(defgeneric table/keys (table)
  (:documentation "Returns a list of keys in the table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/lookup (table key &optional default)
  (:documentation "Lookup a key in a table.  Defaut is returned if key is not found.")
  (:method ((table t) key &optional default)
    (declare (ignore key default))
    (error "Not a table: ~S" table))
  (:method ((table table) key &optional default)
    (declare (ignore key default))
    (error "Not implemented by table subclass.")))

(defgeneric table/maximum (table)
  (:documentation "Returns two values, the maximal key and its associated value.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/minimum (table)
  (:documentation "Returns two values, the minimal key and its associated value.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/pop-maximum (table)
  (:documentation "Returns three values, the maximal key, its associated value, and the table without the key.")
  (:method ((table t))
    (error "Not a table: ~s" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/pop-maximum! (table)
  (:documentation "Returns two values, the maximal key, its associated value.  Table is modified.")
  (:method ((table t))
    (error "Not a table: ~s" table))
  (:method :before ((table immutable-table))
    (signal-attempted-mutation 'table/pop-maximum! table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/pop-minimum (table)
  (:documentation "Returns three values, the minimal key, its associated value, and the table without the key.")
  (:method ((table t))
    (error "Not a table: ~s" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/pop-minimum! (table)
  (:documentation "Returns two values, the minimal key and its associated value.  Table is modified.")
  (:method ((table t))
    (error "Not a table: ~s" table))
  (:method :before ((table immutable-table))
    (signal-attempted-mutation 'table/pop-minimum! table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/remove (table &rest keys)
  (:documentation "Non-destructive removal of an entry in a table.  Returns a new table without an entry for key.")
  (:method ((table t) &rest keys)
    (declare (ignore keys))
    (error "Not a table: ~S" table))
  (:method ((table table) &rest keys)
    (table/remove-keys table keys)))

(defgeneric table/remove-keys (table keys)
  (:documentation "Non-destructive removal of entries from a table.  Returns a new table without entries for keys.")
  (:method ((table t) keys)
    (declare (ignore keys))
    (error "Not a table: ~S" table))
  (:method ((table table) keys)
    (let ((predicate (lambda (key value)
                       (declare (ignore value))
                       (member key keys :test (table/test table)))))
      (table/remove-if table predicate))))

(defgeneric table/remove-if (table predicate)
  (:documentation "Non-destructive removal of an entry in a table.  Returns a new table without an entry for key.")
  (:method ((table t) predicate)
    (declare (ignore predicate))
    (error "Not a table: ~S" table))
  (:method ((table table) predicate)
    (fold-table (lambda (acc key value)
                  (if (funcall predicate key value)
                      acc
                      (table/insert acc key value)))
                (table/clear table)
                table)))

(defgeneric table/remove-if-not (table predicate)
  (:documentation "Non-destructive removal of an entry in a table.  Returns a new table without an entry for key.")
  (:method ((table t) predicate)
    (declare (ignore predicate))
    (error "Not a table: ~S" table))
  (:method ((table table) predicate)
    (fold-table (lambda (acc key value)
                  (if (funcall predicate key value)
                      (table/insert acc key value)
                      acc))
                (table/clear table)
                table)))

(defgeneric table/remove! (table &rest keys)
  (:documentation "Destructive removal of an entry from a table.  Returns the old table after it is modified.")
  (:method ((table t) &rest keys)
    (declare (ignore keys))
    (error "Not a table: ~S" table))
  (:method :before ((table immutable-table) &rest keys)
    (declare (ignore keys))
    (signal-attempted-mutation 'table/remove! table))
  (:method ((table table) &rest keys)
    (table/delete-keys table keys)))

(defgeneric table/remove-keys! (table keys)
  (:documentation "Destructive removal of keys from a table.  Returns original table with keys removed.")
  (:method ((table t) keys)
    (declare (ignore keys))
    (error "Not a table: ~S" table))
  (:method :before ((table immutable-table) keys)
    (signal-attempted-mutation 'table/remove-keys! table))
  (:method ((table table) keys)
    (table/delete-keys table keys)))

(defgeneric table/size (table)
  (:documentation "Returns the number of entries in table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/split-gt (table pivot)
  (:documentation "Returns a table of the same type consisting of only those entries whose key is greater than pivot.")
  (:method ((table t) pivot)
    (error "Not a table: ~S" table))
  (:method ((table table) pivot)
    (error "Not implemented by table subclass.")))

(defgeneric table/split-lt (table pivot)
  (:documentation "Returns a table of the same type consisting of only those entries whose key is less than pivot.")
  (:method ((table t) pivot)
    (error "Not a table: ~S" table))
  (:method ((table table) pivot)
    (error "Not implemented by table subclass.")))

(defgeneric table/subset? (sub super &optional test)
  (:documentation "T if every key in sub is in super.")
  (:method ((sub t) (super t) &optional test)
    (declare (ignore test))
    (error "Not tables: ~s ~s" sub super))
  (:method ((sub t) (super table) &optional test)
    (declare (ignore test))
    (error "Not a table: ~s" sub))
  (:method ((sub table) (super t) &optional test)
    (declare (ignore test))
    (error "Not a table: ~s" super))
  (:method ((sub table) (super table) &optional (test #'eql))
    (let ((default (cons nil nil)))
      (every (lambda (key)
               (funcall test
                        (table/lookup sub key default)
                        (table/lookup super key default)))
             (table/keys sub)))))

(defgeneric table/test (table)
  (:documentation "Returns the table predicate.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/union (left right)
  (:documentation "Returns a table with the union of entries in both left and right.")
  (:method ((left table) (right table))
    (let ((answer (table/copy left)))
      (table/union! answer right)
      answer)))

(defgeneric table/union! (left right)
  (:documentation "Adds to left those entries in right that do not already have a value.")
  (:method ((left t) (right t))
    (error "Not tables: ~s" (list left right)))
  (:method ((left table) right)
    (error "Not a table: ~s" right))
  (:method :before ((left immutable-table) right)
    (signal-attempted-mutation 'table/union! left))
  (:method (left (right table))
    (error "Not a table: ~s" left))
  (:method ((left table) (right table))
    (let ((not-found (cons nil nil)))
      (fold-table (lambda (acc key value)
                    (if (eq (table/lookup acc key not-found) not-found)
                        (table/insert! acc key value)
                        acc))
                  left
                  right))))

(defgeneric table/union-merge (left right merge)
  (:documentation "Returns a table with the union of entries in both left and right. Entries in both are merged with the merge function.")
  (:method ((left t) (right t) merge)
    (error "Not tables: ~s" (list left right)))
  (:method ((left table) right merge)
    (error "Not a table: ~s" right))
  (:method (left (right table) merge)
    (error "Not a table: ~s" left))
  (:method ((left table) (right table) merge)
    (let ((answer (table/copy left)))
      (table/union-merge! answer right merge)
      answer)))

(defgeneric table/union-merge! (left right merge)
  (:documentation "Adds to left the entries in right.  Entries that are in both are merged.")
  (:method ((left t) (right t) merge)
    (error "Not tables: ~s" (list left right)))
  (:method ((left table) right merge)
    (error "Not a table: ~s" right))
  (:method :before ((left immutable-table) right merge)
    (signal-attempted-mutation 'table/union-merge! left))
  (:method (left (right table) merge)
    (error "Not a table: ~s" left))
  (:method ((left table) (right table) merge)
    (let ((not-found (cons nil nil)))
      (fold-table (lambda (acc key value)
                    (let ((probe (table/lookup acc key not-found)))
                      (if (eq probe not-found)
                          (table/insert! acc key value)
                          (table/insert! acc key (funcall merge key probe value)))))
                  left
                  right))))

(defgeneric table/values (table)
  (:documentation "Returns a list of values in the table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defclass alist-table (table)
  ((test :accessor test
         :initarg :test
         :initform #'eql)))

(defmethod shared-initialize :after ((instance alist-table) slot-names &rest initargs &key &allow-other-keys)
  (when (or (eq slot-names 't)
            (member 'representation slot-names))
    (if (member :representation initargs)
        (setf (slot-value instance 'representation) (getf initargs :representation))
        (let* ((default (cons nil nil))
               (initial-contents (getf initargs :initial-contents default)))
          (unless (eq initial-contents default)
            (setf (slot-value instance 'representation)
                  (etypecase initial-contents
                    (null       nil)
                    (alist      (copy-alist initial-contents))
                    (plist      (plist-alist initial-contents))
                    (hash-table (hash-table-alist initial-contents))
                    (symbol     (plist-alist (symbol-plist initial-contents)))
                    (table      (table->alist initial-contents)))))))))

(defclass immutable-alist (alist-table immutable-table)
  ())

(defclass hash-table (table)
  ())

(defmethod shared-initialize :after ((instance hash-table) slot-names &rest initargs &key &allow-other-keys)
  (when (or (eq slot-names 't)
            (member 'representation slot-names))
    (if (member :representation initargs)
        (setf (slot-value instance 'representation) (getf initargs :representation))
        (let* ((default (cons nil nil))
               (initial-contents (getf initargs :initial-contents default)))
          (unless (eq initial-contents default)
            (setf (slot-value instance 'representation)
                  (etypecase initial-contents
                    (null       (make-hash-table :test (getf initargs :test 'eql)))
                    (alist      (alist-hash-table initial-contents))
                    (plist      (plist-hash-table initial-contents))
                    (hash-table (copy-hash-table initial-contents))
                    (symbol     (plist-hash-table (symbol-plist initial-contents)))
                    (table      (table->hash-table initial-contents)))))))))

(defclass immutable-hash-table (hash-table immutable-table)
  ())

(defclass plist-table (table)
  ())

(defmethod shared-initialize :after ((instance plist-table) slot-names &rest initargs &key &allow-other-keys)
  (when (or (eq slot-names 't)
            (member 'representation slot-names))
    (if (member :representation initargs)
        (setf (slot-value instance 'representation) (getf initargs :representation))
        (let* ((default (cons nil nil))
               (initial-contents (getf initargs :initial-contents default)))
          (unless (eq initial-contents default)
            (setf (slot-value instance 'representation)
                  (etypecase initial-contents
                    (null nil)
                    (alist      (alist-plist initial-contents))
                    (plist      (copy-plist initial-contents))
                    (hash-table (hash-table-plist initial-contents))
                    (symbol     (copy-plist (symbol-plist initial-contents)))
                    (table      (table->plist initial-contents)))))))))

(defclass immutable-plist (plist-table immutable-table)
  ())

(defclass wttree-table (table)
  ((test :accessor test
         :initarg :test
         :initform 'equal)))

(defmethod shared-initialize :after ((instance wttree-table) slot-names &rest initargs &key &allow-other-keys)
  (when (or (eq slot-names 't)
            (member 'representation slot-names))
    (if (member :representation initargs)
        (setf (slot-value instance 'representation) (getf initargs :representation))
        (let* ((default (cons nil nil))
               (initial-contents (getf initargs :initial-contents default)))
          (unless (eq initial-contents default)
            (setf (slot-value instance 'representation)
                  (etypecase initial-contents
                    (null       nil)
                    (alist      (alist->node initial-contents))
                    (plist      (plist->node initial-contents))
                    (hash-table (hash-table->node initial-contents))
                    (symbol     (plist->node (symbol-plist initial-contents)))
                    (table      (table->node initial-contents)))))))))

(defclass immutable-wttree (wttree-table immutable-table)
  ())

(defgeneric table->alist (object)
  (:method ((object t))
    (error "Not a table: ~S" object))
  (:method ((object immutable-alist))
    (copy-alist (representation object)))
  (:method ((object alist-table))
    (representation object)))

(defgeneric table->hash-table (object &rest initargs)
  (:method ((object t) &rest initargs)
    (declare (ignore initargs))
    (error "Not a table: ~S" object))
  (:method ((object immutable-hash-table) &rest initargs)
    (declare (ignore initargs))
    (copy-hash-table (representation object)))
  (:method ((object hash-table) &rest initargs)
    (declare (ignore initargs))
    (representation object)))

(defgeneric table->plist (object)
  (:method ((object t))
    (error "Not a table: ~S" object))  
  (:method ((object immutable-plist))
    (copy-plist (representation object)))
  (:method ((object plist-table))
    (representation object)))

(defgeneric table->node (object)
  (:method ((object t))
    (error "Not a table: ~S" object))
  (:method ((object immutable-wttree))
    (node/copy (representation object)))
  (:method ((object wttree-table))
    (representation object)))
