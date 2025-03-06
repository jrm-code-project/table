;;; -*- Lisp -*-

(in-package "TABLE")

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
(defgeneric (setf representation) (table new-value)
  (:documentation "Sets the representation of the object."))

(defclass table (metadata)
  ((representation :accessor representation
        :initarg :representation
        :initform nil)))

(defgeneric table? (object)
  (:documentation "Returns T if object is a table, NIL otherwise.")
  (:method ((object t))   nil)
  (:method ((object table)) t))

(defgeneric tablep (object)
  (:documentation "Returns T if object is a table, NIL otherwise.")
  (:method ((object t))   nil)
  (:method ((object table)) t))

(defmethod print-object ((table table) stream)
  (print-unreadable-object (table stream :type t)
    (format stream "~d ~a" (table/size table) (table/test table))))

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
    (error "Not implemented by table subclass.")))

(defgeneric table/clear! (table)
  (:documentation "Removes all entries from the table (destructive).")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/copy (table)
  (:documentation "Returns a new table that does not share storage with original table.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/delete (table key)
  (:documentation "Destructive removal of an entry from a table.  Returns the old table after it is modified.")
  (:method ((table t) key)
    (declare (ignore key))
    (error "Not a table: ~S" table))
  (:method ((table table) key)
    (error "Not implemented by table subclass.")))

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
  (:method ((table table) key value)
    (error "Not implemented by table subclass.")))

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
  (:method ((table table))
    (error "Not implemented by table subclass.")))

(defgeneric table/remove (table key)
  (:documentation "Non-destructive removal of an entry in a table.  Returns a new table without an entry for key.")
  (:method ((table t) key)
    (declare (ignore key))
    (error "Not a table: ~S" table))
  (:method ((table table) key)
    (error "Not implemented by table subclass.")))

(defgeneric table/remove! (table key)
  (:documentation "Destructive removal of an entry from a table.  Returns the old table after it is modified.")
  (:method ((table t) key)
    (declare (ignore key))
    (error "Not a table: ~S" table))
  (:method ((table table) key)
    (error "Not implemented by table subclass.")))

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
  (:method ((left t) (right t))
    (error "Not tables: ~s" (list left right)))
  (:method ((left table) right)
    (error "Not a table: ~s" right))
  (:method (left (right table))
    (error "Not a table: ~s" left))
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
  (:method (left (right table))
    (error "Not a table: ~s" left))
  (:method ((left table) (right table))
    (let ((not-found (cons nil nil)))
      (dolist (entry (table->alist right) left)
        (when (eq (table/lookup left (car entry) not-found) not-found)
          (table/insert! left (car entry) (cdr entry)))))))

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

(defclass hash-table (table)
  ())

(defmethod shared-initialize :after ((instance hash-table) slot-names &rest initargs &key &allow-other-keys)
  (when (or (eq slot-names 't) (member :representation slot-names))
    (unless (getf initargs :representation)
      (setf (representation instance) (apply #'make-hash-table initargs)))))

(defclass plist-table (table)
  ())

(defclass wttree-table (table)
  ((test :accessor test
         :initarg :test
         :initform #'less)))

(defgeneric table->alist (object)
  (:method ((object t))
    (error "Not a table: ~S" object))
  (:method ((object alist-table))
    (representation object)))

(defgeneric table->hash-table (object &rest initargs)
  (:method ((object t) &rest initargs)
    (declare (ignore initargs))
    (error "Not a table: ~S" object))
  (:method ((object hash-table) &rest initargs)
    (declare (ignore initargs))
    (representation object)))

(defgeneric table->plist (object)
  (:method ((object t))
    (error "Not a table: ~S" object))
  (:method ((object plist-table))
    (representation object)))

(defgeneric table->node (object)
  (:method ((object t))
    (error "Not a table: ~S" object))
  (:method ((object wttree-table))
    (representation object)))
