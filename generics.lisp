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

(defgeneric make-table (implementation &rest initargs)
  (:documentation "Creates a new table with the given implementation.")
  (:method (implementation &rest initargs)
    (error "Unrecognized table implementation.")))

(defgeneric make-singleton-table (implementation key value &rest initargs)
  (:documentation "Creates a new table with the given implementation containing a single entry.")
  (:method (implementation key value &rest initargs)
    (error "Unrecognized table implementation.")))

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

(defgeneric table/test (table)
  (:documentation "Returns the table predicate.")
  (:method ((table t))
    (error "Not a table: ~S" table))
  (:method ((table table))
    (error "Not implemented by table subclass.")))

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
