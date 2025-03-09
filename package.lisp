;;; -*- Lisp -*-

(defpackage "TABLE"
  (:shadow "HASH-TABLE")
  (:import-from "ALEXANDRIA"
                "ALIST-HASH-TABLE"
                "ALIST-PLIST"
                "COPY-HASH-TABLE"
                "DELETE-FROM-PLIST"
                "DOPLIST"
                "HASH-TABLE-ALIST"
                "HASH-TABLE-KEYS"
                "HASH-TABLE-PLIST"
                "HASH-TABLE-VALUES"
                "PLIST-ALIST"
                "PLIST-HASH-TABLE"
                "REMOVE-FROM-PLIST"
                )
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES"
                          "DEFUN"
                          "FUNCALL"
                          "LET*"
                          "MULTIPLE-VALUE-BIND"
                          )
  (:export
   "ALIST-FOLD-LEFT"
   "ALIST-FOLD-RIGHT"
   "ALIST-KEYS"
   "ALIST-TABLE"
   "ALIST-VALUES"
   "COLLECT-ALIST-TABLE"
   "COLLECT-HASH-TABLE"
   "COLLECT-PLIST-TABLE"
   "COLLECT-WTTREE-TABLE"
   "DO-KEYS"
   "DO-TABLE"
   "DO-VALUES"
   "FOLD-TABLE"
   "HASH-TABLE"
   "HASH-TABLE-FOLD-LEFT"
   "HASH-TABLE-FOLD-RIGHT"
   "METADATA"
   "PLIST-FOLD-LEFT"
   "PLIST-FOLD-RIGHT"
   "PLIST-KEYS"
   "PLIST-TABLE"
   "PLIST-VALUES"
   "REPRESENTATION"
   "SCAN-ALIST-TABLE"
   "SCAN-HASH-TABLE"
   "SCAN-PLIST-TABLE"
   "SCAN-WTTREE-TABLE"
   "TABLE"
   "TABLE->ALIST"
   "TABLE->HASH-TABLE"
   ;; "TABLE->NODE"
   "TABLE->PLIST"
   "TABLE/CLEAR!"
   "TABLE/CLEAR"
   "TABLE/COPY"
   "TABLE/DELETE"
   "TABLE/EMPTY?"
   "TABLE/EQUAL?"
   "TABLE/INSERT!"
   "TABLE/INSERT"
   "TABLE/KEYS"
   "TABLE/LOOKUP"
   "TABLE/MAXIMUM"
   "TABLE/MINIMUM"
   "TABLE/POP-MAXIMUM!"
   "TABLE/POP-MAXIMUM"
   "TABLE/POP-MINIMUM!"
   "TABLE/POP-MINIMUM"
   "TABLE/REMOVE!"
   "TABLE/REMOVE"
   "TABLE/SIZE"
   "TABLE/SPLIT-GT"
   "TABLE/SPLIT-GT!"
   "TABLE/SPLIT-LT"
   "TABLE/SPLIT-LT!"
   "TABLE/SUBSET?"
   "TABLE/TEST"
   "TABLE/UNION"
   "TABLE/UNION!"
   "TABLE/UNION-MERGE"
   "TABLE/UNION-MERGE!"
   "TABLE/VALUES"
   "TABLE?"
   "TABLEP"
   "WTTREE-TABLE"
   )
  (:use "COMMON-LISP" "FOLD" "SERIES"))
