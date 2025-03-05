;;; -*- Lisp -*-

(defsystem "table"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :bug-tracker "https://github.com/jrm-code-project/table/issues"
  :description "Abstract TABLE library."
  :homepage "https://github.com/jrm-code-project/table/"
  :license "MIT"
  :long-description "An abstract table library that supports many different concrete table representations."
  :mailto "eval.apply@gmail.com"
  :maintainer "Joe Marshall <eval.apply@gmail.com>"
  :source-control (:git "https://github.com/jrm-code-project/table.git")
  :version "0.5.0"
  :depends-on ("alexandria" "closer-mop" "named-let" "series" "str")
  :components ((:file "alist-table"  :depends-on ("generics" "package"))
               (:file "compare"      :depends-on ("package"))
               (:file "generics"     :depends-on ("package"))
               (:file "hash-table"   :depends-on ("generics" "package"))
               (:file "node"         :depends-on ("package"))
               (:file "package")
               (:file "plist-table"  :depends-on ("generics"
                                                  "package"))
               (:file "table"        :depends-on ("alist-table"
                                                  "compare"
                                                  "generics"
                                                  "hash-table"
                                                  "node"
                                                  "package"
                                                  "plist-table"
                                                  "wttree-table"))
               (:file "wttree-table" :depends-on ("compare"
                                                  "generics"
                                                  "node"
                                                  "package"))
               ))
