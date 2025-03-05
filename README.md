# table
An abstract table api with various concrete implementations such as alist, plist, hash-table, and wttree.

## Installation

You will need `alexandria`, `closer-mop`, `named-let`, `series`, and `str`

 - Clone this repository into `~/quicklisp/local-projects/`

 - (ql:quickload "table")

---

### Constructors

`make-table` _implementation_ `&rest` _initargs_\
Create an empty table.  Supported implementations are `'alist`, `'plist`, `'hash-table`, and `'wttree`

`make-singleton-table` _implementation_ _key_ _value_ `&rest` _initargs_\
Create a table with a single entry.

### Predicates

`tablep` _object_\
`table?` _object_\
Returns T iff _object_ is a table.

`table/empty?` _table_\
Returns T if there are no entries in _table_.

`table/equal?` _table1_ _table2_ `&optional` (_test_ `#'eql`)\
Returns T if _table1_ and _table2_ contain the same enties.  Entry values are compared using _test_.

`table/subset?` _subtable_ _supertable_ `&optional` (_test_ `#'eql`)\
Returns T if every entry in _subtable_ is contained in _supertable_.  Entry values are compare using _test_.

