# table
An abstract table api with various concrete implementations such as alist, plist, hash-table, and wttree.

## Installation

You will need `alexandria`, `closer-mop`, `named-let`, `series`, and `str`

 - Clone this repository into `~/quicklisp/local-projects/`

 - (ql:quickload "table")

---

### Constructors

`make-instance` _implementation_ `&rest` _initargs_\
Create an empty table.  Supported implementations are `'alist-table`, `'plist-table`, `'hash-table`, and `'wttree-table`

### Predicates

`tablep` _object_\
`table?` _object_\
Returns T iff _object_ is a table.

`table/empty?` _table_\
Returns T if there are no entries in _table_.

`table/equal?` _table1_ _table2_ `&optional` (_test_ `#'eql`)\
Returns T if _table1_ and _table2_ contain the same entries.  Entry values are compared using _test_.

`table/subset?` _subtable_ _supertable_ `&optional` (_test_ `#'eql`)\
Returns T if every entry in _subtable_ is contained in _supertable_.  Entry values are compare using _test_.

### Selectors

`metadata` _table_\
Returns a plist associated with the table that can be used for metadata.  Guaranteed to not be used by the implementation.

`representation` _table_\
Returns the underlying representation of the table (an alist, plist, hashtable, or wttree-node).  Exposes the rep.

`table/keys` _table_\
Returns a list of the keys in _table_.  List structure may be shared, so do not modify it.

`table/values` _table_\
Returns a list of the values in _table_.  List structure may be shared, so do not modify it.

`table/size` _table_\
Returns the number of entries in _table_.

`table/split-gt` _table_ _pivot_\
Returns a new table with only those entries greater than _pivot_.

`table/split-lt` _table_ _pivot_\
Returns a new table with only those entries less than _pivot_.

`table/test` _table_\
Returns the predicate used to compare keys in _table_.

### Destructive operations

These operations modify the table object and possibly modify any shared data structures within in the table.

`table/clear!` _table_\
Delete all entries from _table_.

`table/delete` _table_ _key_\
`table/remove!` _table_ _key_\
Delete the entry associated with _key_ from _table_.

`table/insert!` _table_ _key_ _value_\
Insert a new entry mapping _key_ to _value_ in _table_.  If _key_ refers to an old value, it is overwritten.

`table/pop-maximum!` _table_\
Returns two values, the greatest key and its associated value.  The entry associated with this key and value is deleted from the table.

`table/pop-minimum!` _table_\
Returns two values, the least key and its associated value.  The entry associated with this key and value is deleted from the table.

### Non-destructive operations

These operations leave their arguments unmodified.  The results may share storage with the arguments.  These operations can be inefficient if they need to copy their arguments.

`fold-table` _procedure_ _init_ _table_\
_procedure_ takes three arguments, the result of the previous fold step, a key, and a value.

`table/clear` _table_\
Returns a table like the argument, but with no entries.

`table/insert` _table_ _key_ _value_\
Returns a new table with additional entry of _key_ associated with _value_.  Original _table_ is unmodified, but storage may be shared with original _table_.

`table/lookup` _table_ _key_ `&optional` _default_\
Returns the value associated with _key_ if it is in the table, _default if it is not.

`table/maximum` _table_\
Returns two values, the largest key in _table_, and its associated value.

`table/minimum` _table_\
Returns two values, the smallest key in _table_, and its associated value.

`table/pop-maximum` _table_\
Returns three values, the largest key in _table_, its associated value, and _table_ without an entry for that _key_.  New table may share storage with original _table_.  Original _table_ is not modified.

`table/pop-minimum` _table_\
Returns three values, the smallest key in _table_, its associated value, and _table_ without an entry for that _key_.  New table may share storage with original _table_.  Original _table_ is not modified.

`table/remove` _table_ _key_\
Returns the _table_ without an entry for _key_.  Original _table_ is not modified.  Returned _table_ may share storage with original _table_.

### Conversions

`table->alist` _table_\
Return _table_ as an association list.  Resulting alist may share storage with _table_.

`table->hash-table` _table_\
Return _table_ as a hashtable.  Resulting hashtable by share storage with _table_.

`table->plist` _table_\
Return _table_ as a property list.  Resulting plist may share storage with _table_.

### Implementation class changes

`change-class` _table_ _new-table-class_\
Modifies the implementation class for the table instance.
