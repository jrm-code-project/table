# table
An abstract table api with various mutable and immmutable concrete implementations, such as property lists, association lists, hash tables, and balanced trees.

## Installation

You will need `alexandria`, `closer-mop`, `named-let`, `series`, and `str`

 - Clone this repository into `~/quicklisp/local-projects/`

 - (ql:quickload "table")

---

### Classes
Four implementation strategies are provided, with a mutable and immutable variant for each strategy.  Tables are instances of classes of the appropriate strategy.  `immutable-` tables can never be side effected, and an error is signalled on any attempt to do so.

`alist-table`\
`immutable-alist`\
  - a table implemented as an association list
  - operations on alists are typically linear in the number of entries, but fast enough for small tables

`hash-table`\
`immutable-hash-table`\
  - a table implemented as a hash-table
  - many operations on hash tables are O(1), but hash tables waste space
  - some operations on hash tables are O(n)

`plist-table`\
`immutable-plist`\
  - a table implemented as a property list
  - operations on plists are typically linear in the number of entries, but fast enough for small plists
  - keys are always compared with eql

`wttree-table`\
`immutable-wttree`\
  - a table implemented as a weight-balanced tree
  - operations on weight-balanced trees are typically O(log n) in the number of entries, leading to algoriths almost as fast as a hash table.
  - Entries preserve order of keys, so finding the smallest and largest key in the table is O(log n)

In addition, a symbol may be used as a table.  The entries are stored in the property list of the symbol.

### Constructors
Tables are constructed _ab initio_ by instantiating the appropriate class.  If no _initargs_ are suppled, an empty table with default properties is constructed.  The following initargs are valid:

  - `:test` specifies the key equivalance predicate for the table.  plist-tables ignore this and always use `'eql`
  - `:initial-contents` specifies an initial set of entries to be placed in the table.  This may be specified as an association list, a property list, a hash table, or another instance of a table object.

`make-instance` _implementation_ `&rest` _initargs_\
Create a table.

### Predicates

`tablep` _object_\
`table?` _object_\
Returns T iff _object_ is a table.

`table/immutable?` _object_\
`table/immutablep` _object_\
Returns T if _object_ is an immutable table.

`table/empty?` _table_\
Returns T if there are no entries in _table_.

`table/equal?` _table1_ _table2_ `&optional` (_test_ `#'eql`)\
Returns T if _table1_ and _table2_ contain the same entries.  Entry keys are compared by the key compare function associated with _table1_.  Entry values are compared using _test_.  This operation is quite expensive as it must iterate through all entries in both tables.

`table/subset?` _subtable_ _supertable_ `&optional` (_test_ `#'eql`)\
Returns T if every entry in _subtable_ is contained in _supertable_.  Entry values are compare using _test_.  This operation takes O(n)*O(lookup)

### Selectors

`metadata` _table_\
Returns a plist associated with the table that can be used for metadata.  Guaranteed to not be used by the implementation.

`representation` _table_\
Returns the underlying representation of the table (an alist, plist, hashtable, or wttree-node).  If used on an immutable table, a copy of the representation is returned.

`table/keys` _table_\
Returns a list of the keys in _table_.  List structure may be shared, so do not modify it.  O(n) in the number of entries in the table.

`table/values` _table_\
Returns a list of the values in _table_.  List structure may be shared, so do not modify it. O(n) in the number of entries in the table.

`table/size` _table_\
Returns the number of entries in _table_.  O(1) for hash tables and trees, O(n) for alists and plists.

`table/test` _table_\
Returns the predicate used to compare keys in _table_.

### Iterator

`do-table` (_key_ _value_ _table_ `&optional` (_retval_ `nil`)) `&body` _body_\
Iterate over _key_, _value_ pairs in _table_ running _body_ on each pair.  Return _retval_ when done.


### Destructive operations

These operations modify the table object and possibly modify any shared data structures within in the table.  An error is signalled if one of these is called on an immutable table.

`table/clear!` _table_\
Delete all entries from _table_.  O(1) for alists, plists, and wttrees.  O(n) for hash tables.

`table/delete` _table_ _key_\
`table/remove!` _table_ _key_\
Delete the entry associated with _key_ from _table_.

`table/insert!` _table_ _key_ _value_\
Insert a new entry mapping _key_ to _value_ in _table_.  If _key_ refers to an old value, it is overwritten.

`table/pop-maximum!` _table_\
Returns two values, the greatest key and its associated value.  The entry associated with this key and value is deleted from the table.

`table/pop-minimum!` _table_\
Returns two values, the least key and its associated value.  The entry associated with this key and value is deleted from the table.

`table/union!` _left_ _right_\
Adds to _left_ those entries in _right_ that do not already appear in _left_.

`table/union-merge!` _left_ _right_ _merge-fn_\
Adds to _left_ the entries in _right_.  If an entry appears in _both_, _merge-fn_ is called to merge the values.

### Non-destructive operations

These operations leave their arguments unmodified.  The results may share storage with the arguments.  These operations can be inefficient if they need to copy their arguments.

`fold-table` _procedure_ _init_ _table_\
_procedure_ takes three arguments, the result of the previous fold step, a key, and a value.

`table/clear` _table_\
Returns a table like the argument, but with no entries.

`table/copy` _table_\
Returns a copy of _table_ that shares no storage with _table_.  Destructive operations on the copy will not cause side effects on the original.  Table keys and values, however, are _not_ copied.

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
Returns a new _table_ without an entry for _key_.  Original _table_ is not modified.  Returned _table_ may share storage with original _table_.

`table/split-gt` _table_ _pivot_\
Returns a new table with only those entries greater than _pivot_.

`table/split-lt` _table_ _pivot_\
Returns a new table with only those entries less than _pivot_.

`table/union` _left_ _right_\
Returns a new table with all the entries in _left_ and any entries in _right_ that do not already appear in _left_.

`table/union-merge` _left_ _right_ _merge-fn_\
Returns a new table with all the entries in _left_ and all entries in _right_.  Entries in both are passed to _merge-fn_ to determine the value in the result.

### Conversions

`table->alist` _table_\
Return _table_ as an association list.  Resulting alist may share storage with _table_.

`table->hash-table` _table_\
Return _table_ as a hashtable.  Resulting hashtable by share storage with _table_.

`table->plist` _table_\
Return _table_ as a property list.  Resulting plist may share storage with _table_.

### Implementation class changes

`change-class` _table_ _new-table-class_\
Modifies the implementation class for the table instance.  You cannot change a symbol to a new class, however.
