;;; -*- Lisp -*-

(in-package "TABLE")

;;;
;;; NODE
;;;

;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;     2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
;;     2017, 2018, 2019, 2020 Massachusetts Institute of Technology

;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.

;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; Copyright (c) 1993-1994 Stephen Adams

;; This program was written by Stephen Adams, based on the following
;; reference:

;;   Stephen Adams, Implemeting Sets Efficiently in a Functional
;;      Language, CSTR 92-10, Department of Electronics and Computer
;;      Science, University of Southampton, 1992.
;;   <http://groups.csail.mit.edu/mac/users/adams/BB/>

;; The data structure was originally introduced in

;;   J. Nievergelt and E.M. Reingold, `Binary search trees of bounded
;;      balance', Proceedings of the fourth ACM Symposium on Theory of
;;      Computing, pp. 137--142, 1972.

;; The algorithm proposed by Nievergelt & Reingold requires the ratio of
;; sizes of the two subtrees of each node to be bounded by an irrational
;; factor, which is slow to evaluate in integer arithmetic.  Adams
;; proposed a simpler balance condition involvig only a single integer
;; multiplication, but it turns out to be wrong, as explained in

;;    Yoichi Hirai and Kazuhiko Yamamoto, `Balancing weight-balanced
;;      trees', Journal of Functional Programming 21(3), pp. 287--307,
;;      2011.
;;    <https://yoichihirai.com/bst.pdf>

;; We previously used Hirai & Yamamoto's proposed parameters, but there is
;; a much simpler balancing condition, with much simpler analysis, which
;; we adopt here, described in

;;   Salvador Roura, `A new method for balancing binary search trees',
;;      Automata, Languages, and Programming, Orejas, F., Spirakis, P.,
;;      & van Leeuwen, J. (eds), Lecture Notes in Computer Science 2076,
;;      pp. 469--480, Springer, 2001.

(defun make-node (k v l r w)
  (vector w l k r v))

(define-compiler-macro make-node (k v l r w) `(vector (the fixnum ,w) ,l ,k ,r ,v))

(defun node/k (node) (svref node 2))
(defun node/v (node) (svref node 4))
(defun node/l (node) (svref node 1))
(defun node/r (node) (svref node 3))
(defun node/w (node) (svref node 0))

(define-compiler-macro node/k (node) `(svref ,node 2))
(define-compiler-macro node/v (node) `(svref ,node 4))
(define-compiler-macro node/l (node) `(svref ,node 1))
(define-compiler-macro node/r (node) `(svref ,node 3))
(define-compiler-macro node/w (node) `(the fixnum (svref ,node 0)))

(defun call-with-node (node receiver)
  (declare (type (vector * 5) node)
           (optimize (speed 3) (safety 0)))
  (check-type receiver function)
  (funcall receiver (node/k node) (node/v node) (node/l node) (node/r node)))

(define-compiler-macro call-with-node (node receiver)
  (let ((node-var (gensym "NODE-")))
    `(let ((,node-var ,node))
       (declare (type (vector * 5) ,node-var)
                (optimize (speed 3) (safety 0)))
       (,receiver (node/k ,node-var) (node/v ,node-var) (node/l ,node-var) (node/r ,node-var)))))

(defun node/size (node)
  (if (null node)
      0
      (node/w node)))

(define-compiler-macro node/size (node)
  (let ((node-var (gensym "NODE-")))
    `(let ((,node-var ,node))
       (if (null ,node-var)
           0
           (node/w ,node-var)))))

(defun node/weight (node) (node/size node))
(define-compiler-macro node/weight (node) `(node/size ,node))

(defun log2< (a b)
  (and (< a b) (< (ash (logand a b) 1) b)))

(define-compiler-macro log2< (a b)
  (let ((a-var (gensym "A-"))
        (b-var (gensym "B-")))
    `(let ((,a-var ,a)
           (,b-var ,b))
       (declare (type fixnum ,a-var ,b-var)
                (optimize (speed 3) (safety 0)))
       (and (< ,a-var ,b-var)
            (< (the fixnum (ash (the fixnum (logand ,a-var ,b-var)) 1)) ,b-var)))))

(defun overweight? (a b)
  (log2< a (ash b -1)))

(define-compiler-macro overweight? (a b)
  (let ((a-var (gensym "A-"))
        (b-var (gensym "B-")))
    `(let ((,a-var ,a)
           (,b-var ,b))
       (declare (type fixnum ,a-var ,b-var)
                (optimize (speed 3) (safety 0)))
       (log2< ,a-var (the fixnum (ash ,b-var -1))))))

(defun single? (a b)
  (not (log2< b a)))

(define-compiler-macro single? (a b)
  (let ((a-var (gensym "A-"))
        (b-var (gensym "B-")))
    `(let ((,a-var ,a)
           (,b-var ,b))
       (declare (type fixnum ,a-var ,b-var)
                (optimize (speed 3) (safety 0)))
       (not (log2< ,b-var ,a-var)))))

(defun node/singleton (k v) (make-node k v nil nil 1))
(define-compiler-macro node/singleton (k v) `(make-node ,k ,v nil nil 1))

(defun n-join (k v l r)
  (make-node k v l r (+ (node/size l) (node/size r) 1)))

(define-compiler-macro n-join (k v l r)
  (let ((l-var (gensym "L-"))
        (r-var (gensym "R-")))
    `(let ((,l-var ,l)
           (,r-var ,r))
       (make-node ,k ,v ,l-var ,r-var (the fixnum (1+ (the fixnum (+ (node/size ,l-var) (node/size ,r-var)))))))))

(defun single-l (ak av x r)
  (call-with-node r
    (lambda (bk bv y z)
      (n-join bk bv (n-join ak av x y) z))))

(defun double-l (ak av x r)
  (call-with-node r
    (lambda (ck cv rl z)
      (call-with-node rl
        (lambda (bk bv y1 y2)
          (n-join bk bv
                  (n-join ak av x y1)
                  (n-join ck cv y2 z)))))))

(defun single-r (bk bv l z)
  (call-with-node l
    (lambda (ak av x y)
      (n-join ak av x (n-join bk bv y z)))))

(defun double-r (ck cv l z)
  (call-with-node l
    (lambda (ak av x lr)
      (call-with-node lr
        (lambda (bk bv y1 y2)
          (n-join bk bv
                  (n-join ak av x y1)
                  (n-join ck cv y2 z)))))))

(defun t-join (k v l r)
  (let ((lw (node/weight l))
        (rw (node/weight r)))
    (cond ((overweight? lw rw)
           (let ((rlw (node/weight (node/l r)))
                 (rrw (node/weight (node/r r))))
             (if (single? rlw rrw)
                 (single-l k v l r)
                 (double-l k v l r))))
          ((overweight? rw lw)
           (let ((llw (node/weight (node/l l)))
                 (lrw (node/weight (node/r l))))
             (if (single? lrw llw)
                 (single-r k v l r)
                 (double-r k v l r))))
          (t (n-join k v l r)))))

(defun node/maximum (node)
  (cond ((null node)          (error "Empty node."))
        ((null (node/r node)) node)
        (t                      (node/maximum (node/r node)))))

(defun node/minimum (node)
  (cond ((null node)          (error "Empty node."))
        ((null (node/l node)) node)
        (t                      (node/minimum (node/l node)))))

(defun node/remove-maximum (node)
  (cond ((null node)          (error "Empty node."))
        ((null (node/r node)) (node/l node))
        (t                    (t-join (node/k node) (node/v node)
                                      (node/l node) (node/remove-maximum (node/r node))))))

(defun node/remove-minimum (node)
  (cond ((null node)          (error "Empty node."))
        ((null (node/l node)) (node/r node))
        (t                      (t-join (node/k node) (node/v node)
                                        (node/remove-minimum (node/l node)) (node/r node)))))

(defun node/concat2 (node1 node2)
  (cond ((null node1) node2)
        ((null node2) node1)
        (t (let ((min-node (node/minimum node2)))
             (t-join (node/k min-node) (node/v min-node)
                     node1 (node/remove-minimum node2))))))

(defun node/concat (key<? left right)
  (cond ((null left)  right)
        ((null right)  left)
        (t (let ((min-node (node/minimum right)))
	     (node/concat3 key<? (node/k min-node) (node/v min-node) left
			   (node/remove-minimum right))))))

(defun node/concat3 (key<? k v left right)
  (cond ((null left)  (node/add key<? right k v))
        ((null right) (node/add key<? left k v))
        (t
         (let ((w1 (node/weight left))
               (w2 (node/weight right)))
           (cond ((overweight? w1 w2)
                  (call-with-node right
                    (lambda (k2 v2 l2 r2)
                      (t-join k2 v2 (node/concat3 key<? k v left l2) r2))))
                 ((overweight? w2 w1)
                  (call-with-node left
                    (lambda (k1 v1 l1 r1)
                      (t-join k1 v1 l1 (node/concat3 key<? k v r1 right)))))
                 (t
                  (n-join k v left right)))))))
                                    
(defun node/split-lt (key<? node x)
  (cond ((null node)  nil)
	((funcall key<? x (node/k node))
	 (node/split-lt key<? (node/l node) x))
	((funcall key<? (node/k node) x)
	 (node/concat3 key<? (node/k node) (node/v node) (node/l node)
		       (node/split-lt key<? (node/r node) x)))
	(t (node/l node))))

(defun node/split-gt (key<? node x)
  (cond ((null node)  nil)
	((funcall key<? (node/k node) x)
	 (node/split-gt key<? (node/r node) x))
	((funcall key<? x (node/k node))
	 (node/concat3 key<? (node/k node) (node/v node)
		       (node/split-gt key<? (node/l node) x) (node/r node)))
	(t (node/r node))))

(defun node/union (key<? left right)
  (check-type key<? function)
  (labels ((recur (key<? left right)
             (declare (type function key<?))
             (cond ((null left) right)
                   ((null right) left)
                   (t (call-with-node right
                        (lambda (ak av l r)
                          (let ((l1 (node/split-lt key<? left ak))
                                (r1 (node/split-gt key<? left ak)))
                          (node/concat3 key<? ak av (recur key<? l1 l) (recur key<? r1 r)))))))))
    (recur key<? left right)))

(defun node/union-merge (key<? left right merge)
  (check-type key<? function)
  (check-type merge function)
  (labels ((recur (key<? left right merge)
             (declare (type function key<? merge))
             (cond ((null left)    nil)
	           ((null right)  left)
	           (t
	            (call-with-node right
                      (lambda (ak av l r)
                        (let* ((node1  (node/find key<? ak left))
		               (l1     (node/split-lt key<? left ak))
		               (r1     (node/split-gt key<? left ak))
		               (value  (if node1
				           (funcall merge ak av (node/v node1))
				           av)))
		          (node/concat3 key<? ak value
			                (recur key<? l1 l merge)
			                (recur key<? r1 r merge)))))))))
    (recur key<? left right merge)))

(defun node/difference (key<? left right)
  (check-type key<? function)
  (labels ((recur (key<? left right)
             (declare (type function key<?))
             (cond ((null left)   nil)
                   ((null right)   left)
                   (t
                    (call-with-node right
                      (lambda (ak av l r)
                        (declare (ignore av))
                        (let ((l1  (node/split-lt key<? left ak))
                              (r1  (node/split-gt key<? left ak)))
                          (node/concat key<?
                                       (recur key<? l1 l)
                                       (recur key<? r1 r)))))))))
    (recur key<? left right)))

(defun node/add (key<? node k v)
  (check-type key<? function)
  (labels ((recur (key<? node k v)
             (declare (type function key<?))
             (if (null node)
                 (node/singleton k v)
                 (call-with-node node
                   (lambda (key val l r)
                     (cond ((funcall key<? k key)
                            (t-join key val (recur key<? l k v) r))
                           ((funcall key<? key k)
                            (t-join key val l (recur key<? r k v)))
                           (t
                            (n-join key v   l r))))))))
    (recur key<? node k v)))

(defun node/remove (key<? node k)
  (check-type key<? function)
  (labels ((recur (key<? node k)
             (declare (type function key<?))
             (if (null node)
                 nil
                 (call-with-node node
                   (lambda (key val l r)
                     (cond ((funcall key<? k key) (t-join key val (recur key<? l k) r))
                           ((funcall key<? key k) (t-join key val l (recur key<? r k)))
                           (t             (node/concat2 l r))))))))
    (recur key<? node k)))

(defun collect-node (key-series value-series &optional (test #'less))
  (declare (optimizable-series-function))
  (collect-last
   (collecting-fn t
                  (lambda () nil)
                  (lambda (node key value)
                    (node/add test node key value))
                  key-series
                  value-series)))

(defun scan-node (node)
  (declare (optimizable-series-function 2))
  (map-fn '(values t t)
          (lambda (node)
            (values (node/k node) (node/v node)))
          (scan 'list (node/inorder-fold (lambda (list node)
                                           (cons node list))
                                         '()
                                         node))))

(defun node/find (key<? node k)
  (labels ((iter (this best)
             (cond ((null this) best)
                   ((funcall key<? k (node/k this)) (iter (node/l this) best))
                   (t (iter (node/r this) this)))))
    (let ((best (iter node nil)))
      (cond ((not best)     nil)
            ((funcall key<? (node/k best) k) nil)
            (t best)))))

(defun node/inorder-fold (procedure base node)
  (check-type procedure function)
  (labels ((fold (procedure base node)
             (declare (type function procedure))
             (if (null node)
                 base
                 (call-with-node node
                   (lambda (k v l r)
                     (declare (ignore k v))
                     (fold procedure (funcall procedure (fold procedure base r) node) l))))))
    (fold procedure base node)))

(defun node/reverse-order-fold (procedure base node)
  (check-type procedure function)
  (labels ((fold (procedure base node)
             (declare (type function procedure))
             (if (null node)
                 base
                 (call-with-node node
                   (lambda (k v l r)
                     (declare (ignore k v))
                     (fold procedure (funcall procedure (fold procedure base l) node) r))))))
    (fold procedure base node)))

(defun node/keys (node)
  (node/inorder-fold (lambda (keys node)
                       (cons (node/k node) keys))
                     '()
                     node))

(defun node/values (node)
  (node/inorder-fold (lambda (vals node)
                       (cons (node/v node) vals))
                     '()
                     node))

(defun node/intersection (key<? left right)
  (check-type key<? function)
  (labels ((recur (key<? left right)
             (declare (type function key<?))
             (if (or (null left) (null right))
                 nil
                 (call-with-node right
                   (lambda (ak av l r)
                     (let ((l1 (node/split-lt key<? left ak))
                           (r1 (node/split-gt key<? left ak)))
                       (if (node/find key<? left ak)
                           (node/concat3 key<? ak av (recur key<? l1 l)
                                         (recur key<? r1 r))
                           (node/concat key<? (recur key<? l1 l) (recur key<? r1 r)))))))))
    (recur key<? left right)))

(defun node/subset? (key<? left right test)
  "T if all keys in LEFT have mappings in RIGHT."
  (check-type key<? function)
  (check-type test function)
  (labels ((recur (key<? left right test)
             (declare (type function key<? test))
             (or (null left)
                 (and (<= (node/size left) (node/size right))
                      (call-with-node left
                        (lambda (k v l r)
                          (cond ((funcall key<? k (node/k right))
                                 (and (recur key<? l (node/l right) test)
                                      (let ((node (node/find key<? right k)))
                                        (and node (funcall test v (node/v node))))
                                      (recur key<? r right test)))
                                ((funcall key<? (node/k right) k)
                                 (and (recur key<? r (node/r right) test)
                                      (let ((node (node/find key<? right k)))
                                        (and node (funcall test v (node/v node))))
                                      (recur key<? l right test)))
                                (t (and (recur key<? l (node/l right) test)
                                        (recur key<? r (node/r right) test))))))))))
    (recur key<? left right test)))
