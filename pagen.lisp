;; ================================================================
;; 
;;    pagen: Parser Generator Engine for LALR(1)-grammar
;; 
;; ================================================================


(defpackage #:pagen
  (:use #:common-lisp)
  (:import-from #:common-lisp)
  (:export
   ;; Utilities
   #:capp #:==> #:==>* #:dolist-bind
   #:take #:drop #:flip
   #:split
   ;; Struct
   #:make-grammar
   #:rule #:rule-id #:rule-lhs #:rule-rhs #:make-rule
   #:item #:item-pre #:item-suc #:item-lhs
   #:state-gotos #:state-items
   ;; Constructing Parser
   #:lr-parsing-table
   #:lalr-parsing-table
   ;; #:defparser
   #:db-lalr-parsing-table
   )
  )

(in-package #:pagen)


;; 
;;  0. Utility Functions
;; ----------------------------------------------------------------

(defmacro defprinter (name &body body)
  (let* ((sname (symbol-name name))
	 (fname (intern (concatenate 'string "PRINT-" sname)))
	 (pname (intern (concatenate 'string sname "-P"))))
    `(defun ,fname (,name stream depth)
       (if (or (and (numberp *print-level*) (>= depth *print-level*))
	       (,pname ,name))
	   (format stream ,@body)
	   (write ,name :stream stream)))))

(defun take (n l)
  (if (< 0 n) (cons (first l) (take (- n 1) (rest l))) nil))

(defun drop (n l)
  (if (< 0 n) (drop (- n 1) (rest l)) l))

(defun reverse-take (n l)
  (let ((input l)
	(output nil))
    (dotimes (i n output)
      (push (pop input) output))))

(defun set-eq (s1 s2 &key (test #'equal))
  (null (set-exclusive-or s1 s2 :test test)))

(eval-when (compile eval load)
  (setf (symbol-function 'filter) (symbol-function 'remove-if-not))
  )

(declaim (inline undefined-p))
(defun undefined-p (x) (eq :undefined x))

(defun split (pred string &optional (start 0))
  (let ((len (length string))
	(i 0))
    (when (< start len)
      (loop
	while (and (< start len) (funcall pred (char string start)))
	do (incf start))
      (setf i start)
      (when (< i len)
	(loop
	  do (incf i)
	  until (or (>= i len) (funcall pred (char string i))))
	(cons (subseq string start i)
	      (split pred string i))))))

;; 
;; 0.1 Macros
;; --------------------------------

(eval-when (compile eval load)
  (defun capp (f x)
    (if (listp f)
	(append f (list x))
	(list f x)))

  (defmacro ==> (x &body body)
    (if (null body) x
	(destructuring-bind (f . b) body
	  `(==> ,(capp f x) ,@b))))

  (defmacro ==>* (&rest body)
    (let ((sym (gensym)))
      `(lambda (,sym) (==> ,sym ,@body))))

  (defun rep (n)
    (let ((result '()))
      (dotimes (i n result)
	(push i result))))

  (defmacro <@ (&rest f)
    `(lambda (&rest x) (apply ,@f x)))

  (defmacro dolist-bind ((pat exp &optional (ret '())) &body body)
    (let ((sym (gensym)))
      `(dolist (,sym ,exp ,ret)
	 (destructuring-bind ,pat ,sym
	   ,@body))))  )

;; 
;;  1. Definitions of Data Types & Related Functions
;; ----------------------------------------------------------------

;;  1.1 Rule
;; --------------------------------

(defstruct (rule
	    (:constructor make-rule (id lhs rhs))
	    (:print-function print-rule))
  (id 0 :type fixnum)
  (lhs 0 :type fixnum)
  (rhs '() :type list))
;; (defun make-rule (lhs rhs)
;;   (%make-rule -1 lhs rhs))
(defvar empty-rule (make-rule -1 -1 '()))

(defprinter rule
  "~,,2,@a: ~a -> ~{~a~^ ~}"
  (rule-id rule) (rule-lhs rule) (rule-rhs rule))

(declaim (inline rule= rule<))
(defun rule= (r1 r2)
  "We only check id to decide rule's equality."
  (declare (type rule r1 r2))
  (= (rule-id r1) (rule-id r2)))

(defun rule< (r1 r2)
  (declare (type rule r1 r2))
  (< (rule-id r1) (rule-id r2)))


;; 
;;  1.2 Grammar
;; --------------------------------

(defun undefined-array (length)
  (make-array length :initial-element :undefined))

(defstruct (grammar
	    (:conc-name gr-)
	    (:constructor %make-grammar (start rules nt-num t-num
					 %first %head %nullable))
	    (:print-function print-grammar))
  (start 0 :type fixnum)
  (rules '() :type list)
  (nt-num 0 :type fixnum)
  (t-num 0 :type fixnum)
  %first
  %head
  %nullable
  )

(defvar *grammar* nil)
(defun make-grammar (start rules nt-num t-num)
  (let ((t-num+ (+ t-num 2)))
    (%make-grammar start rules nt-num t-num
		   (undefined-array t-num+)
		   (undefined-array t-num+)
		   (undefined-array t-num+))))

(defprinter grammar
  "start: ~a  nt-num: ~a  t-num ~a~%~{~a~^~%~}"
  (gr-start grammar) (gr-nt-num grammar) (gr-t-num grammar) (gr-rules grammar))

;; judgement which non-terminal or terminal
(declaim (inline gr-nt-p gr-t-p))
(defun gr-nt-p (x)
  (declare  (type fixnum x))
  (< x (gr-nt-num *grammar*)))

(defun gr-t-p (x)
  (declare (type fixnum x))
  (not (gr-nt-p x)))


;; 
;;  1.3 Item
;; --------------------------------

(defstruct (item
	    (:include rule)
	    (:constructor %make-item (id lhs rhs pos la))
	    (:print-function print-item))
  (pos 0 :type fixnum)
  (la '() :type list))

(defun make-item (rule pos &optional (la '()))
  (%make-item (rule-id rule) (rule-lhs rule) (rule-rhs rule) pos la))

(defprinter item
  "[~a -> ~{~a~^ ~}.~{~a~^ ~} \| ~{~a~^ ~}]"
  (item-lhs item)
  (take (item-pos item) (item-rhs item))
  (drop (item-pos item) (item-rhs item))
  (item-la item))

(declaim (inline item= item<))
(defun item= (i1 i2)
  (declare (type item i1 i2))
  (and (rule= i1 i2) (= (item-pos i1) (item-pos i2))))
   
(defun item< (i1 i2)
  (declare (type item i1 i2))
  (or (rule< i1 i2)
      (and (rule= i1 i2)
	   (< (item-pos i1) (item-pos i2)))))

(eval-when (compile eval load)
  (declaim (inline item-skeleton item-set-skeleton)))
(defun item-skeleton (item)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (cons (rule-id (the rule item)) (item-pos item)))

(defun item-set-skeleton (l)
  (mapcar #'item-skeleton l))

(defun item-set= (k1 k2)
  ;; (declare (optimize (speed 3) (safety 0) (space 0)))
  (declare (type list k1 k2))
  (or (eq k1 k2)
      (do* ((l1 k1 (cdr l1)) (l2 k2 (cdr l2))
	    (x (car l1) (car l1)) (y (car l2) (car l2)))
	   ((or (eq l1 l2) (null l1) (null l2)) (eq l1 l2))
	(unless (item= x y) (return nil)))))

(defun item-suc-null (item)
  (declare (type item item))
  (= (item-pos item)
     (length (item-rhs item))))

(defun item-next (item)
  (declare (optimize (speed 3) (space 0)))
  (declare (type item item))
  (nth (item-pos item) (item-rhs item)))

(defun shift-item (item)
  (if (< (item-pos item) (length (item-rhs item)))
      (make-item item (+ (item-pos item) 1) (item-la item))
      item))

(defun item-suc (item)
  (drop (item-pos item) (item-rhs item)))

;; 
;;  1.4 State
;; --------------------------------

(defstruct state
  (items '() :type list)
  (gotos '() :type list)
  (comefroms '() :type list)
  (goto-rule-ids '() :type list))


(defun print-state-with (state f)
  (format t "~%")
  (mapcar #'(lambda (item) (print-item-with item f)) (state-items state)))

(defun print-item-with (item f)
  (format t
	   "[~a -> ~{~a~^ ~}.~{~a~^ ~} \| ~{~a~^ ~}]~%"
	   (funcall f (item-lhs item))
	   (mapcar f (take (item-pos item) (item-rhs item)))
	   (mapcar f (drop (item-pos item) (item-rhs item)))
	   (mapcar f (item-la item))))


;; 
;;  2. grammar's properties and functions for computing them
;; ----------------------------------------------------------------

(defun symbol-bv ()
  "make bit-vector indexed by grammar's symbol"
  (make-array (gr-t-num *grammar*) :element-type 'bit :initial-element 0))

;;  2.1 nullability of symbol x
;; --------------------------------

(defun gr-nullable-rule (symbol)
  (filter #'(lambda (rule) (and (= symbol (rule-lhs rule)) (null (rule-rhs rule))))
	  (gr-rules *grammar*)))
				

(defun gr-nullable (symbol &optional (visited (symbol-bv)))
  (let ((nullable (aref (gr-%nullable *grammar*) symbol)))
    (cond ((not (undefined-p nullable)) nullable)
	  ((gr-t-p symbol) nil)
	  ((= 1 (aref visited symbol)) nil)
	  ((gr-nullable-rule symbol) (setf (aref (gr-%nullable *grammar*) symbol) t))
	  (t (let ((nullable nil))
	       (loop
		 while (not nullable)
		 for rule in (gr-rules *grammar*)
		 do (when (= symbol (rule-lhs rule))
		      (if (null (rule-rhs rule))
			  (setf nullable t)
			  (let ((nullable t) (rest (rule-rhs rule)))
			    (setf (aref visited symbol) 1)
			    (loop
			      while nullable
			      for s in rest
			      do (setf nullable (gr-nullable s visited)))))))
	       (setf (aref (gr-%nullable *grammar*) symbol) nullable))))))


(defun gr-seq-nullable (sequence)
  (if (null sequence) t
      (when (gr-nullable (pop sequence))
	(gr-seq-nullable sequence))))


;; 
;;  2.2 head of x : top of a sequence produced from x
;; --------------------------------

(defun gr-head (symbol &optional (visited (symbol-bv)))
  "gr-firts x = { y | x ->* y... }"
  (let ((head (aref (gr-%head *grammar*) symbol)))
    (cond ((not (undefined-p head)) head)
	  ((gr-t-p symbol) (list symbol))
	  ((= 1 (aref visited symbol)) '())
	  (t (setf head (list symbol)
		   (aref visited symbol) 1)
	     (dolist (rule (gr-rules *grammar*))
	       (when (and (= symbol (rule-lhs rule)) (rule-rhs rule))
		 (setf head (union (gr-seq-head (rule-rhs rule) visited) head))))
	     (setf (aref (gr-%head *grammar*) symbol) head)
	     head))))

(defun gr-seq-head (sequence &optional (visited (symbol-bv)))
  (if sequence
      (let ((head (gr-head (first sequence) visited)))
	(if (gr-nullable (first sequence))
	    (union head (gr-seq-head (rest sequence) visited))
	    head))
      '()))

(defun first-symbols (n &optional (visited nil))
  (declare (type fixnum n))
  (cond ((gr-t-p n) (list n))
	((member n visited) nil)
	(t (let ((result (aref (gr-%head *grammar*) n)))
	     (when (eq result :undefined)
	       (setf result (list n))
	       (dolist (rule (gr-rules *grammar*))
		 (when (and (= n (rule-lhs rule)) (rule-rhs rule))
		   (setf result
			 (union
			  (sequence-first-symbols (rule-rhs rule) (cons n visited))
			  result)))))
	     (setf (aref (gr-%head *grammar*) n) result)
	     result))))

(defun sequence-first-symbols (sequence &optional (visited nil))
  (declare (optimize (speed 3) (space 0)))
  (if sequence
      (let ((fs (first-symbols (first sequence) visited)))
	(if (gr-nullable (first sequence))
	    (union fs (sequence-first-symbols (rest sequence) visited))
	    fs))
      ()))

(defun gr-first (symbol)
  (let ((fs (aref (gr-%first *grammar*) symbol)))
    (if (not (undefined-p fs)) fs
	(setf (aref (gr-%first *grammar*) symbol)
	      (filter #'gr-t-p (gr-head symbol))))))

(defun gr-seq-first (sequence)
  (if sequence
      (gr-first (car sequence))
      '()))

;;  3. constructions of LR(0) parsing table
;; ----------------------------------------------------------------

;; 
;; * parsing table is set of states
;; 

(defun make-ht-items ()
  (make-hash-table :test 'equal))

(defmacro gethash-items (key-items hash-table &optional default)
  `(gethash ,(item-set-skeleton key-items) ,hash-table ,default))


(defun closure (items)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((rules (gr-rules *grammar*))
	 (looked-symbols nil))
    (dolist (item items (sort-items items))
      (unless (item-suc-null item)
	(let ((X (item-next item)))
	  (declare (type fixnum X))
	  (when (and (gr-nt-p X)
		     (not (member X looked-symbols)))
	    (let ((fs (first-symbols X)))
	      (dolist (rule rules)
		(when (and (not (member (rule-lhs rule) looked-symbols))
			   (member (rule-lhs rule) fs :test #'=))
		  (push (make-item rule 0) items)))
	      (setf looked-symbols (append fs looked-symbols)))))))))


(declaim (inline item-next-eq))
(defun item-next-eq (symbol item)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (and (not (item-suc-null item))
       (= symbol (item-next item))))

(defun <&> (f g x)
  (cons (funcall f x) (funcall g x)))

(defun goto-item-set (item-set symbol)
  (let ((res (==> item-set
		  (filter (<@ 'item-next-eq symbol)))))
    (cons (==> res (mapcar #'shift-item) closure)
	  (==> res (mapcar #'rule-id)))))

(defun sort-items (items) (sort items #'item<))

;; for making parsing-table
(eval-when (compile eval load)
  (proclaim '(special
	      *state-list*
	      *state-num*
	      *items-ht* *gotos-ht* *goto-rule-ids-ht* *comefroms-ht*)))

(defun flip (f x n)
  (funcall f n x))

(defun lr-parsing-table ()
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((init-items (==> (first (gr-rules *grammar*))
		       (flip #'make-item 0)
		       list closure))
	 (*state-list* (list (cons init-items 0)))
	 (*state-num* 0)
	 (*items-ht* (make-ht-items))
	 (*gotos-ht* (make-ht-items))
	 (*goto-rule-ids-ht* (make-ht-items))
	 (*comefroms-ht* (make-ht-items)))
    (setf (gethash (item-set-skeleton init-items) *items-ht*)
	  0
	  (get '*grammar* 'rhs-heads)
	  (==> (gr-rules *grammar*)
	    (filter #'rule-rhs)
	    (mapcar (==>* rule-rhs first))
	    remove-duplicates))

    (let ((added (list init-items)))
      (loop
	while added
	do (setf added (pt-maker added))))

    (let ((state-array (make-array (+ *state-num* 1))))
      (dolist-bind ((s . n) *state-list* state-array)
	(let ((skeleton (item-set-skeleton s)))
	  (setf (aref state-array n)
		(make-state :items s
			    :gotos (gethash skeleton *gotos-ht*)
			    :comefroms (gethash skeleton *comefroms-ht*)
			    :goto-rule-ids (gethash skeleton *goto-rule-ids-ht*))))))))

(defun pt-maker (item-set)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (reduce #'(lambda (added items)
	      (reduce #'(lambda (l x) (pt-goto items l x))
		      (next-symbols items) :initial-value added))
	  item-set :initial-value '()))

(defun next-symbols (items)
  (==> items
    (remove-if #'item-suc-null)
    (mapcar #'item-next)
    (union (get '*grammar* 'rhs-heads))
    remove-duplicates))

(defun pt-goto (items added x)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (destructuring-bind (goto . goto-rule-ids) (goto-item-set items x)
    (when goto
      (let ((goto-skeleton (item-set-skeleton goto))
	    (items-skeleton (item-set-skeleton items)))
	(multiple-value-bind (next found-p) (gethash goto-skeleton *items-ht*)
	  (unless found-p
	    (incf *state-num*)
	    (setf next *state-num*)
	    (push (cons goto next) *state-list*)
	    (setf (gethash goto-skeleton *items-ht*) next)
	    (push goto added))
	  (pushnew (cons x next)
		   (gethash items-skeleton *gotos-ht*) :test #'equal)
	  (pushnew (cons x goto-rule-ids)
		   (gethash items-skeleton *goto-rule-ids-ht*) :test #'equal)
	  (pushnew (cons (gethash items-skeleton *items-ht*) x)
		   (gethash goto-skeleton *comefroms-ht*) :test #'equal))))
    added))

;; 
;;  4. DeRemer's method
;; --------------------------------


  
;; Direct-Read set
(defun direct-read-set (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "t in DR(p,A) :<=> p -A-> r -t-> (t is terminal)"
  (let ((dr-set (make-hash-table :test #'equal)))
    (dotimes (i (length state-array) dr-set)
      (dolist-bind ((X . next) (state-gotos (aref state-array i)))
	(setf (gethash (cons i X) dr-set)
	      (==> (aref state-array next)
		state-gotos (mapcar #'car) (filter #'gr-t-p)))))))

;; reads: relations on transitions
(defun reads-rel (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "(p,A) reads (r,C) :<=> p -A-> r -C-> /\ C =>* ()"
  (let ((rel (make-hash-table :test #'equal)))
    (dotimes (i (length state-array) rel)
      (dolist-bind ((X . next) (state-gotos (aref state-array i)))
	(setf (gethash (cons i X) rel)
	      (==> (aref state-array next)
		state-gotos (mapcar #'car) (filter #'gr-nullable)
		(mapcar (<@ #'cons next))))))))


;; 
;; algorithm digraph

(eval-when (compile eval load)
  (proclaim '(special
	      *alg-base* *alg-rel* *alg-stack* *alg-depth* *alg-nar*)))

(defun alg-digraph (rel base-ht node-list)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((*alg-base* base-ht)
	 (*alg-rel* rel)
	 (*alg-stack* nil)
	 (*alg-depth* 0)
	 (*alg-nar* (make-hash-table :test #'equal)))
    (reduce
     #'(lambda (result node)
	 (if (= 0 (gethash node *alg-nar* 0)) (traverse node result) result))
     node-list :initial-value (make-hash-table :test #'equal))))

(defun traverse (x result-ht)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (push x *alg-stack*)
  (incf *alg-depth*)
  (setf (gethash x *alg-nar*) *alg-depth*
	(gethash x result-ht) (gethash x *alg-base* nil))
  (dolist (y (gethash x *alg-rel* nil))
    (when (= 0 (gethash y *alg-nar* 0))
      (setf result-ht (traverse y result-ht)))
    (setf (gethash x *alg-nar*)
	  (min (gethash x *alg-nar* 0) (gethash y *alg-nar* 0))
	  (gethash x result-ht)
	  (union (gethash x result-ht nil)
		 (gethash y result-ht nil) :test #'equal)))
  (when (= *alg-depth* (gethash x *alg-nar* 0))
    (loop
      do (setf (gethash (first *alg-stack*) *alg-nar*) most-positive-fixnum)
	 (unless (equal (first *alg-stack*) x)
	   (setf (gethash (first *alg-stack*) result-ht)
		 (gethash x result-ht nil)))
      until (equal x (pop *alg-stack*))))
  result-ht)

(defun bounds (from to)
  (if (< from to) (cons from (bounds (+ from 1) to)) '()))

(defun calc-transitions (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (reduce
   #'(lambda (trns i)
       (append
	trns
	(mapcar (==>* car (cons i)) (state-gotos (aref state-array i)))))
   (bounds 0 (length state-array)) :initial-value '()))
    
;; read set
(defun read-set (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "Read(p, A) := DR(p, A) U U{ Read(r, C) | (p, A) reads (r, C) }"
  (destructuring-bind (rel . base) (<&> #'reads-rel #'direct-read-set state-array)
    (==> (calc-transitions state-array)
      (filter (==>* cdr gr-nt-p)) (alg-digraph rel base))))

(defun rev-traverse (state-array starts sequence)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (reduce
   #'(lambda (s x)
		 (==> s
		   (mapcar (==>* (aref state-array) state-comefroms
				 (filter (==>* cdr (= x))) (mapcar #'car)))
		   (apply #'append)
		   ))
   sequence :initial-value starts))

(defun includes-rel (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  "(p, A) includes (p', B) :<=> B -> bAg /\ g =>* () /\ p' -- b --> p"
  (let ((result (make-hash-table :test #'equal))
	(nt-transitions (filter (==>* cdr gr-nt-p) (calc-transitions state-array))))
    (dolist-bind ((i . X) nt-transitions result)
      (dolist (rule (gr-rules *grammar*))
	(let ((rhs (rule-rhs rule)) (pre nil))
	  (loop
	    while rhs
	    do (when (and (= (first rhs) X) (gr-seq-nullable (rest rhs)))
		 (setf (gethash (cons i X) result)
		       (union
			(gethash (cons i X) result nil)
			(intersection
			 nt-transitions
			 (==> pre
			   (rev-traverse state-array (list i))
			   (mapcar #'(lambda (x) (cons x (rule-lhs rule)))))
			 :test #'equal))))
	       (push (pop rhs) pre)))))))
		
(defun follow-set (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (destructuring-bind (rel . base) (<&> #'includes-rel #'read-set state-array)
    (==> (calc-transitions state-array)
      (filter (==>* cdr gr-nt-p)) (alg-digraph rel base))))

(defun lookback-rel (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((result (make-hash-table :test #'equal)))
    (dotimes (i (length state-array) result)
      (==> (state-items (aref state-array i))
	(filter #'item-suc-null)
	(mapcar
	 #'(lambda (item)
	     (==> (item-rhs item)
	       (reverse-take (item-pos item))
	       (rev-traverse state-array (list i))
	       (mapcar #'(lambda (p) (cons p (item-lhs item))))
	       (setf (gethash (cons i (rule-id item)) result)))))))))


(defun gethash-from (ht def x) (gethash x ht def))

(defun lookahead-set (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((result (make-hash-table :test #'equal))
	(follow (follow-set state-array))
	(lookback (lookback-rel state-array)))
    (dotimes (i (length state-array) result)
      (==> (state-items (aref state-array i))
	(filter #'item-suc-null)
	(mapcar 
	 #'(lambda (item)
	     (setf (gethash (cons i (rule-id item)) result)
		   (remove-duplicates
		    (==> (gethash (cons i (rule-id item)) lookback)
		      (mapcar #'(lambda (x) (gethash x follow nil)))
		      (apply #'append))
		    :test #'equal))))))))

;; lalr1
(defun lalr-parsing-table (state-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((lookahead (lookahead-set state-array)))
    (dotimes (i (length state-array) state-array)
      (==> (state-items (aref state-array i))
	(filter #'item-suc-null)
	(mapcar
	 #'(lambda (item)
	     (setf (item-la item)
		   (gethash (cons i (rule-id item)) lookahead))))))))



;; 
;; 5. Action Table
;; --------------------------------

(defstruct (shift-action
	    (:constructor make-shift-action (num)))
  (num 0 :type fixnum))

(defstruct (reduce-action
	    (:constructor make-reduce-action (rule)))
  (rule empty-rule :type rule))

(defstruct accept-action)

(eval-when (compile eval load)
  (proclaim '(special
	      *at-state-array* *at-state-num*)))

(defun make-action-table (state-array &optional (rp-sp ()))
  (let* ((*at-state-array* state-array)
	 (*at-state-num* (length *at-state-array*)))
    (==> (make-array *at-state-num* :initial-element nil)
      set-shift-action
      set-accept-action
      set-reduce-action
      (conflict-solver rp-sp)
      )))

(defun set-shift-action (action-array)
  (dotimes (i *at-state-num* action-array)
    (setf (aref action-array i)
	  (==> (state-gotos (aref *at-state-array* i))
	    (mapcar #'(lambda (p) (list (car p) (make-shift-action (cdr p)))))))))

(defun set-accept-action (action-array)
  (if (assoc 0 (aref action-array 0))
      (pushnew (make-accept-action) (cdr (assoc 0 (aref action-array 0))))
      (push (list 0 (make-accept-action)) (aref action-array 0)))
  action-array)

(defun set-reduce-action (action-array)
  ;; Reduce Action
  (dotimes (i *at-state-num* action-array)
    (==> (state-items (aref *at-state-array* i))
      (filter #'item-suc-null)
      (mapcar
       #'(lambda (item)
	   (let ((red (make-reduce-action (the rule item))))
	     (if (null (item-la item))
		 (pushnew (list -1 red) (aref action-array i))
		 (dolist (la (item-la item))
		   (if (null (assoc la (aref action-array i)))
		       (push (list la red) (aref action-array i))
		       (push red (cdr (assoc la (aref action-array i)))))))))))))

;; TODO: Refactoring
(defun conflict-solver (rp-sp action-array)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((conflicts nil))
    (labels ((rule-rp (id)
	       (if (null rp-sp) 0
		   (aref (car rp-sp) id)))
	     (rule-sp (id)
	       (if (null rp-sp) 0
		   (aref (cdr rp-sp) id))))
      ;; correcting...
      (dotimes (i *at-state-num* action-array)
	(dolist-bind ((la . actions) (aref action-array i))
	  (when (< 1 (length actions))
	    (let ((shift-ps (==> (state-goto-rule-ids (aref *at-state-array* i))
		  	      (assoc la) cdr (mapcar #'rule-sp)))
		  (ps 0))
	      (when shift-ps (setf ps (apply #'max shift-ps)))
	      
	      (setf actions
		    (sort 
		     (mapcar
		      #'(lambda (action)
			  (cond ((reduce-action-p action)
				 (let ((rp (rule-rp (rule-id (reduce-action-rule action)))))
				   (cons rp action)))
				((shift-action-p action)
				 (cons ps action))
				(t (cons 0 action))))
		      actions)
		     #'< :key #'car))
	      (let ((max (apply #'max (mapcar #'car actions))))
		(setf (cdr (assoc la (aref action-array i)))
		      (mapcar #'cdr (filter (==>* car (= max)) actions)))))))
	;; collecting...
	(dolist-bind ((la . actions) (aref action-array i))
	  (when (< 1 (length actions))
	    (let ((conflict-info nil))
	      (dolist (action actions)
		(cond ((reduce-action-p action)
		       (push (cons :reduce-action (reduce-action-rule action))
			     conflict-info))
		      ((shift-action-p action)
		       (push
			(cons :shift-action
			      (cons 
			       (==> (aref *at-state-array* i)
				 state-goto-rule-ids (assoc la) cdr)
			       (shift-action-num action)))
			     conflict-info))
		      (t nil)))
	      (push (cons (cons i la) conflict-info) conflicts)

	      )))))
    (values action-array conflicts)))


;; 
;; 6. Parser
;; --------------------------------

(defstruct (configuration
	    (:conc-name conf-)
	    (:constructor make-conf (states tokens reader)))
  (states nil :type list)
  (tokens nil :type list)
  (reader () :type function))

(defun conf-first-token (conf)
  (when (null (conf-tokens conf))
    (let ((tv (funcall (conf-reader conf))))
      (push (if (consp tv) tv tv) (conf-tokens conf))))
  (first (conf-tokens conf)))


(eval-when (compile eval load)
  (proclaim '(special
	      *symbol-stack* *val-stack*)))

(defun parse (gr reader action-array &key (f-array nil) (dump nil) (symbol-printer #'identity))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((*grammar* gr)
	(conf (make-conf (list 0) '() reader))
	(*symbol-stack* nil)
	(*val-stack* nil))
    (let ((msg (parse-body conf action-array f-array dump symbol-printer)))
      (if msg (format t "~a" msg)
	  (progn
	    (format t "accept.")
	    (first *val-stack*)))
      )))

(defun conf-reader-printer (conf symbol-printer)
  (apply (conf-reader conf) symbol-printer))

(defun parse-body (conf action-array f-array dump symbol-printer)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((state (first (conf-states conf)))
	 (tv (or (conf-first-token conf) -1))
	 (symbol (if (consp tv) (car tv) tv))
	 (actions (cdr (assoc symbol (aref action-array state) :test #'=))))
    (when dump
      (format
       t " [~3,,,' @a: ~{~a~^ ~} :> ~{~a~^ ~} .. ~{~a~^ ~} ]~%"
       state
       (reverse (mapcar (==>* (funcall symbol-printer)) *symbol-stack*))
       (mapcar #'(lambda (x)
		   (when x 
		     (if (consp x)
			 (funcall symbol-printer (car x))
			 (funcall symbol-printer x)))) (conf-tokens conf))
       (funcall (conf-reader conf) symbol-printer)
       ))
    (cond ((null actions) "error: no action.~%")

	  ((< 1 (length actions))
	   ;; TODO: Use condition system
	   (cons "error: conflict.~%" actions))
	  
	  ((shift-action-p (first actions))
	   (let* ((tv (pop (conf-tokens conf)))
		  (x (if (consp tv) (car tv) tv)))
	     (when (gr-t-p x)
	       (push
		(if (consp tv) (cdr tv)
		    (funcall symbol-printer x))
		*val-stack*))
	     (push x *symbol-stack*))
	   (push (shift-action-num (first actions)) (conf-states conf))
	   ;; rec
	   (parse-body conf action-array f-array dump symbol-printer))
	  
	  ((reduce-action-p (first actions))
	   (let* ((rule (reduce-action-rule (first actions))))
	     (dotimes (i (length (rule-rhs rule)))
	       (pop *symbol-stack*)
	       (pop (conf-states conf)))
	     (when f-array
	       (let ((vals nil))
		 (dotimes (i (length (rule-rhs rule)))
		   (push (pop *val-stack*) vals))
		 (push (apply (aref f-array (rule-id rule)) vals) *val-stack*)))
	     (push (rule-lhs rule) (conf-tokens conf)))
	   ;; rec
	   (parse-body conf action-array f-array dump symbol-printer))
	  
	  ((accept-action-p (first actions)) nil)
	  
	  (t "error: invalid action.~%"))))


(defun print-conflict (conflict rule-arr &optional (sp #'identity))
  (destructuring-bind ((state . la) . info-list) conflict
    (format t "in STATE [~a] looking [~a]:~%" state (funcall sp la))
    (dolist (info info-list)
      (print-info info rule-arr sp))
    ;; (format t "  ~a~%  ~a~%" (car info1) (car info2))
    (format t "~%")
    ))

(defun print-info (info rule-arr &optional (sp #'identity))
  (let ((type (car info)))
    (cond ((eq type :shift-action)
	   (format t "* SHIFT :: shift to state [~a]~%~{    ~a~%~}"
		   (cddr info)
		   (==> (cadr info)
		     (mapcar (<@ 'aref rule-arr))
		     (mapcar #'(lambda (rule)
				 (format nil "~a -> ~{~a~^ ~}"
					 (funcall sp (rule-lhs rule))
					 (mapcar sp (rule-rhs rule)))))
		     )))
	  ((eq type :reduce-action)
	   (let ((rule (cdr info)))
	     (format t "* REDUCE ::~%    ~a -> ~{~a~^ ~}~%"
		     (funcall sp (rule-lhs rule))
		     (mapcar sp (rule-rhs rule)))))

	  (t (format t "~a~%" (equal 'shift (car info)))))))


;; for test
(defun def-defparser (name grammar lex f-arr rp-sp sp)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (multiple-value-bind (action-array conflict-info)
      (make-action-table
       (let ((*grammar* grammar))
	 (lalr-parsing-table (lr-parsing-table)))
       rp-sp)
    ;; if conflicts exist, display details of them
    (when conflict-info
      (let* ((rules (gr-rules grammar))
	     (rule-arr (make-array (length rules))))
	(dolist (rule rules)
	  (setf (aref rule-arr (rule-id rule)) rule))
	(dolist (conflict conflict-info)
	  (print-conflict conflict rule-arr sp))))
    `(defun ,name (string &key (dump nil))
       (declare (optimize (speed 3) (safety 0) (space 0)))
       (parse ,grammar (funcall ,lex string) ,action-array
	      :f-array ,f-arr :dump dump :symbol-printer ,sp))))


;; (defmacro defparser (name grammar &optional (f-array ()) (rp-array ()) (sp-array ()) (sym-arr ()))
;;   (let* ((*grammar* grammar)
;; 	 (rule-arr (make-array (length (gr-rules *grammar*)))))
;;     (multiple-value-bind (action-array conflict-info)
;; 	(make-action-table (db-lalr-parsing-table (lr-parsing-table)) rp-array sp-array)
;;       (when conflict-info
;; 	(dolist (rule (gr-rules *grammar*))
;; 	  (setf (aref rule-arr (rule-id rule)) rule))
;; 	(dolist (conflict conflict-info)
;; 	  (print-conflict conflict rule-arr (<@ 'aref sym-arr))))
;;       `(defun ,name (reader &key (dump nil) (sp #'identity))
;; 	 (parse ,grammar reader ,action-array
;; 		:f-array ,f-array :dump dump :symbol-printer sp)))))

      
(defun simple-reader (l &key eof)
  (let ((input (append l (list eof))))
    #'(lambda (&optional (sp nil))
	(if sp (mapcar #'(lambda (s) (funcall sp (if (consp s) (car s) s))) input)
	    (if (null input) nil (pop input))))))


;; 
;; Disambiguator
;; --------------------------------

(defun resolve-lr (gr mode symbol result)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (or (eq mode :left) (eq mode :right)) ; TODO: care
      (let ((lg (if (eq mode :left) :lt :gt)))
	(dolist (rule (gr-rules gr) result)
	  (let ((id (rule-id rule))
		(rhs (rule-rhs rule)))
	    (when (and (= 3 (length rhs)) (= symbol (second rhs)))
	      (push (list lg (cons :reduce id) (cons :shift id)) result)))))
      (progn
	(format t "Invalid mode for operation.~%")
	result)))

;; TODO: 単項演算子についても強さを指定できるようにしよう。
;; 例えば単項の − や negation

(defun resolve-order (gr mode symbol-list result)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (or (eq mode :ord<) (eq mode :ord>)) ; TODO: care
      (let ((lg (if (eq mode :ord<) :lt :gt))
	    (rule-array (make-array (gr-t-num gr))))
	(dolist (rule (gr-rules gr))
	  (let ((id (rule-id rule))
		(rhs (rule-rhs rule)))
	    (when (and (= 3 (length rhs))
		       (member (second rhs) symbol-list))
	      (setf (aref rule-array (second rhs)) id))))
	(append
	 (apply #'append
		(maplist #'(lambda (l)
			     (if (<= 2 (length l))
				 (let ((w (first l))
				       (s (second l)))
				   (list (list lg (cons :reduce w) (cons :shift s))
					 (list lg (cons :shift w) (cons :reduce s))))
				 nil))
			 (mapcar #'(lambda (x) (aref rule-array x)) symbol-list)))
	 result))
      (progn
	(format t "Invalid mode for operation.~%")
	result)))

(defun resolve-sr (gr mode symbol result)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (or (eq mode :shift) (eq mode :reduce)) ; TODO: care
      (let ((lg (if (eq mode :shift) :lt :gt))
	    (rel-rules (filter (==>* rule-rhs (member symbol)) (gr-rules gr))))
	(dolist (rule rel-rules result)
	  (let* ((pos (position symbol (rule-rhs rule)))
		 (head (subseq (rule-rhs rule) 0 pos))
		 (rels (filter (==>* rule-rhs (equal head)) (gr-rules gr))))
	    (dolist (r rels)
	      (push (list lg (cons :reduce (rule-id r)) (cons :shift (rule-id rule))) result)))))
      (progn
	(format t "Invalid mode for operation.~%")
	result)))

(defun resolver (gr result info-list)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (reduce
   #'(lambda (result info)
       (if (consp info)
	   (destructuring-bind (mode . symbols) info
	     (cond ((or (eq mode :left) (eq mode :right))
		    (reduce #'(lambda (res symbol) (resolve-lr gr mode symbol res))
			    symbols :initial-value result))
		   ((or (eq mode :shift) (eq mode :reduce))
		    (reduce #'(lambda (res symbol) (resolve-sr gr mode symbol res))
			    symbols :initial-value result))
		   ((or (eq mode :ord<) (eq mode :ord>))
		    (resolve-order gr mode symbols result))
		   (t result)))
	   result))
   info-list :initial-value result))


(defun disambiguator (gr info-list)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((edge-base (==> info-list
		      (resolver gr nil)
		      (mapcar #'(lambda (x)
				  (cond ((eq :lt (first x)) (list (second x) (third x)))
					((eq :gt (first x)) (list (third x) (second x)))
					(t nil))))))
	 (vertices (remove-duplicates (apply #'append edge-base) :test #'equal))
	 (edges (mapcar #'(lambda (x) (when x (cons (first x) (second x)))) edge-base))
	 (rp-array (make-array (length (gr-rules gr)) :initial-element 0))
	 (sp-array (make-array (length (gr-rules gr)) :initial-element 0))
	 (num 0))
    (dolist (v (sort (cdr (dfs vertices edges)) #'> :key #'cdr))
      (destructuring-bind (type . id) (car v)
	(if (eq type :reduce)
	    (setf (aref rp-array id) num)
	    (setf (aref sp-array id) num))
	(incf num)))
    (cons rp-array sp-array)))



;; TODO: Topological sort for acyclic digraph
;; we can use the linear order given by this sort to decide reduce/shift-time priorities 

(defun dfs (vertices edges)
  (let* ((color (make-hash-table :test 'equal))
	 (p (make-hash-table :test 'equal))
	 (d (make-hash-table :test 'equal))
	 (f (make-hash-table :test 'equal))
	 (time 0))
    (labels
	((dfs-visit (u)
	   (setf (gethash u color) :gray
		 (gethash u d) time)
	   (incf time)
	   (dolist (v (mapcar #'cdr (filter (==>* car (equal u)) edges)))
	     (when (eq :white (gethash v color :white))
	       (setf (gethash v p) u)
	       (dfs-visit v)))
	   (setf (gethash u color) :black
		 (gethash u f) time)
	   (incf time)))
      (dolist (v vertices)
	(when (eq :white (gethash v color :white))
	(dfs-visit v)))
      (let ((dl nil)
	    (fl nil))
	(dolist (v vertices)
	  (push (cons v (gethash v d)) dl)
	  (push (cons v (gethash v f)) fl))
	(cons dl fl)))))
		  



;; 
;;  7. Printer
;; --------------------------------

(defun pt-printer (state-array &key (stream t) (printer #'identity) (simple nil))
  (format stream "digraph pt {~%")
  (dotimes (i (length state-array))
    (let ((state (aref state-array i))
	  (esprinter #'(lambda (x)
			 (==> (funcall printer x)
			   (replacer #\< "\\<") (replacer #\> "\\>")))))
      (format stream "  ~a [shape=record, label=\"{~a|{" i i)
      (dolist (item (state-items state))
	(when (or (not simple) (item-la item))
	  (format stream "[~a\\ -\\>\\ ~{~a~^\\ ~}\\ .\\ ~{~a~^\\ ~}\\ \\|\\ ~{~a~^\\ ~}]\\ \\l"
		  (funcall esprinter (item-lhs item))
		  (mapcar esprinter (take (item-pos item) (item-rhs item)))
		  (mapcar esprinter (drop (item-pos item) (item-rhs item)))
		  (mapcar esprinter (item-la item)))))
      (format stream "}}\"];~%")
      (dolist-bind ((symbol . next) (state-gotos state))
	(format stream "  ~a -> ~a [label=\"~a\",weight=~a,rank=~a];~%" i next (funcall esprinter symbol) (* 10 (+ 1 (length (state-comefroms state)))) (length (state-comefroms state))))))
  (format stream "}~%"))

(defun replacer (char str base &optional (start 0))
  (let ((pos (position char base :start start)))
    (if pos
	(concatenate 'string
		     (subseq base start pos)
		     str
		     (replacer char str base (+ pos 1)))
	(subseq base start))))

			  

;; 
;;  8. Aho's Algorithm (from Dragon book)
;; --------------------------------

(defun items->kernel (item-set)
  (filter #'(lambda (item) (or (< 0 (item-pos item)) (= 0 (rule-id item)))) item-set))


(defun lr1-closure (lr1-items )
  (let ((result lr1-items))
    (labels ((f (items)
	       (when items 
		 (dolist (item items)
		   (unless (or (item-suc-null item) (gr-t-p (item-next item)))
		     (dolist (rule (gr-rules *grammar*))
		       (when (= (item-next item) (rule-lhs rule))
			 (let* ((new-lr1-items (mapcar #'(lambda (x) (make-item rule 0 (list x)))
						       (gr-seq-first (append (rest (item-suc item)) (item-la item)))))
				(added nil))
			   (setf added (set-difference new-lr1-items result :test #'lr1-item=))
			   (setf result (append added result))
			   ;; (format t  "~a:~a~%" result added)
			   (f added)))))))))
      (f lr1-items))
    result))

;; (defun lr1-closure (item-set)
;;   (lr1-closure-aux item-set item-set))

;; (defun lr1-closure-aux (acc item-set)
;;   (when item-set
;;     (let ((added nil))
;;       (dolist (item item-set acc)
;; 	(format t "[~a]" (item-la item))
;; 	(unless (or (item-suc-null item) (gr-t-p (item-next item)))
;; 	  (dolist (rule (gr-rules *grammar*))
;; 	    (when (= (item-next item) (rule-lhs rule))
;; 	      (let ((new-items
;; 		      (mapcar #'(lambda (x) (make-item rule 0 (list x)))
;; 			      (gr-seq-first (append (rest (drop (item-pos item) (item-rhs item))) (item-la item))))))
;; 		(setf added (union new-items added :test #'item=)))))))
;;       (setf added (set-difference added acc :test #'item=))
;;       (if added (lr1-closure-aux (append added acc) added) acc))))


(defun include-p (l1 l2)
  (every #'(lambda (x1) (member x1 l2)) l1))

(defun perm-equal (l1 l2)
  (and (= (length l1) (length l2)) (include-p l1 l2) (include-p l2 l1)))

(defun lr1-item= (i1 i2)
  (declare (type item i1 i2))
  (and (rule= i1 i2)
       (= (item-pos i1) (item-pos i2))
       (perm-equal (item-la i1) (item-la i2)) ;; so slow
       ))

(defun db-lalr-parsing-table (state-array)
  (let* ((num (length state-array))
	 (propagate-symbol (+ (gr-t-num *grammar*) 1))
	 (propagate-array (make-array num))
	 (la-set-array (make-array num))
	 (changed t))
    (dotimes (i num)
      (setf (aref propagate-array i) (make-hash-table :test 'equal)
	    (aref la-set-array i) (make-hash-table :test 'equal)))
    ;; Determine Look-Ahead Propagation
    (dotimes (i num)
      ;; each item in kernel of state I
      (dolist (k-item (items->kernel (state-items (aref state-array i))))
	;; all GOTO(I,X)
	(let ((gotos (state-gotos (aref state-array i))))
	  ;; calculate propagation for all item B -> a.Xb s.t. X is non-terminal
	  (dolist (item (lr1-closure (list (make-item (the rule k-item) (item-pos k-item) (list propagate-symbol)))))
	    (when (and (not (item-suc-null item))
		       (assoc (item-next item) gotos))
	      (let ((propagate-ht (aref propagate-array (cdr (assoc (item-next item) gotos))))
		    (shifted-item (shift-item item)))
		(if (member propagate-symbol (item-la item))
		     (push (cons i k-item) (gethash (item-skeleton shifted-item) propagate-ht))	; lookahead propagate from item in state i
		    (push (car (item-la item)) (gethash (item-skeleton shifted-item) propagate-ht))) ; lookahead is generated spontaneously
		 ))))))
    ;; Propagation
    (loop
      while changed
      do (setf changed nil)
    	 (dotimes (i num)
    	   (let ((propagate-ht (aref propagate-array i)))
    	     ;; for item in state i
    	     (dolist (item (state-items (aref state-array i)))
    	       (let ((la-set-ht (aref la-set-array i)))
    		 (dolist (x (gethash (item-skeleton item) propagate-ht nil))
    		   (cond ((numberp x)
    			  (unless (member x (gethash (item-skeleton item) la-set-ht nil))
    			    (push x (gethash (item-skeleton item) la-set-ht))
    			    (setf changed t)))
    			 ((consp x)
    			  (let ((dif (set-difference (gethash (item-skeleton (cdr x)) (aref la-set-array (car x)))
    						     (gethash (item-skeleton item) la-set-ht nil))))
    			    (when dif
    			      (setf (gethash (item-skeleton item) la-set-ht)
				    (append dif (gethash (item-skeleton item) la-set-ht nil))
    				    changed t))))
    			 (t nil))))))))
    (dotimes (i num)
      (setf (state-items (aref state-array i))
	    (sort 
	     (lalr1-closure
	      (mapcar
	       #'(lambda (item)
		   (setf (item-la item)
			 (gethash (item-skeleton item) (aref la-set-array i)))
		   item)
	       (items->kernel (state-items (aref state-array i)))))
	     #'item<)
	    ))
    state-array))

(defun lalr1-closure (items)
  (declare (optimize (speed 3) (space 0)))
  (let ((rules (gr-rules *grammar*)))
    (dolist (item items)
	(unless (item-suc-null item)
	  (let ((X (item-next item)))
	    (when (and (numberp X) (gr-nt-p X))
	      (let ((fs (gr-head X)))
		(dolist (rule rules)
		  (when (member (rule-lhs rule) fs)
		    (pushnew (make-item rule 0 (item-la item)) items :test #'item=))))))))
    items))
