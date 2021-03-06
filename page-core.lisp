;; ================================ ;;
;;  PaGe-Core:
;;   [canonicalized grammar]
;;       -->  [lalr1 parser]
;; ================================ ;;

(defpackage #:page
  (:use #:common-lisp)
  (:import-from #:common-lisp)
  (:export #:deremer-lalr1-parser #:parse #:make-parser #:make-action-array))

(in-package #:page)


;; --------------------------------
;;  Utility
;;
 
(setf (symbol-function 'filter) (symbol-function 'remove-if-not))
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


;; --------------------------------
;;  rule
;; 
(defstruct (rule
	    (:constructor make-rule (lhs rhs &key (id 0) )))
  (id 0 :type fixnum) (lhs 0 :type fixnum) rhs )

(declaim (inline rule=))
(defun rule= (r1 r2)
  (declare (type rule r1 r2))
  (eq r1 r2))
;  (= (rule-id r1) (rule-id r2)))

(defun rule< (r1 r2)
  (declare (type rule r1 r2))
  (< (rule-id r1) (rule-id r2)))
(declaim (inline rule<))



;; --------------------------------
;; grammar represented by numbers (and symbol-number table)
;; 
(defstruct (canonical-grammar
	    (:conc-name cg-)
	    (:constructor make-canonical-grammar-base
	      (start rules nt-num num)))
  start rules nt-num num
  )

(defparameter *current-cg* :undefined)
(defparameter *cg-nullable-array* :undefined)
(defparameter *cg-first-array* :undefined)
(defparameter *cg-follow-array* :undefined)
(defparameter *cg-first-symbols-array* :undefined)

(defun make-canonical-grammar (start rules nt-num num)
  (setf *cg-nullable-array* :undefined
	*cg-first-array* :undefined
	*cg-follow-array* :undefined
	*cg-first-symbols-array* :undefined)
  (setf *current-cg* (make-canonical-grammar-base start rules nt-num num)))
  

(declaim (inline (undefined-p)))
(defun undefined-p (x) (eq :undefined x))

(defun cg-nullable (n)
  ;; (declare (canonical-grammar cg))
  (when (undefined-p *cg-nullable-array*) (calc-nullable))
  (= 1 (aref *cg-nullable-array* n)))

(defun cg-first (n)
  ;; (declare (canonical-grammar cg))
  (when (undefined-p *cg-first-array*) (calc-first *current-cg*))
  (if (< n (cg-num *current-cg*))
      (aref *cg-first-array* n)
      (list n)))

(defun cg-follow (n)
  ;; (declare (canonical-grammar cg))
  (when (undefined-p *cg-follow-array*) (calc-follow *current-cg*))
  (aref *cg-follow-array* n))

(declaim (inline (cg-non-terminal-p cg-terminal-p)))
(defun cg-non-terminal-p (num)
  ;; (declare (canonical-grammar cg))
  (< num (cg-nt-num *current-cg*)))

(defun cg-terminal-p (num)
  (<= (cg-nt-num *current-cg*) num))


(defun terminal-p (num)
  (<= (cg-nt-num *current-cg*) num))


;; --------------------------------
;;  memoize
;; 

(defun memo (fn)
  "Return a memoized-function of fn"
  (let ((memo-table (make-hash-table)))
    #'(lambda (&rest args)
	(multiple-value-bind (val found-p) (gethash args memo-table)
	  (if found-p val
	      (setf (gethash args memo-table) (apply fn args)))))))

(defun memoize (fn-name)
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))
	
(defmacro defun-memo (fn args &body body)
  `(memoize (defun ,fn ,args . , body)))



;; nullability-checker
(defun calc-nullable ()
  (let ((nullable (make-array (+ (cg-num *current-cg*) 1) :element-type 'bit :initial-element 0))
n	(changed t))
    ;; initialize
    (loop while changed
	  do (setf changed nil)
	     (dolist (rule (cg-rules *current-cg*))
	       (when (or (null (rule-rhs rule)) ; X -> 
			 (every #'(lambda (n) (= 1 (aref nullable n))) (rule-rhs rule))) ; X -> Y_1 .. Y_n s.t. all Y_i is nullable.
		 ;; only false -> true
		 (when (= 0 (aref nullable (rule-lhs rule)))
		   (setf (aref nullable (rule-lhs rule)) 1
			 changed t)))))
    (setf *cg-nullable-array* nullable)))

(defun cg-sequence-nullable (sequence)
  (let ((b t))
    (loop while b
	  for s in sequence
	  do (setf b (cg-nullable s)))
    b))

;; 
(defun calc-first (cg)
  (let ((firsts (make-array (+ (cg-num cg) 1) :initial-element '()))
	(changed t))
    (loop for n from (cg-nt-num cg) to (cg-num cg)
	  do (setf (aref firsts n) (list n)))
    (loop while changed
	  do (setf changed nil)
	     (dotimes (X (cg-nt-num cg))
	       (let ((old-x-firsts (aref firsts X)))
		 ;; all [X -> Y_1 .. Y_n]s
		 (dolist (rule (cg-rules cg))
		   (when (= X (rule-lhs rule))
		     (let ((all-null t))
		       (loop while all-null
			     for Y in (rule-rhs rule)
			     do (let ((y-firsts (aref firsts Y))
				      (null-exists nil))
				  (dolist (symbol y-firsts)
				    (if (null symbol)
					(setf null-exists t)
					(pushnew symbol (aref firsts X))))
				  ;; if nil isn't in y-firsts, we don't have to look rest [Y_i]s.
				  (unless null-exists (setf all-null nil))))
		       ;; n = 0 or all Y_i's firsts has nil.
		       (when all-null
			 (pushnew nil (aref firsts X) :test #'equal)))))
		 (unless (set-eq old-x-firsts (aref firsts X))
		   (setf changed t)))))
    (setf *cg-first-array* firsts)))
		     
(defun nt-derive-first (symbol &optional (seen nil))
  (let ((rules (cg-rules *current-cg*))
	(result nil))
    (dolist (rule rules result)
      (when (and (= symbol (rule-lhs rule))
		 (rule-rhs rule))
	(let ((X (first (rule-rhs rule))))
	  (when (and (cg-non-terminal-p X)
		     (not (member X seen)))
	    (pushnew X result)
	    (setf result (union result (nt-derive-first X (cons symbol seen))))))))))
	
(defun cg-sequence-first (sequence)
  (if (null sequence) nil
      (let ((firsts (cg-first (first sequence))))
	(if (member () firsts)
	    (union firsts (cg-sequence-first (rest sequence)))
	    firsts))))

(defun calc-follow ()
  (let ((follows (make-array (+ (cg-num *current-cg*) 1) :initial-element nil))
	(changed t))
    (push (cg-num *current-cg*) (aref follows (cg-start *current-cg*)))
    (loop while changed
	  do (setf changed nil)
	     (dolist (rule (cg-rules *current-cg*))
	       (let ((A (rule-lhs rule)))
		 (labels ((f (symbol-list)
			    (let ((B (first symbol-list)))
			      (when (cg-non-terminal-p B)
				(let ((rest-firsts (cg-sequence-first (rest symbol-list))))
				  ;; [ A -> aB ] or [ A -> aBb /\ () in First(b) ]
				  (when (or (null rest-firsts) (member nil rest-firsts :test #'equal))
				    (dolist (symbol (aref follows A))
				      (unless (member symbol (aref follows B))
					(push symbol (aref follows B))
					(setf changed t))))
				  (dolist (symbol rest-firsts)
				    (unless (or (null symbol) (member symbol (aref follows B)))
				      (push symbol (aref follows B))
				      (setf changed t))))))))
		   (maplist #'f (rule-rhs rule))))))
    (setf *cg-follow-array* follows)))
  


;; --------------------------------
;;  LR parser
;; 

;; ----------------------------------
;;  lr-item
;; 
(defstruct (item 
	    (:constructor make-item (rule position &key (la-set nil))))
  rule (position 0 :type fixnum) (la-set nil))

(declaim (inline item-rule item-lhs item-rhs item-pre item-suc  item-next item= item<))

(defun item-lhs (item) (rule-lhs (item-rule item)))
(defun item-rhs (item) (declare (optimize (speed 3) (safety 0) (space 0))) (rule-rhs (item-rule item)))
(defun item-pre (item) (take (item-position item) (item-rhs item)))
(defun item-suc (item) (drop (item-position item) (item-rhs item)))
(defun item-suc-null (item)
  (declare (type item item))
  (= (item-position item) (length (the list (item-rhs item)))))
(defun item-next (item)
  (declare (type item item))
  (nth (item-position item) (item-rhs item)))

(defun item= (i1 i2)
  (declare (type item i1 i2) (optimize (speed 3) (safety 0) (space 0)))
  (or (eq i1 i2)
      (and (rule= (the rule (item-rule i1)) (the rule (item-rule i2)))
	   (= (item-position i1) (item-position i2)))))

(defun item< (i1 i2)
  ;; (declare (type item i1 i2))
  (cond ((eq i1 i2) nil)
	((rule< (item-rule i1) (item-rule i2)) t)
	((rule= (item-rule i1) (item-rule i2))
	 (< (item-position i1) (item-position i2)))
	(t nil)))

(defun shift-item (item)
  (unless (item-suc-null item)
    (make-item
     (item-rule item)
     (+ (item-position item) 1))))


(defun cg-first-symbols (n)
  (when (undefined-p *cg-first-symbols-array*)
    (setf *cg-first-symbols-array* (make-array (+ (cg-num *current-cg*) 1) :initial-element nil)))
  (aref *cg-first-symbols-array* n))

(defun first-symbols (n &optional (visited nil))
  (declare (type fixnum n))
  (cond ((cg-terminal-p n) (list n))
	((member n visited) nil)
	(t (let ((result (cg-first-symbols n)))
	     (when (eq result nil)
	       (setf result (list n))
	       (dolist (rule (cg-rules *current-cg*))
		 (when (and (= n (rule-lhs rule)) (rule-rhs rule))
		   (setf result (union (sequence-first-symbols (rule-rhs rule) (cons n visited)) result)))))
	     ;; (unless visited (setf (aref (cg-first-symbols-array cg) n) result))
	     (setf (aref *cg-first-symbols-array* n) result)
	     result))))

(defun sequence-first-symbols (sequence &optional (visited nil))
  (declare (optimize (speed 3) (space 0)))
  (if sequence
      (let ((fs (first-symbols (first sequence) visited)))
	(if (cg-nullable (first sequence))
	    (union fs (sequence-first-symbols (rest sequence) visited))
	    fs))
      ()))



;; 
;; state
;; 
(defstruct (state
	    (:constructor make-state-base))
  (items nil :type list)
  (gotos nil :type list)
  (comefroms nil :type list)
  (goto-rules nil :type list))

(defun make-state (&key items gotos comefroms goto-rules)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (make-state-base :items (sort items #'item<) :gotos gotos :comefroms comefroms :goto-rules goto-rules))

(defun item-set= (k1 k2)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (declare (list k1 k2))
  (or (eq k1 k2)
      (do* ((l1 k1 (cdr l1)) (l2 k2 (cdr l2))
	    (x (car l1) (car l1)) (y (car l2) (car l2)))
	   ((or (eq l1 l2) (null l1) (null l2)) (eq l1 l2))
	(unless (item= x y) (return nil)))))

(defun lr0-closure (items)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((rules (cg-rules *current-cg*))
	 (looked-symbols nil))
    (dolist (item items items)
      (unless (item-suc-null item)
	(let ((X (item-next item)))
	  (declare (type fixnum X))
	  (when (and (cg-non-terminal-p X)
		     (not (member X looked-symbols)))
	    (let ((fs (first-symbols X)))
	      (dolist (rule rules)
		(when (and (not (member (rule-lhs rule) looked-symbols))
			   (member (rule-lhs rule) fs :test #'=))
		  (push (make-item rule 0) items)))
	      (setf looked-symbols (append fs looked-symbols)))))))))

(defun lr0-goto (items symbol)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((base-items (remove-if-not
		     #'(lambda (item) (and (not (item-suc-null item))
					   (= symbol (item-next item))))
		     items)))
    (cons (lr0-closure (mapcar #'shift-item base-items))
	  (mapcar #'item-rule
		  (remove-if #'(lambda (x) (item-suc-null x)) base-items)))))

;; (define-hash-table-test state=
;;     (lambda (x) (sxhash (coerce x 'state))))

(defun lr0-parse-table ()
  (let* ((init-items (lr0-closure (list (make-item (first (cg-rules *current-cg*)) 0))))
	 (items-ht (make-hash-table :test 'item-set=))
	 (gotos-ht (make-hash-table :test 'item-set=))
	 (goto-rules-ht (make-hash-table :test 'item-set=))
	 (comefroms-ht (make-hash-table :test 'item-set=))
	 (state-list (list (cons init-items 0)))
	 (num 0))
    (declare (fixnum num))
    (setf (gethash init-items items-ht) 0)
    (labels ((f (items-list)
	       (when items-list
		 (let ((added nil))
		   (labels ((g (x items)
			      (destructuring-bind (goto0 . goto-rules) (lr0-goto items x)
				(let ((goto (sort goto0 #'item<)))
				(when goto
				  (multiple-value-bind (next found-p) (gethash goto items-ht)
				    (unless found-p
				      (the fixnum (incf num))
				      (setf next num)
				      (push (cons goto next) state-list)
				      (setf (gethash goto items-ht) next)
				      (push goto added))
				    (pushnew (cons x next) (gethash items gotos-ht) :test #'equal)
				    (pushnew (cons x goto-rules) (gethash items goto-rules-ht) :test #'equal) ; for disambiguation
				    (pushnew (cons (gethash items items-ht) x) (gethash goto comefroms-ht) :test #'equal)))))))
		     (dolist (items items-list)
		       (let ((symbols nil))
		       	 (dolist (item items)
		       	   (when (not (item-suc-null item)) (pushnew (item-next item) symbols)))
		       	 (dolist (rule (cg-rules *current-cg*))
		       	   (when (rule-rhs rule) (pushnew (first (rule-rhs rule)) symbols)))
		       	 (dolist (x symbols) (g x items))))
		     (f added))))))
      (f (list init-items))
      (let ((state-array (make-array (+ num 1))))
	(dolist (s-n state-list state-array)
	  (setf (aref state-array (cdr s-n))
		(make-state :items (car s-n)
			    :gotos (gethash (car s-n) gotos-ht)
			    :comefroms (gethash (car s-n) comefroms-ht)
			    :goto-rules (gethash (car s-n) goto-rules-ht))))))))



;; ================================
;;  DeRemer
;; 

(defstruct (parser
	    (:constructor make-parser-base (state-array state-num)))
  state-array
  (state-num 0 :type fixnum)
  )

(defparameter *pagen-current-parser* nil)
(defparameter *pagen-transitions* nil)
(defparameter *pagen-nt-transitions* nil)
(defparameter *pagen-direct-read* nil)
(defparameter *pagen-reads* nil)
(defparameter *pagen-read-set* nil)
(defparameter *pagen-includes* nil)
(defparameter *pagen-follow-set* nil)
(defparameter *pagen-lookback* nil)
(defparameter *pagen-lookahead-set* nil)

(defun make-parser (state-array)
  (setf *pagen-current-parser* (make-parser-base state-array (length state-array))))

(defun calc-transitions ()
  (let ((result nil)
	(n (parser-state-num *pagen-current-parser*)))
    (dotimes (i n)
      (setf result (append (mapcar #'(lambda (x) (cons i (car x))) (state-gotos (parser-state i))) result)))
    (setf *pagen-transitions*
	  result
	  *pagen-nt-transitions*
	  (remove-if-not #'(lambda (transition) (cg-non-terminal-p (cdr transition))) result))))

(defun parser-state (n)
  (aref (parser-state-array *pagen-current-parser*) n))

;; 
;; DR(p,A)
(defun calc-direct-read ()
  (let ((dr-set (make-hash-table :test #'equal))
	(n (parser-state-num *pagen-current-parser*)))
    (dotimes (i n)
      (let ((k (parser-state i)))
	(dolist (transition (state-gotos k))
	  (setf (gethash (cons i (car transition)) dr-set)
		(remove-if-not #'(lambda (x) (cg-terminal-p x))
			       (mapcar #'car (state-gotos (parser-state (cdr transition)))))))))
    dr-set))

;; note: we don't have to calculate before the calculating lookaheads
(defun calc-reads ()
  (let ((reads (make-hash-table :test #'equal))
	(n (parser-state-num *pagen-current-parser*))
	(exist-nullable nil))
    (loop until exist-nullable
	  for i from 0 to (- (cg-num *current-cg*) 1)
	  do (setf exist-nullable (cg-nullable  i)))
    (when exist-nullable
      (dotimes (i n)
	(dolist (transition (state-gotos (parser-state i)))
	  (setf (gethash (cons i (car transition)) reads)
		(mapcar #'(lambda (x) (cons (cdr transition) x))
			(remove-if-not #'(lambda (x) (cg-nullable  x))
				       (mapcar #'car (state-gotos (parser-state (cdr transition))))))))))
    reads))

;; assoc using equal
(defun assoceq (item alist)
  (assoc item alist :test #'equal))

;; 
;; algorithm: Digraph
(defun alg-digraph (node-list rel base-f-ht)
  (let* ((result-f-ht (make-hash-table :test #'equal))
	 (stack nil)
	 (depth 0)
	 (nar (make-hash-table :test #'equal)))
    (declare (type fixnum depth))
    (labels ((traverse (x)
	       (declare (optimize (speed 3) (safety 0) (space 0)))
	       (push x stack)
	       (incf depth)
	       (setf (gethash x nar) depth)
	       (setf (gethash x result-f-ht) (gethash x base-f-ht nil))
	       (dolist (y (gethash x rel nil))
		 (when (= 0 (the fixnum (gethash y nar 0))) (traverse y))
		 (setf (gethash x nar) (funcall #'(lambda (x y) (declare (type fixnum x y)) (if (< x y) x y)) (gethash x nar 0) (gethash y nar 0)))
		 (setf (gethash x result-f-ht)
		       (union (gethash x result-f-ht nil)
			      (gethash y result-f-ht nil) :test #'equal)))
	       (when (= depth (the fixnum (gethash x nar 0)))
		 (loop do (setf (gethash (first stack) nar) most-positive-fixnum)
			  (unless (equal (first stack) x)
			    (setf (gethash (first stack) result-f-ht) (gethash x result-f-ht nil))
			    )
		       until (equal x (pop stack))))))
      (dolist (node node-list result-f-ht)
	(when (= 0 (the fixnum (gethash node nar 0))) (traverse node))))))

(defun detect-loop (rel x &key (visited nil) (test #'eql))
  (let ((found nil))
    (loop until found
	  for s in (cdr (assoc x rel :test test))
	  do (if (member x visited :test test)
		 (setf found t)
		 (setf found (detect-loop rel s :visited (cons x visited) :test test))))
    found))

(defun calc-read-set ()
  ;; making node-list
  (alg-digraph *pagen-nt-transitions* *pagen-reads* *pagen-direct-read*))

(defun rev-traverse (starts sequence)
  ;; (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((s starts))
    (declare (list s))
    (loop for x in sequence
    	  do (setf s (apply #'append
    			    (mapcar #'(lambda (n)
    					(mapcar #'car
    						(filter #'(lambda (p) (= x (cdr p))) (state-comefroms (parser-state n)))))
    				    s))))
    s))

(defun calc-includes ()
  (let* ((rules (cg-rules *current-cg*))
	 (nt-transitions *pagen-nt-transitions*)
	 (result (make-hash-table :test #'equal)))
    (dolist (transition nt-transitions)	; transition = (p, A)
      (dolist (rule rules)		; rule = X -> aYb
	(let ((rhs (rule-rhs rule))
	      (pre nil))
	  (loop while rhs
		do (when (and (= (first rhs) (cdr transition))
			      (cg-sequence-nullable (rest rhs)))
		     (let* ((starts (rev-traverse (list (car transition)) pre))
			    (available (intersection (mapcar #'(lambda (x) (cons x (rule-lhs rule))) starts)
						     nt-transitions
						     :test #'equal)))
		       ;; (if (gethash (cons (car transition) (first rhs)) result nil)
			   (setf (gethash transition result)
				 (union (gethash transition result nil) available))))
			   ;; (setf (gethash transition result) available))))
		   (push (pop rhs) pre)))))
    result))


(defun calc-follow-set ()
  (alg-digraph *pagen-nt-transitions* *pagen-includes* *pagen-read-set*))

(defun calc-lookback ()
  (let ((result (make-hash-table :test #'equal)))
    (dotimes (n (parser-state-num *pagen-current-parser*))
      (dolist (item (state-items (parser-state n)))
	(when (item-suc-null item)
	  (let ((ps (rev-traverse (list n) (reverse-take (item-position item) (item-rhs item)))))
	    (when ps
	      (setf (gethash (cons n (rule-id (item-rule item))) result)
		    (mapcar #'(lambda (p) (cons p (item-lhs item))) ps)) result)))))
    result))

(defun calc-lookahead-set ()
  (let ((result (make-hash-table :test #'equal))
	(n (parser-state-num *pagen-current-parser*)))
    (dotimes (i n)
      (dolist (item (state-items (parser-state i)))
	(when (item-suc-null item)
	  (setf (gethash (cons i (rule-id (item-rule item))) result)
		(remove-duplicates
		 (apply #'append (mapcar #'(lambda (transition) (gethash transition *pagen-follow-set* nil))
					 (gethash (cons i (rule-id (item-rule item))) *pagen-lookback* nil)
					 ))
		 :test #'equal)))))
    result))


;; --------------------------------
;;  LALR(1)-parse-table
;; 

(defun deremer-lalr1-parser ()
  (make-parser (lr0-parse-table))
  (let* ((*pagen-direct-read* (calc-direct-read))
	 (*pagen-reads* (calc-reads))
	 (*pagen-transitions* (calc-transitions))
	 (*pagen-includes* (calc-includes))
	 (*pagen-read-set* (calc-read-set))
	 (*pagen-follow-set* (calc-follow-set))
	 (*pagen-lookback* (calc-lookback))
	 (*pagen-lookahead-set* (calc-lookahead-set)))
    (dotimes (i (parser-state-num *pagen-current-parser*))
      (mapcar #'(lambda (item)
		  (when (item-suc-null item)
		    (setf (item-la-set item) (gethash (cons i (rule-id (item-rule item))) *pagen-lookahead-set*)))
		  item)
	      (state-items (parser-state i))))
    *pagen-current-parser*))


;; --------------------------------
;;  Parser Action
;;  : conflict checker

(defstruct (shift-action
	    (:constructor make-shift-action (num)))
  (num 0 :type fixnum))

(defstruct (reduce-action
	    (:constructor make-reduce-action (rule)))
  rule)

(defstruct accept-action)

(defun make-action-array (&optional (rp-array ()) (sp-array ())) ; lalr1-parser
  (let* ((n (parser-state-num *pagen-current-parser*))
	 (conflicts nil)
	 (action-array (make-array n :initial-element nil)))
    (labels ((rule-rp (rule)
	       (if (null rp-array) 0
		   (aref rp-array (rule-id rule))))
	     (rule-sp (rule)
	       (if (null sp-array) 0
		   (aref sp-array (rule-id rule)))))
      ;; Shift Action
      (dotimes (i n)
	(setf (aref action-array i)
	      (mapcar #'(lambda (p) (list (car p) (make-shift-action (cdr p)))) (state-gotos (parser-state i)))))
      ;; Accept Action
      (if (assoc 0 (aref action-array 0))
	  (pushnew (make-accept-action) (cdr (assoc 0 (aref action-array 0))))
	  (push (list 0 (make-accept-action)) (aref action-array 0)))
      ;; Reduce Accept Action
      (dotimes (i n)
	(dolist (item (state-items (parser-state i)))
	    (when (item-suc-null item)
	      (if (null (item-la-set item))
		  (pushnew (list -1 (make-reduce-action (item-rule item))) (aref action-array i))
		  (dolist (la (item-la-set item))
		    (let ((action (assoc la (aref action-array i))))
		      (cond ((null action)
			     (push (list la (make-reduce-action (item-rule item))) (aref action-array i)))
			    (t 
			     (push (make-reduce-action (item-rule item)) (cdr (assoc la (aref action-array i))))))))))))
      ;; conflict check
      (dotimes (i n)
	(dolist (l (aref action-array i))
	  (destructuring-bind (la . actions) l
	    (when (< 1 (length actions))
	      (let ((conflict-info nil)
		    (shift-ps (mapcar #'rule-sp (cdr (assoc la (state-goto-rules (parser-state i))))))
		    (ps 0))
		(when shift-ps (setf ps (apply #'max shift-ps)))
		(dolist (action actions)
		  (cond ((reduce-action-p action)
			 (push (cons 'reduce (reduce-action-rule action)) conflict-info))
			((shift-action-p action)
			 (push (cons 'shift (cons (mapcar #'rule-id (cdr (assoc la (state-goto-rules (parser-state i)))))
						  (shift-action-num action)))
			       conflict-info))
			(t nil)))
		(push (cons (cons i la) conflict-info) conflicts)
		(setf actions
		      (sort 
		       (mapcar #'(lambda (action)
				   (cond ((reduce-action-p action)
					  (cons (rule-rp (reduce-action-rule action)) action))
					 ((shift-action-p action)
					  (cons ps action))
					 (t (cons 0 action))))
			       actions)
		       #'< :key #'car))
		(let ((max (apply #'max (mapcar #'car actions))))
		  (setf (cdr (assoc la (aref action-array i))) (mapcar #'cdr (filter #'(lambda (x) (= max (car x))) actions))))
		)))))
      ;; (pushnew (cons (cons i la) (cons new-action (cdr action))) sr-conflict)
      (values action-array conflicts))))



(defstruct (configuration
	    (:constructor make-configuration (states tokens reader))
	    (:print-function print-configuration))
  (states nil :type list)
  (tokens nil :type list)
  (reader () :type function))

(defun first-token (configuration)
  (when (null (configuration-tokens configuration))
    (push (funcall (configuration-reader configuration)) (configuration-tokens configuration)))
  (first (configuration-tokens configuration)))

(defun print-configuration (configuration stream depth)
  (if (or (and (numberp *print-level*) (>= depth *print-level*))
	  (configuration-p configuration))
      (format stream "[~{~a~^-~a->~}]~{~a~}~%"
	      (reverse (configuration-states configuration))
	      (configuration-tokens configuration))
      (write configuration :stream stream)))

(defun parse (reader action-array &key (function-array nil) (dump nil) (symbol-printer #'identity))
  (let* ((conf (make-configuration
		(list 0) 
		'()
		reader))
	 (symbol-stack nil)
	 (val-stack nil)
	 (parsing t)
	 (result nil))
    (loop while parsing
	  do (let* ((state (first (configuration-states conf)))
		    (symbol (or (first-token conf) -1)) ; -1 doesn't appear in grammar
		    (actions (cdr (assoc symbol (aref action-array state) :test #'=))))
	       (when dump
		 (format t "~3,,,' @a: ~{~a ~}. ~{~a~^ ~}~%"
			 state
			 (reverse (mapcar #'(lambda (x) (funcall symbol-printer x)) symbol-stack))
			 (mapcar #'(lambda (x) (funcall symbol-printer x)) (configuration-tokens conf))))	
	       (cond ((null actions)
		      (setf result "error: no action.~%" parsing nil))
		     ((< 1 (length actions))
		      ;; TODO: use condition-system to solve conflict
		      (setf result "error: conflict.~%" parsing nil))
		     ((shift-action-p (first actions))
		      ;; (when dump (format t "Shift~%"))
		      (let ((x (pop (configuration-tokens conf))))
			    (when (cg-terminal-p x)
			      (push (funcall symbol-printer x) val-stack))
			(push x symbol-stack))
		      (push (shift-action-num (first actions)) (configuration-states conf)))
		     ((reduce-action-p (first actions))
		      (let* ((rule (reduce-action-rule (first actions)))
			     (lhs (rule-lhs rule))
			     (rhs (rule-rhs rule)))
			;; (when dump (format t "Reduce by ~a -> ~{~a~^ ~}~%" (cg-symbol cg lhs) (mapcar #'(lambda (x) (cg-symbol cg x)) rhs)))
			(dotimes (i (length rhs))
			  ;tmp
			  (pop symbol-stack)
			  (pop (configuration-states conf)))
			(when function-array
			  (let ((vals nil))
			    ;; (when dump (format t ":: ~{~a~^ ~}~%" val-stack))
			    (dotimes (i (length rhs))
			      (push (pop val-stack) vals))
			    (push (apply (aref function-array (rule-id rule)) vals) val-stack)
			    ))
			(push lhs (configuration-tokens conf))))
		     ((accept-action-p (first actions))
		      (setf result nil parsing nil))
		     (t (setf result "error: invalid action.~%" parsing nil)))))
    (if result result (progn
			(when dump (format t "Accept!" ))
			(first val-stack)))))



;; 
;; Dragon Book's Method
;; 

(defun items->kernel (items)
  (remove-if-not #'(lambda (item)
	      (or (< 0 (item-position item))
		  (= 0 (rule-id (item-rule item)))))
	  items))

(defstruct (lr1-item
	    (:constructor make-lr1-item (body la)))
  body la)

(defun lr1-item= (i1 i2)
  (declare (type lr1-item i1 i2))
  (or (eq i1 i2)
      (and (item= (lr1-item-body i1) (lr1-item-body i2))
	   (= (lr1-item-la i1) (lr1-item-la i2)))))
(declaim (inline lr1-item))

(defun lr1-item< (i1 i2)
  (cond ((eq i1 i2) nil)
	((item< (lr1-item-body i1) (lr1-item-body i2)) t)
	((item= (lr1-item-body i1) (lr1-item-body i2))
	 (< (lr1-item-la i1) (lr1-item-la i2)))))

(defun lr1-closure (lr1-items )
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((result lr1-items))
    (labels ((f (items)
	       (when items 
		 (dolist (lr1-item items)
		   (let ((item (lr1-item-body lr1-item)))
		     (unless (or (item-suc-null item) (cg-terminal-p (item-next item)))
		       (dolist (rule (cg-rules *current-cg*))
			 (when (= (the fixnum (item-next item)) (the fixnum (rule-lhs rule)))
			   (let* ((new-lr1-items (mapcar #'(lambda (x) (make-lr1-item (make-item rule 0) x))
							 (cg-sequence-first (append (rest (item-suc item)) (list (lr1-item-la lr1-item))))))
				  (added nil))
			     (setf added (set-difference new-lr1-items result :test #'lr1-item=))
			     (setf result (append added result))
			     ;; (format t  "~a:~a~%" result added)
			     (f added))))))))))
      (f lr1-items))
    result))


(defun db-lalr1-parser ()
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((parser (make-parser (lr0-parse-table)))
	 (num (parser-state-num *pagen-current-parser*))
	 (propagate-symbol (+ (cg-num *current-cg*) 1))
	 (propagate-array (make-array num))
	 (la-set-array (make-array num))
	 (changed t))
    (dotimes (i num)
      (setf (aref propagate-array i) (make-hash-table :test 'item=)
	    (aref la-set-array i) (make-hash-table :test 'item=)))
    ;; Determine Look-Ahead Propagation
    (dotimes (i num)
      ;; each item in kernel of state I
      (dolist (item (items->kernel (state-items (parser-state i))))
	;; all GOTO(I,X)
	(let ((gotos (state-gotos (parser-state i))))
	  ;; calculate propagation for all item B -> a.Xb s.t. X is non-terminal
	  (dolist (lr1-item (lr1-closure (list (make-lr1-item item propagate-symbol))))
	    (let ((body (lr1-item-body lr1-item)))
	      (when (and (not (item-suc-null body))
			 (assoc (item-next body) gotos))
		(let ((propagate-ht (aref propagate-array (cdr (assoc (item-next body) gotos))))
		      (shifted-item (shift-item body)))
		  (if (= propagate-symbol (lr1-item-la lr1-item))
		      ;; lookahead propagate from item in state i
		      (push (cons i item) (gethash shifted-item propagate-ht))
		      ;; lookahead is generated spontaneously
		      (push (lr1-item-la lr1-item) (gethash shifted-item propagate-ht))))))))
	))
    ;; Propagation
    (loop while changed
	  do (setf changed nil)
	     (dotimes (i num)
	       (let ((propagate-ht (aref propagate-array i)))
		 ;; for item in state i
		 (dolist (item (state-items (parser-state i)))
		   (let ((la-set-ht (aref la-set-array i)))
		     (dolist (x (gethash item propagate-ht nil))
		       (cond ((numberp x)
			      (unless (member x (gethash item la-set-ht nil))
				(push x (gethash item la-set-ht))
				(setf changed t)))
			     ((consp x)
			      (let ((dif (set-difference (gethash (cdr x) (aref la-set-array (car x)))
							 (gethash item la-set-ht nil))))
				(when dif
				  (setf (gethash item la-set-ht) (append dif (gethash item la-set-ht))
					changed t))))
			     (t nil))))))))
    (dotimes (i num)
      (setf (state-items (parser-state i))
	    (lalr1-closure (mapcar #'(lambda (item) (setf (item-la-set item) (gethash item (aref la-set-array i))) item)
				      (items->kernel (state-items (parser-state i)))))))
    *pagen-current-parser*))


(defun lalr1-closure (items)
  (declare (optimize (speed 3) (space 0)))
  (let ((rules (cg-rules *current-cg*)))
    (dolist (item items)
	(unless (item-suc-null item)
	  (let ((X (item-next item)))
	    (when (and (numberp X)
		       (cg-non-terminal-p X))
	      (let ((fs (first-symbols X)))
		(dolist (rule rules)
		  (when (member (rule-lhs rule) fs)
		    (pushnew (make-item rule 0 :la-set (item-la-set item)) items :test #'item=))))))))
    items))
