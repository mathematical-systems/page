;; ================================================================ ;;
;;                                                                  ;;
;;   PaGe :: Parser Generator for LALR(1)-grammar by Common Lisp    ;;
;;                                                                  ;;
;; ================================================================ ;;

;; (defpackage :page
;;   (:use :cl-user))
;; (in-package 'page)

(defstruct (grammar
	    (:constructor make-grammar (start rules)))
  start (rules () :type list))

;; --------------------------------
;;  rule
;; 
(defstruct (rule
	    (:constructor make-rule (lhs rhs &key (function nil) (rp 0) (sp 0))))
  (id 0 :type fixnum) (lhs 0 :type fixnum) rhs (function nil :type function) (rp 0 :type fixnum) (sp 0 :type fixnum))

(declaim (inline rule=))
(defun rule= (r1 r2)
  (declare (type rule r1 r2))
  (eq r1 r2))
;  (= (rule-id r1) (rule-id r2)))

(defun rule< (r1 r2)
  (declare (type rule r1 r2))
  (< (rule-id r1) (rule-id r2)))
(declaim (inline rule<))



;; 
;; grammar represented by numbers (and symbol-number table)
;; 
(defstruct (canonical-grammar
	    (:conc-name cg-)
	    (:constructor make-canonical-grammar
	      (start rules nt-num num sym-array sym-alist unknown-symbol)))
  start rules nt-num num sym-array sym-alist unknown-symbol
  (nullable-array nil)			; bit array
  (first-array nil)
  (follow-array nil)
  (first-symbols-array nil)
  )


(defun canonicalize (grammar &key (start "$accept") (eof "$eof") (unknown-symbol "#"))
  (let* ((ntset (remove-duplicates (mapcar #'car (grammar-rules grammar)) :test #'equal))
	 (tset (set-difference (remove-duplicates (apply #'append (mapcar #'cdr (grammar-rules grammar))) :test #'equal) ntset :test #'equal))
	 (symbol-alist nil)
	 (nt-num 1)
	 (num 0))
    ;; numbering to non-terminal symbol
    (loop for symbol in ntset
	  do (push (cons symbol nt-num) symbol-alist)
	     (incf nt-num))
    (setf num nt-num)
    ;; numbering to terminal symbol
    (loop for symbol in tset
	  do (push (cons symbol num) symbol-alist)
	     (incf num))
    (push (cons eof num) symbol-alist)
    (push (cons unknown-symbol (+ num 1)) symbol-alist)
    ;; make symbol-array and canonical-grammar
    (let ((symbol-array (make-array (+ num 2) :initial-element nil))
	  (rules (cons (make-rule 0 (list (cdr (assoc (grammar-start grammar) symbol-alist :test #'equal)) num))
		       (mapcar #'(lambda (rule)
				   (let ((r (mapcar #'(lambda (x) (cdr (assoc x symbol-alist :test #'equal))) rule)))
				     (make-rule (car r) (cdr r)))) (grammar-rules grammar)))))
      (dolist (sym-num symbol-alist)
	(setf (aref symbol-array (cdr sym-num)) (car sym-num)))
      (let ((rule-num 0))
	(dolist (rule rules)
	  (setf (rule-id rule) rule-num)
	  (incf rule-num)))
      (setf (aref symbol-array 0) start (aref symbol-array num) eof)
      ;; default act-function
      (let ((symbols nil) (max 0))
	(mapcar #'(lambda (rule)
		    (let ((len (length (rule-rhs rule))))
		      (when (< max len)
			(dotimes (i (- len max))
			  (push (gensym) symbols))
			(setf max len))
		      (let ((args (take len symbols)) (body nil))
			(loop
			  for symbol in args
			  for x in (rule-rhs rule)
			  do (if (<= nt-num x)
				 (push `(list ,symbol) body)
				 (push symbol body)))
			(setf (rule-function rule) `(lambda ,args ,(list* 'list (aref symbol-array (rule-lhs rule)) (reverse body)))))))
		rules))
      (setf rules (sort rules #'< :key #'(lambda (rule) (length (rule-rhs rule)))))
      (loop for i from 0 to (- (length rules) 1)
	    for rule in rules
	    do (setf (rule-sp rule) i (rule-rp rule) i))
      ;; replace symbol to index
      (make-canonical-grammar
       (cdr (assoc (grammar-start grammar) symbol-alist :test #'equal))
       rules
       nt-num num symbol-array symbol-alist unknown-symbol))))

;; rule with function
(defun canonicalize-f (grammar &key (start "$accept") (eof "$eof") (unknown-symbol "#"))
  (let* ((ntset (remove-duplicates (mapcar #'caar (grammar-rules grammar)) :test #'equal))
	 (tset (set-difference (remove-duplicates (apply #'append (mapcar #'cdar (grammar-rules grammar))) :test #'equal) ntset :test #'equal))
	 (symbol-alist nil)
	 (nt-num 1)
	 (num 0))
    ;; numbering to non-terminal symbol
    (loop for symbol in ntset
	  do (push (cons symbol nt-num) symbol-alist)
	     (incf nt-num))
    (setf num nt-num)
    ;; numbering to terminal symbol
    (loop for symbol in tset
	  do (push (cons symbol num) symbol-alist)
	     (incf num))
    (push (cons eof num) symbol-alist)
    (push (cons unknown-symbol (+ num 1)) symbol-alist)
    ;; make symbol-array and canonical-grammar
    (let ((symbol-array (make-array (+ num 2) :initial-element nil))
	  (rules (cons (make-rule 0 (list (cdr (assoc (grammar-start grammar) symbol-alist :test #'equal)) num)
				  :function #'(lambda (x y) (list start x (list y))))
		       (mapcar #'(lambda (rule)
				   (let ((r (mapcar #'(lambda (x) (cdr (assoc x symbol-alist :test #'equal))) (car rule))))
				     (make-rule (car r) (cdr r)
						:function (second rule)
						:rp (or (car (third rule)) 0) :sp (or (cdr (third rule)) 0)
						))) (grammar-rules grammar)))))
      (dolist (sym-num symbol-alist)
	(setf (aref symbol-array (cdr sym-num)) (car sym-num)))
      (let ((rule-num 0))
	(dolist (rule rules)
	  (setf (rule-id rule) rule-num)
	  (incf rule-num)))
      (setf (aref symbol-array 0) start (aref symbol-array num) eof)
      ;; replace symbol to index
      (make-canonical-grammar
       (cdr (assoc (grammar-start grammar) symbol-alist :test #'equal))
       rules
       nt-num num symbol-array symbol-alist unknown-symbol))))


(defun cg-symbol (cg n &key (unknown t))
  ;; (declare (canonical-grammar cg))
  (if (and (<= 0 n) (<= n (cg-num cg)))
      (aref (cg-sym-array cg) n)
      (when unknown (cg-unknown-symbol cg))))

(defun cg-rule-symbol (cg rule)
  (cons (cg-symbol cg (rule-lhs rule))
	(mapcar #'(lambda (x) (cg-symbol cg x)) (rule-rhs rule))))

(defun cg-nullable (cg n)
  ;; (declare (canonical-grammar cg))
  (unless (cg-nullable-array cg) (calc-nullable cg))
  (= 1 (aref (cg-nullable-array cg) n)))

(defun cg-first (cg n)
  ;; (declare (canonical-grammar cg))
  (unless (cg-first-array cg) (calc-first cg))
  (if (< n (cg-num cg))
      (aref (cg-first-array cg) n)
      (list n)))

(defun cg-follow (cg n)
  ;; (declare (canonical-grammar cg))
  (unless (cg-follow-array cg) (calc-follow cg))
  (aref (cg-follow-array cg) n))

(defun cg-non-terminal-p (cg num)
  ;; (declare (canonical-grammar cg))
  (< num (cg-nt-num cg)))

(defun cg-terminal-p (cg num)
  (declare (canonical-grammar cg))
  (<= (cg-nt-num cg) num))

(defun foldl (e op l)
  (if l (foldl (funcall op e (first l)) op (rest l)) e))

(defun filter (predicate sequence)
  (when sequence
    (destructuring-bind (x . ls) sequence
      (if (funcall predicate x)
	  (cons x (filter predicate ls))
	  (filter predicate ls)))))

(defun take (n l)
  (if (< 0 n) (cons (first l) (take (- n 1) (rest l))) nil))

(defun reverse-take (n l)
  (let ((input l)
	(output nil))
    (dotimes (i n output)
      (push (pop input) output))))

(defun drop (n l)
  (if (< 0 n) (drop (- n 1) (rest l)) l))

(defun set-eq (s1 s2 &key (test #'equal))
  (null (set-exclusive-or s1 s2 :test test)))

;; nullability-checker
(defun calc-nullable (cg)
  (let ((nullable (make-array (+ (cg-num cg) 1) :element-type 'bit :initial-element 0))
	(changed t))
    ;; initialize
    (loop while changed
	  do (setf changed nil)
	     (dolist (rule (cg-rules cg))
	       (when (or (null (rule-rhs rule)) ; X -> 
			 (every #'(lambda (n) (= 1 (aref nullable n))) (rule-rhs rule))) ; X -> Y_1 .. Y_n s.t. all Y_i is nullable.
		 ;; only false -> true
		 (when (= 0 (aref nullable (rule-lhs rule)))
		   (setf (aref nullable (rule-lhs rule)) 1
			 changed t)))))
    (setf (cg-nullable-array cg) nullable)))

(defun cg-sequence-nullable (cg sequence)
  (let ((nullable t))
    (loop while nullable
	  for s in sequence
	  do (setf nullable (cg-nullable cg s)))
    nullable))

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
    (setf (cg-first-array cg) firsts)))
		     
(defun nt-derive-first (cg symbol &optional (seen nil))
  (let ((rules (cg-rules cg))
	(result nil))
    (dolist (rule rules result)
      (when (and (= symbol (rule-lhs rule))
		 (rule-rhs rule))
	(let ((X (first (rule-rhs rule))))
	  (when (and (cg-non-terminal-p cg X)
		     (not (member X seen)))
	    (pushnew X result)
	    (setf result (union result (nt-derive-first cg X (cons symbol seen))))))))))
	
(defun cg-sequence-first (cg sequence)
  (if (null sequence) nil
      (let ((firsts (cg-first cg (first sequence))))
	(if (member () firsts)
	    (union firsts (cg-sequence-first cg (rest sequence)))
	    firsts))))

(defun calc-follow (cg)
  (let ((follows (make-array (+ (cg-num cg) 1) :initial-element nil))
	(changed t))
    (push (cg-num cg) (aref follows (cg-start cg)))
    (loop while changed
	  do (setf changed nil)
	     (dolist (rule (cg-rules cg))
	       (let ((A (rule-lhs rule)))
		 (labels ((f (symbol-list)
			    (let ((B (first symbol-list)))
			      (when (cg-non-terminal-p  cg B)
				(let ((rest-firsts (cg-sequence-first cg (rest symbol-list))))
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
    (setf (cg-follow-array cg) follows)))
  


;; --------------------------------
;;  LR parser
;; 

;; ----------------------------------
;;  lr-item
;; 
(defstruct (item 
	    (:constructor make-item (rule position)))
  rule (position 0 :type fixnum))

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



(defun cg-print-item (cg item &key (stream t))
  (format stream "~S -> ~{~S~^ ~} .. ~{~S~^ ~}"
	  (cg-symbol cg (item-lhs item))
	  (mapcar #'(lambda (x) (cg-symbol cg x)) (item-pre item))
	  (mapcar #'(lambda (x) (cg-symbol cg x)) (item-suc item))))



(defun cg-first-symbols (cg n)
  (unless (cg-first-symbols-array cg)
    (setf (cg-first-symbols-array cg) (make-array (+ (cg-num cg) 1) :initial-element nil)))
  (aref (cg-first-symbols-array cg) n))

(defun first-symbols (cg n &optional (visited nil))
  (declare (type fixnum n))
  (cond ((cg-terminal-p cg n) (list n))
	((member n visited) nil)
	(t (let ((result (cg-first-symbols cg n)))
	     (when (eq result nil)
	       (setf result (list n))
	       (dolist (rule (cg-rules cg))
		 (when (and (= n (rule-lhs rule)) (rule-rhs rule))
		   (setf result (union (sequence-first-symbols cg (rule-rhs rule) (cons n visited)) result)))))
	     ;; (unless visited (setf (aref (cg-first-symbols-array cg) n) result))
	     (setf (aref (cg-first-symbols-array cg) n) result)
	     result))))

(defun sequence-first-symbols (cg sequence &optional (visited nil))
  (declare (optimize (speed 3) (space 0)))
  (if sequence
      (let ((fs (first-symbols cg (first sequence) visited)))
	(if (cg-nullable cg (first sequence))
	    (union fs (sequence-first-symbols cg (rest sequence) visited))
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


(defun cg-print-state (cg state &key (stream t))
  (format stream "~{[~a]~%~}~{~a~^~%~}"
	  (mapcar #'(lambda (x) (cg-print-item cg x :stream nil)) (state-items state))
	  (mapcar #'(lambda (x) (cons (cg-symbol cg (car x)) (cdr x))) (state-gotos state))
	  ))

(defun lr0-closure (cg items)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((rules (cg-rules cg))
	 (looked-symbols nil))
    (dolist (item items items)
      (unless (item-suc-null item)
	(let ((X (item-next item)))
	  (declare (type fixnum X))
	  (when (and (cg-non-terminal-p cg X)
		     (not (member X looked-symbols)))
	    (let ((fs (first-symbols cg X)))
	      (dolist (rule rules)
		(when (and (not (member (rule-lhs rule) looked-symbols))
			   (member (rule-lhs rule) fs :test #'=))
		  (push (make-item rule 0) items)))
	      (setf looked-symbols (append fs looked-symbols)))))))))

(defun lr0-goto (cg items symbol)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((base-items (remove-if-not
		     #'(lambda (item) (and (not (item-suc-null item))
					   (= symbol (item-next item))))
		     items)))
    (cons (lr0-closure cg (mapcar #'shift-item base-items))
	  (mapcar #'item-rule
		  (remove-if #'(lambda (x) (item-suc-null x)) base-items)))))

;; (define-hash-table-test state=
;;     (lambda (x) (sxhash (coerce x 'state))))

(defun lr0-parse-table (cg)
  (let* ((init-items (lr0-closure cg (list (make-item (first (cg-rules cg)) 0))))
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
			      (destructuring-bind (goto0 . goto-rules) (lr0-goto cg items x)
				(let ((goto (sort goto0 #'item<)))
				(when goto
				  (let ((next (gethash goto items-ht nil)))
				    (unless next
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
		       	 (dolist (rule (cg-rules cg))
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
	    (:constructor make-parser-base (grammar state-array state-num)))
  grammar
  state-array
  (state-num 0 :type fixnum)
  (transitions nil :type list)
  (nt-transitions nil :type list)
  (direct-read nil )
  (reads nil)
  (read-set nil)
  (includes nil)
  (follow-set nil)
  (lookback nil)
  (lookahead-set nil)
  )

(defun make-parser (grammar state-array)
  (make-parser-base grammar state-array (length state-array)))

(defun calc-transitions (parser)
  (let ((result nil)
	(n (parser-state-num parser)))
    (dotimes (i n)
      (setf result (append (mapcar #'(lambda (x) (cons i (car x))) (state-gotos (parser-state parser i))) result)))
    (setf (parser-transitions parser)
	  result
	  (parser-nt-transitions parser)
	  (remove-if-not #'(lambda (transition) (cg-non-terminal-p (parser-grammar parser) (cdr transition))) result))))

(defun parser-state (parser n)
  (declare (type parser parser) (fixnum n))
  (aref (parser-state-array parser) n))

;; 
;; DR(p,A)
(defun calc-direct-read (parser)
  (let ((dr-set (make-hash-table :test #'equal))
	(n (parser-state-num parser)))
    (dotimes (i n)
      (let ((k (parser-state parser i)))
	(dolist (transition (state-gotos k))
	  (setf (gethash (cons i (car transition)) dr-set)
		(remove-if-not #'(lambda (x) (cg-terminal-p (parser-grammar parser) x))
			       (mapcar #'car (state-gotos (parser-state parser (cdr transition)))))))))
    (setf (parser-direct-read parser) dr-set)))

;; note: we don't have to calculate before the calculating lookaheads
(defun calc-reads (parser)
  (let ((reads (make-hash-table :test #'equal))
	(n (parser-state-num parser))
	(exist-nullable nil))
    (loop until exist-nullable
	  for i from 0 to (- (cg-num (parser-grammar parser)) 1)
	  do (setf exist-nullable (cg-nullable (parser-grammar parser) i)))
    (when exist-nullable
      (dotimes (i n)
	(dolist (transition (state-gotos (parser-state parser i)))
	  (setf (gethash (cons i (car transition)) reads)
		(mapcar #'(lambda (x) (cons (cdr transition) x))
			(remove-if-not #'(lambda (x) (cg-nullable (parser-grammar parser) x))
				       (mapcar #'car (state-gotos (parser-state parser (cdr transition))))))))))
    (setf (parser-reads parser) reads)))


(defun cg-print-reads (parser)
  (dolist (r (parser-reads parser))
    (destructuring-bind (dom . cod) r
      (format t "~a reads ~{~a~^,~}~%"
	      (cons (car dom) (cg-symbol (parser-grammar parser) (cdr dom)))
	      (mapcar #'(lambda (x) (cons (car x) (cg-symbol (parser-grammar parser) (cdr x)))) cod)))))

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

(defun calc-read-set (parser)
  (let* ((nt-transitions (parser-nt-transitions parser)))
    ;; making node-list
    (setf (parser-read-set parser) (alg-digraph nt-transitions (parser-reads parser) (parser-direct-read parser)))))

(defun rev-traverse (parser starts sequence)
  ;; (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((s starts))
    (declare (list s))
    (loop for x in sequence
    	  do (setf s (apply #'append
    			    (mapcar #'(lambda (n)
    					(mapcar #'car
    						(filter #'(lambda (p) (= x (cdr p))) (state-comefroms (parser-state parser n)))))
    				    s))))
    s))

(defun calc-includes (parser)
  (let* ((cg (parser-grammar parser))
	 (rules (cg-rules cg))
	 (nt-transitions (parser-nt-transitions parser))
	 (result (make-hash-table :test #'equal)))
    (dolist (transition nt-transitions)	; transition = (p, A)
      (dolist (rule rules)		; rule = X -> aYb
	(let ((rhs (rule-rhs rule))
	      (pre nil))
	  (loop while rhs
		do (when (and (= (first rhs) (cdr transition))
			      (cg-sequence-nullable cg (rest rhs)))
		     (let* ((starts (rev-traverse parser (list (car transition)) pre))
			    (available (intersection (mapcar #'(lambda (x) (cons x (rule-lhs rule))) starts)
						     nt-transitions
						     :test #'equal)))
		       ;; (if (gethash (cons (car transition) (first rhs)) result nil)
			   (setf (gethash transition result)
				 (union (gethash transition result nil) available))))
			   ;; (setf (gethash transition result) available))))
		   (push (pop rhs) pre)))))
    (setf (parser-includes parser) result)))


(defun calc-follow-set (parser)
  (let* ((nt-transitions (parser-nt-transitions parser)))
    (setf (parser-follow-set parser) (alg-digraph nt-transitions (parser-includes parser) (parser-read-set parser)))))

(defun calc-lookback (parser)
  (let ((result (make-hash-table :test #'equal)))
    (dotimes (n (parser-state-num parser))
      (dolist (item (state-items (parser-state parser n)))
	(when (item-suc-null item)
	  (let ((ps (rev-traverse parser (list n) (reverse-take (item-position item) (item-rhs item)))))
	    (when ps
	      (setf (gethash (cons n (item-rule item)) result)
		    (mapcar #'(lambda (p) (cons p (item-lhs item))) ps)) result)))))
    (setf (parser-lookback parser) result)))

(defun calc-lookahead-set (parser)
  (let ((result (make-hash-table :test #'equal))
	(n (parser-state-num parser)))
    (dotimes (i n)
      (dolist (item (state-items (parser-state parser i)))
	(when (item-suc-null item)
	  (setf (gethash (cons i (item-rule item)) result)
		(remove-duplicates
		 (apply #'append (mapcar #'(lambda (transition) (gethash transition (parser-follow-set parser) nil))
					 (gethash (cons i (item-rule item)) (parser-lookback parser) nil)
					 ))
		 :test #'equal)))))
    (setf (parser-lookahead-set parser) result)))

;; --------------------------------
;;  LALR(1)-parse-table
;; 

(defstruct (lalr1-item
	    (:constructor make-lalr1-item-base (body la-set)))
  body la-set)

(defun make-lalr1-item (body la-set)
  (make-lalr1-item-base body (sort la-set #'<)))

(defun lalr1-item= (i1 i2)
  (declare (type lalr1-item i1 i2))
  (or (eq i1 i2)
      (and (item= (lalr1-item-body i1) (lalr1-item-body i2))
	   (set-eq (lalr1-item-la-set i1) (lalr1-item-la-set i2)))))

(defun cg-print-lalr1-item (cg lalr1-item &optional (stream t))
  (format stream "[~a ;; ~{~S~^,~}]"
	  (cg-print-item cg (lalr1-item-body lalr1-item) :stream nil)
	  (mapcar #'(lambda (x) (cg-symbol cg x)) (lalr1-item-la-set lalr1-item))))

(defstruct (lalr1-state
	    (:constructor make-lalr1-state-base (items gotos goto-rules)))
  items gotos goto-rules)

(defun make-lalr1-state (items gotos goto-rules)
  (make-lalr1-state-base (sort items #'item< :key #'lalr1-item-body) (sort gotos #'< :key #'car) goto-rules))

(defun cg-print-lalr1-state (cg lalr1-state &optional (stream t))
  (format stream "~{~a~%~}~{~S~^~%~}"
	  (mapcar #'(lambda (x) (cg-print-lalr1-item cg x nil)) (lalr1-state-items lalr1-state))
	  (mapcar #'(lambda (c) (cons (cg-symbol cg (car c)) (cdr c))) (lalr1-state-gotos lalr1-state))))

(defstruct (lalr1-parser
	    (:constructor make-lalr1-parser-base (grammar state-array state-num)))
  grammar state-array (state-num 0 :type fixnum))

(defun make-lalr1-parser (grammar state-array)
  (make-lalr1-parser-base grammar state-array (length state-array)))

(defun lalr1-parser-state (lalr1-parser n)
  ;; TODO: error handling
  (aref (lalr1-parser-state-array lalr1-parser) n))

(defun print-lalr1-parser (lalr1-parser &optional (stream t))
  (dotimes (i (lalr1-parser-state-num lalr1-parser))
    (format stream ":state(~a):~%~a~%" i (cg-print-lalr1-state (lalr1-parser-grammar lalr1-parser) (lalr1-parser-state lalr1-parser i) nil))))

(defun deremer-lalr1-parser (cg)
  (let* ((parser (make-parser cg (lr0-parse-table cg)))
	 (n (parser-state-num parser))
	 (lalr1-state-array (make-array n)))
    (calc-direct-read parser)
    (calc-reads parser)
    (calc-transitions parser)
    (calc-includes parser)
    (calc-read-set parser)
    (calc-follow-set parser)
    (calc-lookback parser)
    (calc-lookahead-set parser)
    (dotimes (i (parser-state-num parser))
      (setf (aref lalr1-state-array i)
	    (make-lalr1-state
	      (mapcar #'(lambda (item)
			  (make-lalr1-item item
					  (when (item-suc-null item)
					    (gethash (cons i (item-rule item)) (parser-lookahead-set parser)))))
		     (state-items (parser-state parser i)))
	      (state-gotos (parser-state parser i))
	      (state-goto-rules (parser-state parser i)))))
    (make-lalr1-parser (parser-grammar parser) lalr1-state-array)))


(defun dot-lalr1-parse-table (lalr1-parser &key (name "lalr1") (stream t))
  (let ((cg (lalr1-parser-grammar lalr1-parser)))
    (labels ((f (item)
	       (let ((body (lalr1-item-body item)))
		 (if (lalr1-item-la-set item)
		     (format nil "[~a\\ -\\>\\ ~{~a~^\\ ~}・\\ ~{~a~^\\ ~}\\ ,\\ ~{~a~^\\ ~}]"
			     (cg-symbol cg (item-lhs body))
			     (mapcar #'(lambda (x) (cg-symbol cg x)) 
				     (take (item-position body) (item-rhs body)))
			     (mapcar #'(lambda (x) (cg-symbol cg x))
				     (drop (item-position body) (item-rhs body)))
			     (mapcar #'(lambda (x) (cg-symbol cg x)) 
				     (lalr1-item-la-set item)))
		     (format nil "[~a\\ -\\>\\ ~{~a~^\\ ~}・\\ ~{~a~^\\ ~}]\\ "
			 (cg-symbol cg (item-lhs body))
			 (mapcar #'(lambda (x) (cg-symbol cg x)) 
				 (take (item-position body) (item-rhs body)))
			 (mapcar #'(lambda (x) (cg-symbol cg x))
				 (drop (item-position body) (item-rhs body)))
			 )))))
      (format stream "~{~a~}"
	      (cons (format nil "digraph ~a {~%" name)
		    (let ((str nil))
		      (dotimes (i (lalr1-parser-state-num lalr1-parser))
			(push (format nil
				      "  ~a [shape=record, label=\"{~a|{~{~a\\l~}}}\"];~%~{~a~%~}"
				      i i (mapcar #'f (lalr1-state-items (lalr1-parser-state lalr1-parser i)))
				      (mapcar #'(lambda (c)
						  (format nil "  ~a -> ~a [label=\"~a\"];" i (cdr c) (cg-symbol cg (car c))))
					      (lalr1-state-gotos (lalr1-parser-state lalr1-parser i)))
				      )
			      str))
		      (push "}" str)
		      (reverse str)))))))

(defun write-dot-lalr1-pt (fname parser)
  (with-open-file (ost fname :direction :output :if-exists :supersede)
    (dot-lalr1-parse-table parser :stream ost)))
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

(defun parser-check (parser) ; lalr1-parser
  (let* ((n (lalr1-parser-state-num parser))
	 (action-array (make-array n :initial-element nil)))
    ;; Shift Action
    (dotimes (i n)
      (setf (aref action-array i)
	    (mapcar #'(lambda (p) (list (car p) (make-shift-action (cdr p)))) (lalr1-state-gotos (lalr1-parser-state parser i)))))
    ;; Accept Action
    (if (assoc 0 (aref action-array 0))
	(pushnew (make-accept-action) (cdr (assoc 0 (aref action-array 0))))
	(push (list 0 (make-accept-action)) (aref action-array 0)))
    ;; Reduce Accept Action
    (dotimes (i n)
      (dolist (lalr1-item (lalr1-state-items (lalr1-parser-state parser i)))
	(let ((body (lalr1-item-body lalr1-item)))
	  (when (item-suc-null body)
	    (if (null (lalr1-item-la-set lalr1-item))
		(pushnew (list -1 (make-reduce-action (item-rule body))) (aref action-array i))
		(dolist (la (lalr1-item-la-set lalr1-item))
		  (let ((action (assoc la (aref action-array i))))
		    (cond ((null action)
			   (push (list la (make-reduce-action (item-rule body))) (aref action-array i)))
			  (t 
			   (push (make-reduce-action (item-rule body)) (cdr (assoc la (aref action-array i)))))))))))))
    ;; conflict check
    (let ((cg (lalr1-parser-grammar parser)))
      (dotimes (i n)
	(dolist (l (aref action-array i))
	  (destructuring-bind (la . actions) l
	    (when (< 1 (length actions))
	      (format t "(~a , \"~a\"):~%" i (cg-symbol cg la))
	      (let ((all-reduce t)
		    (shift-ps (mapcar #'rule-sp (cdr (assoc la (lalr1-state-goto-rules (lalr1-parser-state parser i))))))
		    (ps 0))
		(when shift-ps (setf ps (apply #'max shift-ps)))
		(dolist (action actions)
		  (cond ((reduce-action-p action)
			 (let ((rule (cg-rule-symbol cg (reduce-action-rule action))))
			   (format t "  <Reduce> ~a: ~a -> ~{~a~^ ~} [~a]~%"
				   (rule-id (reduce-action-rule action)) (car rule) (cdr rule)
				   (rule-rp (reduce-action-rule action)))))
			((shift-action-p action)
			 (setf all-reduce nil)
			 (format t "  <Shift> (~{~a~^,~}) to ~a [~a]~%"
				 (mapcar #'rule-id (cdr (assoc la (lalr1-state-goto-rules (lalr1-parser-state parser i))))) (shift-action-num action) ps))
			(t nil)))
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
		))))))
    ;; (pushnew (cons (cons i la) (cons new-action (cdr action))) sr-conflict)
    action-array))


(defstruct (configuration
	    (:constructor make-configuration (states symbols))
	    (:print-function print-configuration))
  (states nil :type list)
  (symbols nil :type list))

(defun print-configuration (configuration stream depth)
  (if (or (and (numberp *print-level*) (>= depth *print-level*))
	  (configuration-p configuration))
      (format stream "[~{~a~^-~a->~}]~{~a~}~%"
	      (reverse (configuration-states configuration))
	      (configuration-symbols configuration))
      (write configuration :stream stream)))

(defun p-apply (sequence f predicate)
  (let ((result nil))
    (do* ((l sequence (rest l)) (x (car l) (car l)) (n 0 (incf n)))
	 ((null l) (reverse result))
      (push (if (funcall predicate n) (funcall f x) x) result))))
    

(defun cg-print-configuration (cg configuration stream)
  (format stream "[~{~a~^ -- ~a -> ~}]~{~a~^ ~}~%"
	  (p-apply (reverse (configuration-states configuration)) #'(lambda (x) (cg-symbol cg x)) #'oddp)
	  (mapcar #'(lambda (x) (cg-symbol cg x)) (configuration-symbols configuration))))


(defun parse (parser sequence &key (with-eof nil) (dump nil))
  (let* ((action-array (parser-check parser))
	 (input (mapcar #'(lambda (symbol) (or (cdr (assoc symbol (cg-sym-alist (parser-grammar parser)) :test #'equal)) -1)) sequence))
	 (conf (make-configuration
		(list 0) 
		(if with-eof input (append input (list (cg-num (parser-grammar parser)))))))
	 (symbol-stack nil)
	 (val-stack nil)
	 (cg (parser-grammar parser))
	 (parsing t)
	 (result nil))
    ;; Report Shift/Reduce conflict.
    (loop while parsing
	  do (let* ((state (first (configuration-states conf)))
		    (symbol (or (first (configuration-symbols conf)) -1))
		    (actions (cdr (assoc symbol (aref action-array state) :test #'=))))
	       
	       (cond ((null actions)
		      (setf result "error: no action.~%" parsing nil))
		     ((< 1 (length actions))
		      ;; TODO: use condition-system to solve conflict
		      (setf result "error: conflict.~%" parsing nil))
		     ((shift-action-p (first actions))
		      ;; (when dump (format t "Shift~%"))
		      (let ((x (pop (configuration-symbols conf))))
			    (when (cg-terminal-p cg x)
			      (push (cg-symbol cg x) val-stack))
			(push x symbol-stack))
		      (push (shift-action-num (first actions)) (configuration-states conf)))
		     ((reduce-action-p (first actions))
		      (let* ((rule (reduce-action-rule (first actions)))
			     (lhs (rule-lhs rule))
			     (rhs (rule-rhs rule))
			     (fun (rule-function rule)))
			(when dump
			  (format t "~3,,,' @a: ~{~a ~}. ~{~a~^ ~}~%"
				  state
				  (reverse (mapcar #'(lambda (x) (cg-symbol cg x)) symbol-stack))
				  (mapcar #'(lambda (x) (cg-symbol cg x)) (configuration-symbols conf))))
			;; (when dump (format t "Reduce by ~a -> ~{~a~^ ~}~%" (cg-symbol cg lhs) (mapcar #'(lambda (x) (cg-symbol cg x)) rhs)))
			(dotimes (i (length rhs))
			  ;tmp
			  (pop symbol-stack)
			  (pop (configuration-states conf)))
			(when fun
			  (let ((vals nil))
			    ;; (when dump (format t ":: ~{~a~^ ~}~%" val-stack))
			    (dotimes (i (length rhs))
			      (push (pop val-stack) vals))
			    (push (apply fun vals) val-stack)
			    ))
			(push lhs (configuration-symbols conf))))
		     ((accept-action-p (first actions))
		      (setf result nil parsing nil))
		     (t (setf result "error: invalid action.~%" parsing nil)))))
    (if result result (progn
			(when dump (format t "Accept!" ))
			(first val-stack)))))

;; 
(defun dfs-tree (tree &optional (num 0))
  (if (null tree) (values () num)
      (let ((n (+ num 1)))
	(values
	 (cons (cons num (car tree))
	       (let ((result nil))
		 (loop for child in (cdr tree)
		       do (multiple-value-bind (child1 next)
			      (dfs-tree child n)
			    (setf n next)
			    (push child1 result)))
		 (reverse result)))
	 n))
       ))


(defun dot-tree (tree &optional (stream t))
  (let ((bottoms nil))
    (labels ((f (x)
	       (when x
		 (destructuring-bind (node . children) x
		   (format stream "  ~a [shape=plaintext, label = ~S];~%" (car node) (cdr node))
		   (when (null children)
		     (push (car node) bottoms))
		   (mapcar #'(lambda (child) (format stream "  ~a -> ~a;~%" (car node) (caar child))) children)
		   (mapcar #'(lambda (child) (f child)) children)
		   nil))))
      (format stream "digraph tree {~%")
      (f tree)
      (format stream "  {rank = same; ~{~a~^;~}}~%}~%" bottoms))))
      
(defun write-dot-tree (fname tree)
  (with-open-file (ost fname :direction :output :if-exists :supersede)
    (dot-tree tree ost)))
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

(defun lr1-closure (cg lr1-items )
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let ((result lr1-items))
    (labels ((f (items)
	       (when items 
		 (dolist (lr1-item items)
		   (let ((item (lr1-item-body lr1-item)))
		     (unless (or (item-suc-null item) (cg-terminal-p cg (item-next item)))
		       (dolist (rule (cg-rules cg))
			 (when (= (the fixnum (item-next item)) (the fixnum (rule-lhs rule)))
			   (let* ((new-lr1-items (mapcar #'(lambda (x) (make-lr1-item (make-item rule 0) x))
							 (cg-sequence-first cg (append (rest (item-suc item)) (list (lr1-item-la lr1-item))))))
				  (added nil))
			     (setf added (set-difference new-lr1-items result :test #'lr1-item=))
			     (setf result (append added result))
			     ;; (format t  "~a:~a~%" result added)
			     (f added))))))))))
      (f lr1-items))
    result))


(defun db-lalr1-parser (cg)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((parser (make-parser cg (lr0-parse-table cg)))
	 (num (parser-state-num parser))
	 (propagate-symbol (+ (cg-num cg) 1))
	 (propagate-array (make-array num))
	 (la-set-array (make-array num))
	 (changed t))
    (dotimes (i num)
      (setf (aref propagate-array i) (make-hash-table :test 'item=)
	    (aref la-set-array i) (make-hash-table :test 'item=)))
    ;; Determine Look-Ahead Propagation
    (dotimes (i num)
      ;; each item in kernel of state I
      (dolist (item (items->kernel (state-items (parser-state parser i))))
	;; all GOTO(I,X)
	(let ((gotos (state-gotos (parser-state parser i))))
	  ;; calculate propagation for all item B -> a.Xb s.t. X is non-terminal
	  (dolist (lr1-item (lr1-closure cg (list (make-lr1-item item propagate-symbol))))
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
		 (dolist (item (state-items (parser-state parser i)))
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
    (let ((lalr1-state-array (make-array num)))
      (dotimes (i num)
	(setf
	 (aref lalr1-state-array i)
	 (make-lalr1-state
	  (lalr1-closure cg (mapcar #'(lambda (item) (make-lalr1-item item (gethash item (aref la-set-array i))))
				    (items->kernel (state-items (parser-state parser i)))))
	  (state-gotos (parser-state parser i))
	  (state-goto-rules (parser-state parser i)))
	 ))
      (make-lalr1-parser (parser-grammar parser) lalr1-state-array))))


(defun lalr1-closure (cg items)
  (declare (optimize (speed 3) (space 0)))
  (let ((rules (cg-rules cg)))
    (dolist (lalr1-item items)
      (let ((body (lalr1-item-body lalr1-item)))
	(unless (item-suc-null body)
	  (let ((X (item-next body)))
	    (when (and (numberp X)
		       (cg-non-terminal-p cg X))
	      (let ((fs (first-symbols cg X)))
		(dolist (rule rules)
		  (when (member (rule-lhs rule) fs)
		    (pushnew (make-lalr1-item (make-item rule 0) (lalr1-item-la-set lalr1-item)) items :test #'lalr1-item=)))))))))
    items))

;; (defun make-lalr1-items (cg)
;;   (let* (
;; 	 ;; step 1 and 2
;; 	 (all-items (lr0-parse-table cg))
;; 	 ;; step 3
;; 	 (all-kernels 
;; 	   (mapcar #'(lambda (x)
;; 		       (cons (make-kernels (car x)) (cdr x))) ; magic-number: 0
;; 		   (car all-items)))
;; 	 (result nil)
;; 	 (all-lalr1-kernels nil)
;; 	 ;; (not-in-symbol (gensym))
;; 	 (ht-from (make-hash-table :test #'equal))
;; 	 (ht-spon (make-hash-table :test #'equal)))
;;     ;; step 4
;;     (setf (gethash
;;     	   (cons 0 (list 0 () (list (cg-start cg) (cg-num cg))))
;;     	   ht-spon)
;; 	  nil)
;;     (dolist (kernels-and-num all-kernels)
;;       (let ((kernels (car kernels-and-num))
;; 	    (num (cdr kernels-and-num))
;; 	    (tmp nil))
;; 	(dotimes (symb (cg-num cg))
;; 	  (let ((dl (determine-lookaheads cg kernels num symb (cdr all-items) ht-from ht-spon (+ (cg-num cg) 1))))
;; 	    (when dl (push dl tmp))))
;; 	(when tmp (push (cons kernels tmp) result))))
;;     ;; (format t "~a~%" result)
;;     ;; step 5.
;;     (let ((ht-lookaheads (make-hash-table :test #'equal)))
;;       ;; spontaneously lookaheads
;;       (dolist (kernels-and-num all-kernels)
;; 	(let ((kernels (car kernels-and-num))
;; 	      (num (cdr kernels-and-num)))
;; 	  (dolist (kern kernels)
;; 	    (setf (gethash (cons num kern) ht-lookaheads)
;; 		  (gethash (cons num kern) ht-spon)))))
;;       ;; lookaheads' propagation
;;       (let ((changed t))
;; 	(loop
;; 	  while changed
;; 	  do (setf changed nil)
;; 	     (dolist (kernels-and-num all-kernels)
;; 	       (let* ((kernels (car kernels-and-num))
;; 		      (num (cdr kernels-and-num)))
;; 		 (dolist (kernel kernels)
;; 		   (let ((old (copy-list (gethash (cons num kernel) ht-lookaheads))))
;; 		     (dolist (from (remove-if #'(lambda (x) (equal (cons num kernel) x)) (gethash (cons num kernel) ht-from)))
;; 		       (setf (gethash (cons num kernel) ht-lookaheads)
;; 			     (union (gethash (cons num kernel) ht-lookaheads)
;; 				    (gethash from ht-lookaheads)
;; 				    :test #'equal)))
;; 		     (unless (set-eq old (gethash (cons num kernel) ht-lookaheads))
;; 		       (setf changed t))))))))
;;      ;; make lalr1-kernels
;;       ;; (format t "******~%")
;;       ;; (format t "~a~%" all-kernels)
;;       ;; (format t "******~%")
;;       (dolist (kernels-and-num all-kernels all-lalr1-kernels)
;; 	(let ((kernels (car kernels-and-num))
;; 	      (num (cdr kernels-and-num))
;; 	      (lalr1-kernels nil))
;; 	  (dolist (kernel kernels)
;; 	    (push (cons kernel (gethash (cons num kernel) ht-lookaheads)) lalr1-kernels))
;; 	  (push (cons lalr1-kernels num) all-lalr1-kernels)))
;;       ;; step 6. from kernel to state
;;       ;; (format t "~a~%" all-lalr1-kernels)
;;       (let ((l (mapcar
;; 		#'(lambda (x)
;; 		    (cons 
;; 		     (lalr1-state-compress (lr1-closure cg (car x)))
;; 		     (cdr x)))
;; 		all-lalr1-kernels)))
;; 	(cons l (cdr all-items))))))

;; (defun lalr1-state-compress (state)
;;   (sort
;;    (foldl '()
;; 	  #'(lambda (items item)
;; 	      (if items
;; 		  (let ((sep-ls (separate
;; 				 #'(lambda (x) (equal (car item) (car x)))
;; 				 items)))
;; 		    (cons (cons (car item)
;; 				(union (cdr item) (cdaar sep-ls)))
;; 			  (cdr sep-ls)))
;; 		  (list item)))
;; 	  state)
;;    #'(lambda (x y) (< (caar x) (caar y))))
;;   )

;; (defun separate (predicate list &key (key #'identity))
;;   (labels ((f (x y)
;; 	     (if (funcall predicate (funcall key y))
;; 		 (cons (cons y (car x)) (cdr x))
;; 		 (cons (car x) (cons y (cdr x))))))
;;     (foldl (cons () ()) #'f list)))













;; Grammar samples
(defparameter *g1-f*
  (make-grammar "E"
		`((("E" "E" "+" "T") ,#'(lambda (x y z) (list "E" x (list y) z)))
		  (("E" "T") ,#'(lambda (x) (list "E" x)))
		  (("T" "T" "*" "F") ,#'(lambda (x y z) (list "T" x (list y) z)))
		  (("T" "F") ,#'(lambda (x) (list "T" x)))
		  (("F" "(" "E" ")") ,#'(lambda (x y z) (list "F" (list x) y (list z))))
		  (("F" "id")  ,#'(lambda (x) (list "F" (list x)))))))

(defparameter *g-arith*
  (make-grammar "E"
		`((("E" "E" "+" "E") ,#'(lambda (x y z) (list '"E" x (list y) z)) (10 . 5))
		  (("E" "E" "*" "E") ,#'(lambda (x y z) (list '"E" x (list y) z)) (20 . 15))
		  (("E" "(" "E" ")") ,#'(lambda (x y z) (list '"E" (list x) y (list z))) (10 . 5))
		  (("E" "id") ,#'(lambda (x) (list '"E" (list x))) (0 . 0)))))

(defparameter *g-reg*
  (make-grammar "Reg"
		`((("Reg" "ADD_rr") ,#'(lambda (x) (list "Reg" x)))
		  (("Reg" "ADD_cr") ,#'(lambda (x) (list "Reg" x)))
		  (("Reg" "MUL_rr") ,#'(lambda (x) (list "Reg" x)))
		  (("Reg" "LOAD_c") ,#'(lambda (x) (list "Reg" x)))
		  (("Reg" "reg") ,#'(lambda (x) (list "Reg" (list x))))
		  (("ADD_rr" "(" "add" "Reg" "Reg" ")") ,#'(lambda (x y z w u) (list "ADD_rr" (list x) (list y) z w (list u))))
		  (("ADD_cr" "(" "add" "const" "Reg" ")") ,#'(lambda (x y z w u) (list "ADD_cr" (list x) (list y) (list z) w (list u))) (0 . 0))
		  (("MUL_rr" "(" "mul" "Reg" "Reg" ")") ,#'(lambda (x y z w u) (list "MUL_rr" (list x) (list y) z w (list u))))
		  (("LOAD_c" "const") ,#'(lambda (x) (list "LOAD_c" (list x))) (3 . 1)))))
		  
(defparameter *g-reg-tiger*
  (make-grammar "S"
		`((("S" "Reg") (LAMBDA (X) (LIST "S" x)))
		  (("S" "Void") (LAMBDA (X) (LIST "S" x)))
		  (("Reg" "ADD") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "MUL") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "ADDI") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "LOAD") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "reg") (LAMBDA (X) (LIST "Reg" (list x))) (0 . 5))
		  (("Void" "STORE") (LAMBDA (X) (LIST "Void" x)))
		  (("Void" "MOVEM") (LAMBDA (X) (LIST "Void" x)))
		  (("ADD" "(" "Reg" "+" "Reg" ")") (LAMBDA (X Y Z W U) (LIST "ADD" (list x) y (list z) w (list u))) (0 . 30))
		  (("MUL" "(" "Reg" "*" "Reg" ")") (LAMBDA (X Y Z W U) (LIST "MUL" (list x) y (list z) w (list u))) (0 . 40))
		  (("ADDI" "(" "Reg" "+" "const" ")")
		   (LAMBDA (X Y Z W U) (LIST "ADDI" (list x) y (list z) (list w) (list u))) (50 . 50))
		  (("ADDI" "const") (LAMBDA (X) (LIST "ADDI" (list x))) (10. 10))
		  (("LOAD" "mem" "(" "Reg" "+" "const" ")")
		   (LAMBDA (X Y Z W U V) (LIST "LOAD" (list x) (list y) z (list w) (list u) (list v))) (60 . 60))
		  (("LOAD" "mem" "const") (LAMBDA (X Y) (LIST "LOAD" (list x) (list y))) (20 . 20))
		  (("LOAD" "mem" "Reg") (LAMBDA (X Y) (LIST "LOAD" (list x) y)) (0 . 15))
		  (("STORE" "move" "(" "mem" "(" "Reg" "+" "const" ")" "," "Reg" ")")
		   (LAMBDA (X Y Z W U V P Q R S N) (LIST "STORE" (list x) (list y) (list z) (list w) u (list v) (list p) (list q) (list r) s (list n)))
		   (0 . 100))
		  (("STORE" "move" "(" "mem" "const" "," "Reg" ")")
		   (LAMBDA (X Y Z W U V P) (LIST "STORE" (list x) (list y) (list z) (list w) (list u) v (list p)))
		   (0 . 80))
		  (("STORE" "move" "(" "mem" "Reg" "," "Reg" ")")
		   (LAMBDA (X Y Z W U V P) (LIST "STORE" (list x) (list y) (list z) w (list u) v (list p)))
		   (0 . 70))
		  (("MOVEM" "move" "(" "mem" "Reg" "," "mem" "Reg" ")")
		   (LAMBDA (X Y Z W U V P Q) (LIST "MOVEM" (list x) (list y) (list z) w (list u) (list v) p (list q)))
		   (0 . 90)
		   ))))

(defparameter *g-reg-tiger-inv*
  (make-grammar "S"
		`((("S" "Reg") (LAMBDA (X) (LIST "S" x)))
		  (("S" "Void") (LAMBDA (X) (LIST "S" x)))
		  (("Reg" "ADD") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "MUL") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "ADDI") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "LOAD") (LAMBDA (X) (LIST "Reg" x)))
		  (("Reg" "reg") (LAMBDA (X) (LIST "Reg" (list x))) (0 . -5))
		  (("Void" "STORE") (LAMBDA (X) (LIST "Void" x)))
		  (("Void" "MOVEM") (LAMBDA (X) (LIST "Void" x)))
		  (("ADD" "(" "Reg" "+" "Reg" ")") (LAMBDA (X Y Z W U) (LIST "ADD" (list x) y (list z) w (list u))) (0 . -30))
		  (("MUL" "(" "Reg" "*" "Reg" ")") (LAMBDA (X Y Z W U) (LIST "MUL" (list x) y (list z) w (list u))) (0 . -40))
		  (("ADDI" "(" "Reg" "+" "const" ")")
		   (LAMBDA (X Y Z W U) (LIST "ADDI" (list x) y (list z) (list w) (list u))) (50 . -50))
		  (("ADDI" "const") (LAMBDA (X) (LIST "ADDI" (list x))) (20. -10))
		  (("LOAD" "mem" "(" "Reg" "+" "const" ")")
		   (LAMBDA (X Y Z W U V) (LIST "LOAD" (list x) (list y) z (list w) (list u) (list v))) (60 . -60))
		  (("LOAD" "mem" "const") (LAMBDA (X Y) (LIST "LOAD" (list x) (list y))) (10 . -20))
		  (("LOAD" "mem" "Reg") (LAMBDA (X Y) (LIST "LOAD" (list x) y)) (0 . -15))
		  (("STORE" "move" "(" "mem" "(" "Reg" "+" "const" ")" "," "Reg" ")")
		   (LAMBDA (X Y Z W U V P Q R S N) (LIST "STORE" (list x) (list y) (list z) (list w) u (list v) (list p) (list q) (list r) s (list n)))
		   (0 . -100))
		  (("STORE" "move" "(" "mem" "const" "," "Reg" ")")
		   (LAMBDA (X Y Z W U V P) (LIST "STORE" (list x) (list y) (list z) (list w) (list u) v (list p)))
		   (0 . -80))
		  (("STORE" "move" "(" "mem" "Reg" "," "Reg" ")")
		   (LAMBDA (X Y Z W U V P) (LIST "STORE" (list x) (list y) (list z) w (list u) v (list p)))
		   (0 . -70))
		  (("MOVEM" "move" "(" "mem" "Reg" "," "mem" "Reg" ")")
		   (LAMBDA (X Y Z W U V P Q) (LIST "MOVEM" (list x) (list y) (list z) w (list u) (list v) p (list q)))
		   (0 . -90)
		   ))))

(defparameter *input-g-reg-tiger-1*
  '("move" "(" "mem" "reg" "," "(" "reg" "+" "(" "const" "*" "reg" ")" ")" ")"))
(defparameter *input-g-reg-tiger-2*
  '("move" "(" "mem" "(" "mem" "(" "reg" "+" "const" ")" "+" "(" "reg" "*" "const" ")" ")" "," "mem" "(" "reg" "+" "const" ")" ")" ))

;; (defparameter *g-arith*
;;   (make-grammar 'E
;; 		`(((E E #\+ E) ,#'(lambda (x y z) (list 'E x (list y) z)) (10 . 5))
;; 		  ((E E #\* E) ,#'(lambda (x y z) (list 'E x (list y) z)) (20 . 15))
;; 		  ((E #\( E #\)) ,#'(lambda (x y z) (list 'E (list x) y (list z))) (10 . 5))
;; 		  ((E id) ,#'(lambda (x) (list 'E (list x))) (0 . 0)))))

(defparameter *g-if*
  (make-grammar 'S
		`(((S if b then S) ,#'(lambda (x y z w) (list 'S (list x) (list y) (list z) w)) (10 . 5))
		  ((S if b then S else S) ,#'(lambda (x y z w u v) (list 'S (list x) (list y) (list z) w (list u) v)) (20 . 15)))))

(defparameter *g-minus*
  (make-grammar 'E
		`(((E E #\+ E) ,#'(lambda (x y z) (list 'E x (list y) z)) (10 . 5))
		  ((E E #\- E) ,#'(lambda (x y z) (list 'E x (list y) z)) (10 . 5))
		  ((E #\- E) ,#'(lambda (x y) (list 'E (list x) y)) (10 . 5))
		  ((E id) ,#'(lambda (x) (list 'E (list x))) (0 . 0)))))

(defparameter *g1*
  (make-grammar "E"
             '(("E" "E" "+" "T") ("E" "T") ("T" "T" "*" "F") ("T" "F")
	       ("F" "(" "E" ")") ("F" "id")))) 
(defparameter *g2*
  (make-grammar "S" '(("S" "id") ("S" "S" "," "id")))) 
(defparameter *g3*
  (make-grammar "S" '(("S" "A") ("S" "S" "A")))) 
(defparameter *g4*
  (make-grammar "S" '(("S" "A") ("S" "A" "S")))) 
(defparameter *g5*
  (make-grammar "S" '(("S" "int" "L") ("L" "id") ("L" "L" "," "id")))) 
(defparameter *g6*
  (make-grammar "S"
             '(("S" "L" "=" "R") ("S" "R") ("L" "*" "R") ("L" "id")
	       ("R" "L")))) 
(defparameter *g7*
  (make-grammar "S" '(("S" "C" "C") ("C" "B" "C") ("C" "D")))) 
(defparameter *g8*
  (make-grammar "S"
             '(("S" "L" "=" "E") ("L" "id") ("L" "R" "^" "id")
              ("E" "E" "+" "R") ("E" "R") ("E" "@" "L") ("R" "id")))) 
(defparameter *G9*
  (make-grammar "A" '(("A" "b" "B") ("B" "c" "C") ("C" "d" "A") ("A" "a"))))
(defparameter *G10*
  (make-grammar "A" '(("A" "B" "C" "D" "A") ("B") ("C") ("D") ("A" "a")))) 
(defparameter *g11*
  (make-grammar "S" '(("S" "A" "B") ("A") ("B") ("A" "x"))))


(defparameter *cbnf-symbol-alist*
  '(	  (|translation_unit| (|external_decl|)
			      (|translation_unit| |external_decl|))
	  (|external_decl| (|function_definition|) (|decl|))
	  (|function_definition|
	   (|decl_specs| |declarator| |decl_list| |compound_stat|)
	   (|declarator| |decl_list| |compound_stat|)
	   (|decl_specs| |declarator| |compound_stat|)
	   (|declarator| |compound_stat|))
	  (|decl| (|decl_specs| |init_declarator_list| |';'|)
		  (|decl_specs| |';'|))
	  (|decl_list| (|decl|) (|decl_list| |decl|))
	  (|decl_specs| (|storage_class_spec| |decl_specs|)
			(|storage_class_spec|) (|type_spec| |decl_specs|) (|type_spec|)
			(|type_qualifier| |decl_specs|) (|type_qualifier|))
	  (|storage_class_spec| (|'auto'|) (|'register'|) (|'static'|)
				(|'extern'|) (|'typedef'|))
	  (|type_spec| (|'void'|) (|'char'|) (|'short'|) (|'int'|) (|'long'|)
		       (|'float'|) (|'double'|) (|'signed'|) (|'unsigned'|)
		       (|struct_or_union_spec|) (|enum_spec|) (|typedef_name|))
	  (|type_qualifier| (|'const'|) (|'volatile'|))
	  (|struct_or_union_spec|
	   (|struct_or_union| |id| |'{'| |struct_decl_list| |'}'|)
	   (|struct_or_union| |'{'| |struct_decl_list| |'}'|)
	   (|struct_or_union| |id|))
	  (|struct_or_union| (|'struct'|) (|'union'|))
	  (|struct_decl_list| (|struct_decl|)
			      (|struct_decl_list| |struct_decl|))
	  (|init_declarator_list| (|init_declarator|)
				  (|init_declarator_list| |','| |init_declarator|))
	  (|init_declarator| (|declarator|) (|declarator| |'='| |initializer|))
	  (|struct_decl| (|spec_qualifier_list| |struct_declarator_list| |';'|))
	  (|spec_qualifier_list| (|type_spec| |spec_qualifier_list|)
				 (|type_spec|) (|type_qualifier| |spec_qualifier_list|)
				 (|type_qualifier|))
	  (|struct_declarator_list| (|struct_declarator|)
				    (|struct_declarator_list| |','| |struct_declarator|))
	  (|struct_declarator| (|declarator|) (|declarator| |':'| |const_exp|)
			       (|':'| |const_exp|))
	  (|enum_spec| (|'enum'| |id| |'{'| |enumerator_list| |'}'|)
		       (|'enum'| |'{'| |enumerator_list| |'}'|) (|'enum'| |id|))
	  (|enumerator_list| (|enumerator|)
			     (|enumerator_list| |','| |enumerator|))
	  (|enumerator| (|id|) (|id| |'='| |const_exp|))
	  (|declarator| (|pointer| |direct_declarator|) (|direct_declarator|))
	  (|direct_declarator| (|id|) (|'('| |declarator| |')'|)
			       (|direct_declarator| |'['| |const_exp| |']'|)
			       (|direct_declarator| |'['| |']'|)
			       (|direct_declarator| |'('| |param_type_list| |')'|)
			       (|direct_declarator| |'('| |id_list| |')'|)
			       (|direct_declarator| |'('| |')'|))
	  (|pointer| (|'*'| |type_qualifier_list|) (|'*'|)
		     (|'*'| |type_qualifier_list| |pointer|) (|'*'| |pointer|))
	  (|type_qualifier_list| (|type_qualifier|)
				 (|type_qualifier_list| |type_qualifier|))
	  (|param_type_list| (|param_list|) (|param_list| |','| |'...'|))
	  (|param_list| (|param_decl|) (|param_list| |','| |param_decl|))
	  (|param_decl| (|decl_specs| |declarator|)
			(|decl_specs| |abstract_declarator|) (|decl_specs|))
	  (|id_list| (|id|) (|id_list| |','| |id|))
	  (|initializer| (|assignment_exp|) (|'{'| |initializer_list| |'}'|)
			 (|'{'| |initializer_list| |','| |'}'|))
	  (|initializer_list| (|initializer|)
			      (|initializer_list| |','| |initializer|))
	  (|type_name| (|spec_qualifier_list| |abstract_declarator|)
		       (|spec_qualifier_list|))
	  (|abstract_declarator| (|pointer|)
				 (|pointer| |direct_abstract_declarator|)
				 (|direct_abstract_declarator|))
	  (|direct_abstract_declarator| (|'('| |abstract_declarator| |')'|)
					(|direct_abstract_declarator| |'['| |const_exp| |']'|)
					(|'['| |const_exp| |']'|) (|direct_abstract_declarator| |'['| |']'|)
					(|'['| |']'|)
					(|direct_abstract_declarator| |'('| |param_type_list| |')'|)
					(|'('| |param_type_list| |')'|)
					(|direct_abstract_declarator| |'('| |')'|) (|'('| |')'|))
	  (|typedef_name| (|id|))
	  (|stat| (|labeled_stat|) (|exp_stat|) (|compound_stat|)
		  (|selection_stat|) (|iteration_stat|) (|jump_stat|))
	  (|labeled_stat| (|id| |':'| |stat|)
			  (|'case'| |const_exp| |':'| |stat|) (|'default'| |':'| |stat|))
	  (|exp_stat| (|exp| |';'|) (|';'|))
	  (|compound_stat| (|'{'| |decl_list| |stat_list| |'}'|)
			   (|'{'| |stat_list| |'}'|) (|'{'| |decl_list| |'}'|) (|'{'| |'}'|))
	  (|stat_list| (|stat|) (|stat_list| |stat|))
	  (|selection_stat| (|'if'| |'('| |exp| |')'| |stat|)
			    (|'if'| |'('| |exp| |')'| |stat| |'else'| |stat|)
			    (|'switch'| |'('| |exp| |')'| |stat|))
	  (|iteration_stat| (|'while'| |'('| |exp| |')'| |stat|)
			    (|'do'| |stat| |'while'| |'('| |exp| |')'| |';'|)
			    (|'for'| |'('| |exp| |';'| |exp| |';'| |exp| |')'| |stat|)
			    (|'for'| |'('| |exp| |';'| |exp| |';'| |')'| |stat|)
			    (|'for'| |'('| |exp| |';'| |';'| |exp| |')'| |stat|)
			    (|'for'| |'('| |exp| |';'| |';'| |')'| |stat|)
			    (|'for'| |'('| |';'| |exp| |';'| |exp| |')'| |stat|)
			    (|'for'| |'('| |';'| |exp| |';'| |')'| |stat|)
			    (|'for'| |'('| |';'| |';'| |exp| |')'| |stat|)
			    (|'for'| |'('| |';'| |';'| |')'| |stat|))
	  (|jump_stat| (|'goto'| |id| |';'|) (|'continue'| |';'|)
		       (|'break'| |';'|) (|'return'| |exp| |';'|) (|'return'| |';'|))
	  (|exp| (|assignment_exp|) (|exp| |','| |assignment_exp|))
	  (|assignment_exp| (|conditional_exp|)
			    (|unary_exp| |assignment_operator| |assignment_exp|))
	  (|assignment_operator| (|'='|) (|'*='|) (|'/='|) (|'%='|) (|'+='|)
				 (|'-='|) (|'<<='|) (|'>>='|) (|'&='|) (|'^='|) (|'\|='|))
	  (|conditional_exp| (|logical_or_exp|)
			     (|logical_or_exp| |'?'| |exp| |':'| |conditional_exp|))
	  (|const_exp| (|conditional_exp|))
	  (|logical_or_exp| (|logical_and_exp|)
			    (|logical_or_exp| |'\|\|'| |logical_and_exp|))
	  (|logical_and_exp| (|inclusive_or_exp|)
			     (|logical_and_exp| |'&&'| |inclusive_or_exp|))
	  (|inclusive_or_exp| (|exclusive_or_exp|)
			      (|inclusive_or_exp| |'\|'| |exclusive_or_exp|))
	  (|exclusive_or_exp| (|and_exp|) (|exclusive_or_exp| |'^'| |and_exp|))
	  (|and_exp| (|equality_exp|) (|and_exp| |'&'| |equality_exp|))
	  (|equality_exp| (|relational_exp|)
			  (|equality_exp| |'=='| |relational_exp|)
			  (|equality_exp| |'!='| |relational_exp|))
	  (|relational_exp| (|shift_expression|)
			    (|relational_exp| |'<'| |shift_expression|)
			    (|relational_exp| |'>'| |shift_expression|)
			    (|relational_exp| |'<='| |shift_expression|)
			    (|relational_exp| |'>='| |shift_expression|))
	  (|shift_expression| (|additive_exp|)
			      (|shift_expression| |'<<'| |additive_exp|)
			      (|shift_expression| |'>>'| |additive_exp|))
	  (|additive_exp| (|mult_exp|) (|additive_exp| |'+'| |mult_exp|)
			  (|additive_exp| |'-'| |mult_exp|))
	  (|mult_exp| (|cast_exp|) (|mult_exp| |'*'| |cast_exp|)
		      (|mult_exp| |'/'| |cast_exp|) (|mult_exp| |'%'| |cast_exp|))
	  (|cast_exp| (|unary_exp|) (|'('| |type_name| |')'| |cast_exp|))
	  (|unary_exp| (|postfix_exp|) (|'++'| |unary_exp|) (|'--'| |unary_exp|)
		       (|unary_operator| |cast_exp|) (|'sizeof'| |unary_exp|)
		       (|'sizeof'| |'('| |type_name| |')'|))
	  (|unary_operator| (|'&'|) (|'*'|) (|'+'|) (|'-'|) (|'~'|) (|'!'|))
	  (|postfix_exp| (|primary_exp|) (|postfix_exp| |'['| |exp| |']'|)
			 (|postfix_exp| |'('| |argument_exp_list| |')'|)
			 (|postfix_exp| |'('| |')'|) (|postfix_exp| |'.'| |id|)
			 (|postfix_exp| |'->'| |id|) (|postfix_exp| |'++'|)
			 (|postfix_exp| |'--'|))
	  (|primary_exp| (|id|) (|const|) (|string|) (|'('| |exp| |')'|))
	  (|argument_exp_list| (|assignment_exp|)
			       (|argument_exp_list| |','| |assignment_exp|))
	  (|const| (|int_const|) (|char_const|) (|float_const|)
		   (|enumeration_const|))))
(defparameter *cbnf-symbol* (apply #'append (mapcar #'(lambda (l) (mapcar #'(lambda (x) (cons (car l) x)) (cdr l))) *cbnf-symbol-alist*)))

(defparameter *cbnf*
  (make-grammar "translation_unit" (mapcar #'(lambda (x) (mapcar #'symbol-name x)) *cbnf-symbol*)))

