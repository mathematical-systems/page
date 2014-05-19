;; ================================================================ ;;
;;                                                                  ;;
;;   PaGe :: Parser Generator for LALR(1)-grammar by Common Lisp    ;;
;;                                                                  ;;
;; ================================================================ ;;

(in-package #:page)
(export '(cg-parse canonicalize canonicalize-f))

(defstruct (grammar
	    (:constructor make-grammar (start rules)))
  start (rules () :type list))


(defparameter *cg-num-to-symbol-array* :undefined)
(defparameter *cg-symbol-to-num-alist* :undefined)
(defparameter *cg-unknown-symbol* :undefined)

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
      ;; (setf rules (sort rules #'< :key #'(lambda (rule) (length (rule-rhs rule)))))
      ;; (loop for i from 0 to (- (length rules) 1)
      ;; 	    for rule in rules
      ;; 	    do (setf (rule-sp rule) i (rule-rp rule) i))
      ;; replace symbol to index
      (setf *cg-num-to-symbol-array* symbol-array
	    *cg-symbol-to-num-alist* symbol-alist
	    *cg-unknown-symbol* unknown-symbol)
      (make-canonical-grammar
       (cdr (assoc (grammar-start grammar) symbol-alist :test #'equal))
       rules nt-num num))))

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
      (setf *cg-num-to-symbol-array* symbol-array
	    *cg-symbol-to-num-alist* symbol-alist
	    *cg-unknown-symbol* unknown-symbol)
      (make-canonical-grammar
       (cdr (assoc (grammar-start grammar) symbol-alist :test #'equal))
       rules nt-num num))))


(defun cg-symbol (n &key (unknown t))
  ;; (declare (canonical-grammar cg))
  (let ((len (length *cg-num-to-symbol-array*)))
    (if (and (<= 0 n) (< n len))
	(aref *cg-num-to-symbol-array* n)
	(when unknown *cg-unknown-symbol*))))

(defun cg-rule-symbol (cg rule)
  (cons (cg-symbol (rule-lhs rule))
	(mapcar #'(lambda (x) (cg-symbol x)) (rule-rhs rule))))

(defun cg-print-item (cg item &key (stream t))
  (format stream "~S -> ~{~S~^ ~} .. ~{~S~^ ~}"
	  (cg-symbol (item-lhs item))
	  (mapcar #'(lambda (x) (cg-symbol x)) (item-pre item))
	  (mapcar #'(lambda (x) (cg-symbol x)) (item-suc item))))

(defun cg-print-state (cg state &key (stream t))
  (format stream "~{[~a]~%~}~{~a~^~%~}"
	  (mapcar #'(lambda (x) (cg-print-item cg x :stream nil)) (state-items state))
	  (mapcar #'(lambda (x) (cons (cg-symbol (car x)) (cdr x))) (state-gotos state))
	  ))


(defun cg-print-reads (parser)
  (dolist (r (parser-reads parser))
    (destructuring-bind (dom . cod) r
      (format t "~a reads ~{~a~^,~}~%"
	      (cons (car dom) (cg-symbol (parser-grammar parser) (cdr dom)))
	      (mapcar #'(lambda (x) (cons (car x) (cg-symbol (parser-grammar parser) (cdr x)))) cod)))))


(defun cg-print-lalr1-item (cg lalr1-item &optional (stream t))
  (format stream "[~a ;; ~{~S~^,~}]"
	  (cg-print-item cg (lalr1-item-body lalr1-item) :stream nil)
	  (mapcar #'(lambda (x) (cg-symbol x)) (lalr1-item-la-set lalr1-item))))

(defun cg-print-lalr1-state (cg lalr1-state &optional (stream t))
  (format stream "~{~a~%~}~{~S~^~%~}"
	  (mapcar #'(lambda (x) (cg-print-lalr1-item cg x nil)) (lalr1-state-items lalr1-state))
	  (mapcar #'(lambda (c) (cons (cg-symbol (car c)) (cdr c))) (lalr1-state-gotos lalr1-state))))

(defun print-lalr1-parser (lalr1-parser &optional (stream t))
  (dotimes (i (lalr1-parser-state-num lalr1-parser))
    (format stream ":state(~a):~%~a~%" i (cg-print-lalr1-state (lalr1-parser-grammar lalr1-parser) (lalr1-parser-state lalr1-parser i) nil))))

(defun cg-print-configuration (cg configuration stream)
  (format stream "[~{~a~^ -- ~a -> ~}]~{~a~^ ~}~%"
	  (p-apply (reverse (configuration-states configuration)) #'(lambda (x) (cg-symbol x)) #'oddp)
	  (mapcar #'(lambda (x) (cg-symbol x)) (configuration-symbols configuration))))

(defun dot-lalr1-parse-table (lalr1-parser &key (name "lalr1") (stream t))
  (let ((cg (lalr1-parser-grammar lalr1-parser)))
    (labels ((f (item)
	       (let ((body (lalr1-item-body item)))
		 (if (lalr1-item-la-set item)
		     (format nil "[~a\\ -\\>\\ ~{~a~^\\ ~}・\\ ~{~a~^\\ ~}\\ ,\\ ~{~a~^\\ ~}]"
			     (cg-symbol (item-lhs body))
			     (mapcar #'(lambda (x) (cg-symbol x)) 
				     (take (item-position body) (item-rhs body)))
			     (mapcar #'(lambda (x) (cg-symbol x))
				     (drop (item-position body) (item-rhs body)))
			     (mapcar #'(lambda (x) (cg-symbol x)) 
				     (lalr1-item-la-set item)))
		     (format nil "[~a\\ -\\>\\ ~{~a~^\\ ~}・\\ ~{~a~^\\ ~}]\\ "
			 (cg-symbol (item-lhs body))
			 (mapcar #'(lambda (x) (cg-symbol x)) 
				 (take (item-position body) (item-rhs body)))
			 (mapcar #'(lambda (x) (cg-symbol x))
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
						  (format nil "  ~a -> ~a [label=\"~a\"];" i (cdr c) (cg-symbol (car c))))
					      (lalr1-state-gotos (lalr1-parser-state lalr1-parser i)))
				      )
			      str))
		      (push "}" str)
		      (reverse str)))))))

(defun write-dot-lalr1-pt (fname parser)
  (with-open-file (ost fname :direction :output :if-exists :supersede)
    (dot-lalr1-parse-table parser :stream ost)))

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

(defun cg-parse (parser sequence &key (with-eof nil) (dump nil))
  (let ((input (mapcar #'(lambda (symbol) (or (cdr (assoc symbol *cg-symbol-to-num-alist* :test #'equal)) -1)) sequence)))
    (parse parser input :with-eof with-eof :dump dump :symbol-printer #'cg-symbol)))
