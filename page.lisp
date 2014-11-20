;; ================================================================ ;;
;;                                                                  ;;
;;   PaGe :: Parser Generator for LALR(1)-grammar by Common Lisp    ;;
;;                                                                  ;;
;; ================================================================ ;;

(defpackage #:page
  (:use #:common-lisp #:pagen)
  (:import-from #:common-lisp)
  (:shadowing-import-from
   #:pagen
   ;; Utilities
   #:<@ #:==> #:split 
   ;; grammar
   #:make-rule
   #:*grammar*
   #:gr-t-num
   ;; parser
   #:disambiguator
   #:make-action-table
   #:def-defparser
   #:parse
   #:simple-reader
   )
  )

(in-package #:page)

;; ================================================================
;;  PaGe: a Wrapper of PaGEn for Normal Use
;; ================================================================

(defmacro defparser (name &body body)
  (destructuring-bind (pg d lex) (interpret body)
    (multiple-value-bind
	  (grammar symbol-array symbol-ht f-array rp-sp) (pg->gr pg)
      (let* ((rp-sp (or (disambiguator grammar (disambiguation symbol-ht d)) rp-sp))
	     (lex (or lex
		      #'(lambda (str) (simple-page-lexer str (gr-t-num grammar) symbol-ht)))))
	(def-defparser name grammar lex f-array rp-sp (<@ 'aref symbol-array))))))

(defun disambiguation (sym-ht info-list)
  (mapcar (<@ 'mapcar #'(lambda (x) (gethash x sym-ht x))) info-list))

(defun simple-page-lexer (str eof ht)
  (simple-reader
   (mapcar
    #'(lambda (x) (gethash x ht))
    ;; #'(lambda (x)
    ;; 	(if (position (char x 0) "0123456789")
    ;; 	    (cons (gethash "id" ht)
    ;; 		  (- (char-int (char x 0)) 48))
    ;; 	    (gethash x ht)))
    (separate-by-space str)) :eof eof))

(defun separate-by-space (string &optional (start 0))
  (let ((pos (position #\Space string :start start)))
    (if pos
	(cons (subseq string start pos)
	      (separate-by-space string (+ pos 1)))
	(list (subseq string start)))))


;; --------------------------------
;;  1. Data Structures and Translator
;; --------------------------------

(defstruct (page-grammar
	    (:conc-name pg-)
	    (:constructor %make-page-grammar (start rules)))
  start (rules () :type list))

(defstruct (page-rule
	    (:conc-name pr-)
	    (:constructor %make-page-rule (lhs rhs act rp sp)))
  (id -1 :type fixnum) lhs rhs act (rp 0 :type fixnum) (sp 0 :type fixnum))

(defun pg->gr (pg &key (start "$accept") (eof "$eof") (unknown-symbol "#"))
  "Translate from page-grammar into grammar (for PaGEn)"
  (let* ((prs (pg-rules pg))
	 (nt-symbols (remove-duplicates (mapcar #'pr-lhs prs) :test #'equal))
	 (t-symbols (set-difference
		     (remove-duplicates (apply #'append (mapcar #'pr-rhs prs)):test #'equal)
		     nt-symbols :test #'equal))
	 (sym-alist (list (cons start 0)))
	 (nt-num 1)
	 (t-num 0))
    ;; 1. symbol and num's relation
    (loop
      for symbol in nt-symbols
      do (push (cons symbol nt-num) sym-alist)
	 (incf nt-num))
    (setf t-num nt-num)
    (loop
      for symbol in t-symbols
      do (push (cons symbol t-num) sym-alist)
	 (incf t-num))
    (push (cons eof t-num) sym-alist)
    (push (cons unknown-symbol (+ t-num 1)) sym-alist)
    ;; 
    (let ((sym-array (make-array (+ t-num 2) :initial-element nil))
	  (sym-ht (make-hash-table :test 'equal))
	  (f-array (make-array (+ (length prs) 1) :initial-element nil))
	  (rp-array (make-array (+ (length prs) 1) :initial-element 0))
	  (sp-array (make-array (+ (length prs) 1) :initial-element 0))
	  (rules nil)
	  (rule-num 0))
      (dolist (sym-num sym-alist)
	(setf (gethash (car sym-num) sym-ht) (cdr sym-num))
	(setf (aref sym-array (cdr sym-num)) (car sym-num)))
      (dolist (pr (sort prs #'< :key #'(lambda (x) (gethash (pr-lhs x) sym-ht))))
	(incf rule-num)
	(setf (aref f-array rule-num) (pr-act pr))
	(setf (aref rp-array rule-num) (pr-rp pr))
	(setf (aref sp-array rule-num) (pr-sp pr))
	(let ((lhs (cdr (assoc (pr-lhs pr) sym-alist :test #'equal)))
	      (rhs (mapcar #'(lambda (x) (cdr (assoc x sym-alist :test #'equal))) (pr-rhs pr))))
	  (push (make-rule rule-num lhs rhs) rules)))
      (setf rules
	    (cons (make-rule 0 0 (list (cdr (assoc (pg-start pg) sym-alist :test #'equal)) t-num))
		  (reverse rules)))
      (setf (aref f-array 0)
	    (let ((x (gensym))
		  (y (gensym)))
	      (eval `(lambda (,x ,y) ,(list 'list start x (list 'list y))))))
      (setf (aref rp-array 0) 0)
      (setf (aref sp-array 0) 0)
      
      (let ((gr (make-grammar 0 rules nt-num t-num)))
	(values gr sym-array sym-ht f-array (cons rp-array sp-array))))))



;; --------------------------------
;;  2. Input Interpretation
;; --------------------------------

(defun interpret (body)
  "interpret defparser's inputs"
  (let ((g nil) (d nil) (lex nil))
    (dolist (form body (list g d lex))
      (when (listp form)
	(case (car form)
	  (:lexer (setf lex (second form)))
	  (:grammar (setf g (make-page-grammar (cdr form))))
	  (:grammar-var (setf g (make-page-grammar (eval (second form)))))
	  (:disambiguation (setf d (cdr form)))
	  (otherwise nil))))))

;; 
;; 2.1 constucting page-grammar
;; 
(eval-when (compile eval load)
  (proclaim '(special *mpg-max-len* *mpg-symbols* *mpg-nt-symbols*)))

(defun make-page-grammar (s-r)
  (let ((*mpg-symbols* nil)
	(*mpg-max-len* 0)
	(*mpg-nt-symbols* nil))
    (destructuring-bind (start . rules) s-r
      (setf rules (mapcar #'normalize-pr rules))
      (dolist (pr rules) (pushnew (car (first pr)) *mpg-nt-symbols* :test #'equal))
      (%make-page-grammar start (mapcar #'make-page-rule rules)))))

(defun normalize-pr (pr)
  (cond
    ((stringp pr)
     (list (remove "->" (split (<@ 'eq #\Space) pr) :test #'string=) '() '()))
    ((stringp (first pr))
     (list (remove "->" (split (<@ 'eq #\Space) (first pr)) :test #'string=)
	   (second pr) (third pr)))
    (t pr)))

(defun make-page-rule (pr)
  (let ((lhs (car (first pr)))
	(rhs (cdr (first pr)))
	(act nil)
	(rp 0)
	(sp 0))
    (if (eq 'function (car (second pr)))
	(progn
	  (setf act (eval (second pr)))
	  (when (consp (third pr))
	    (setf rp (car (third pr)) sp (cdr (third pr)))))
	(progn
	  (setf act (eval (default-act (first pr))))
	  (when (consp (second pr))
	    (setf rp (car (second pr)) sp (cdr (second pr))))))
    (%make-page-rule lhs rhs act rp sp)))

(defun default-act (rule)
  (let ((len (length (cdr rule))))
    (when (< *mpg-max-len* len)
      (dotimes (i (- len *mpg-max-len*))
	(push (gensym) *mpg-symbols*))
      (setf *mpg-max-len* len))
    (let ((args (take len *mpg-symbols*))
	  (body nil))
      (loop
	for symbol in args
	for x in (cdr rule)
	do (if (member x *mpg-nt-symbols* :test #'equal)
	       (push symbol body)
	       (push `(list ,symbol) body)))
      `(lambda ,args ,(list* 'list (car rule) (reverse body))))))

;; translate grammar into canonical-form for using PaGEn.
;; e.g. (E, {E -> E + E, E -> E * E}) -> (1 (0 1 4) (1 1 2 1) (1 1 3 1) 2 4) + symbol table


;; constructing
		  
(defun print-conflict (conflict sym-arr rule-arr)
  (destructuring-bind ((state . la) . info-list) conflict
    (format t "in STATE [~a] looking [~a]:~%" state (aref sym-arr la))
    (dolist (info info-list)
      (print-info info sym-arr rule-arr))
    ;; (format t "  ~a~%  ~a~%" (car info1) (car info2))
    (format t "~%")
    ))

(defun print-info (info sym-arr rule-arr)
  (let ((type (car info)))
    (cond ((eq type :shift-action)
	   (format t "* SHIFT :: shift to STATE [~a]~%~{    ~a~%~}"
		   (cddr info)
		   (==> (cadr info)
		     (mapcar (<@ 'aref rule-arr))
		     (mapcar #'(lambda (rule)
				 (format nil "~a -> ~{~a~^ ~}"
					 (aref sym-arr (rule-lhs rule))
					 (mapcar (<@ 'aref sym-arr) (rule-rhs rule)))))
		     )))
	  ((eq type :reduce-action)
	   (let ((rule (cdr info)))
	     (format t "* REDUCE ::~%    ~a -> ~{~a~^ ~}~%"
		     (aref sym-arr (rule-lhs rule))
		     (mapcar (<@ 'aref sym-arr) (rule-rhs rule)))))
	  
	  (t (format t "~a~%" (equal 'shift (car info)))))))

;; --------------------------------
;;  Utilities

;; tree
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
	 n))))

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
      
(defun write-dot-ast (fname ast)
  (with-open-file (ost fname :direction :output :if-exists :supersede)
    (dot-tree (dfs-tree ast) ost)))

;; macro (not used)
(eval-when (compile load eval)
  ;; (defmacro uncurry (f p)
  ;;   `(,f (car ,p) (cdr ,p)))

  (defmacro fun (&rest body)
    (let ((apos (or (position '=> body) 0)))
    `#'(lambda ,(take apos body) ,@(drop (+ apos 1) body))))

  (defmacro define (&rest body)
    (let* ((apos (or (position ':= body) 0))
	   (pre (take apos body))
	   (post (drop (+ apos 1) body))
	   (name (if (< 0 apos) (car pre) (gensym)))
	   (args (if (< 0 apos) (cdr pre) pre)))
      `(defun ,name ,args ,@post)))
)

