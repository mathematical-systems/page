;; 
;;  pagen-test: test scripts for pagen.lisp by lisp-unit
;; =================================================================


(defpackage #:pagen-test
  (:use #:common-lisp #:lisp-unit #:pagen)
  (:shadowing-import-from #:pagen
			  *grammar*
			  #:make-rule
			  #:make-grammar
			  #:gr-nullable
			  #:gr-head)
  ;; (:import-from #:pagen #:make-grammar #:gr-nullable #:gr-head)
  )

(in-package #:pagen-test)

;; 
;;  0. Grammars for Testing
;; --------------------------------

;; canonicalized-grammars

(defparameter *gr1*
  (make-grammar 0
		(list (make-rule 0 0 '(1 9))
		      (make-rule 1 1 '(1 8 2))
		      (make-rule 2 1 '(2))
		      (make-rule 3 2 '(2 7 3))
		      (make-rule 4 2 '(3))
		      (make-rule 5 3 '(6 1 5))
		      (make-rule 6 3 '(4)))
		4
		9))

(defparameter *gr1-symbols*
  (make-array 10
	      :initial-contents 
	      '("S" "E" "T" "F" "id" ")" "(" "*" "+" "$")))

(defun print-by (arr l)
  (mapcar #'(lambda (x) (aref arr x)) l))


(defparameter *gr2*
  (make-grammar 0
		(list (make-rule 0 0 '(1 6))
		      (make-rule 1 1 '(2 3 4 1))
		      (make-rule 2 2 '())
		      (make-rule 3 3 '())
		      (make-rule 4 4 '())
		      (make-rule 5 1 '(5)))
		5 6))

(defparameter *gr2-symbols*
  (make-array 7
	      :initial-contents 
	      '("S" "A" "B" "C" "D" "a" "$")))

(defparameter *gr3*
  (make-grammar 0
		(list (make-rule 0 0 '(1 10))
		      (make-rule 1 1 '(1 9 1))
		      (make-rule 2 1 '(1 8 1))
		      (make-rule 3 1 '(7 1 6))
		      (make-rule 4 1 '(5))
		      (make-rule 5 1 '(3 1 2 1 4 1))
		      (make-rule 6 1 '(3 1 2 1)))
		2 10))

(defparameter *gr3-symbols*
  (make-array 11
	      :initial-contents 
	      '("S" "E" "then" "if" "else" "id" ")" "(" "*" "+" "$")))


(defparameter *gr-example*
  (make-grammar 0
		(list (make-rule 0 0 '(2 20))
		      (make-rule 1 2 '(2 19 2))
		      (make-rule 2 2 '(2 18 2))
		      (make-rule 3 2 '(2 17 2))
		      (make-rule 4 2 '(2 16 2))
		      (make-rule 5 2 '(8 2 7))
		      (make-rule 6 2 '(15))
		      (make-rule 7 1 '(2 14 2))
		      (make-rule 8 1 '(2 13 2))
		      (make-rule 9 1 '(2 12 2))
		      (make-rule 10 1 '(1 11 1))
		      (make-rule 11 1 '(1 10 1))
		      (make-rule 12 1 '(9 1))
		      (make-rule 13 1 '(8 1 7))
		      (make-rule 14 1 '(6))
		      (make-rule 15 2 '(5 1 4 2))
		      (make-rule 16 2 '(5 1 4 2 3 2))
		      )
		3 20))

(defparameter *gr-example-symbols*
  (make-array 21
	      :initial-contents 
	      '("$accept" "B" "E" "else" "then" "if" "bool"
		")" "(" "not" "or" "and" ">" "<"
		"=" "id" "/" "-" "*" "+" "$eof")))

;; (defparameter *g-example*
;;   (make-grammar "E"
;; 		'(
;; 		  ("E" "E" "+" "E")
;; 		  ("E" "E" "*" "E")
;; 		  ("E" "E" "-" "E")
;; 		  ("E" "E" "/" "E")
;; 		  ("E" "(" "E" ")")
;; 		  ("E" "id")
;; 		  ("B" "E" "=" "E")
;; 		  ("B" "E" "<" "E")
;; 		  ("B" "E" ">" "E")
;; 		  ("B" "B" "/\\" "B")
;; 		  ("B" "B" "\\/" "B")
;; 		  ("B" "~" "B")
;; 		  ("B" "(" "B" ")")
;; 		  ("B" "bool")
;; 		  ("E" "if" "B" "then" "E")
;; 		  ("E" "if" "B" "then" "E" "else" "E")
;; 		  )))


;; (defparameter *G10*
;;   (make-grammar "A" '(("A" "B" "C" "D" "A") ("B") ("C") ("D") ("A" "a")))) 


(defun include-p (l1 l2)
  (every #'(lambda (x1) (member x1 l2)) l1))

(defun perm-equal (l1 l2)
  (and (= (length l1) (length l2)) (include-p l1 l2) (include-p l2 l1)))


;; 
;;  1. Computing grammar's property
;; --------------------------------

(define-test gr-nullability
  (let ((*grammar* *gr1*))
    (dotimes (i 9) 
      (assert-equal nil (gr-nullable i))))
  (let ((*grammar* *gr2*))
    (loop for i from 0 to 1
	  do (assert-equal nil (gr-nullable i)))
    (loop for i from 2 to 4
	  do (assert-equal T (gr-nullable i)))
    (assert-equal nil (gr-nullable 5))))


(define-test gr-head
  (let ((*grammar* *gr1*))
    (assert-true (perm-equal (gr-head 1) '(1 2 3 4 6)))
    (assert-true (perm-equal (gr-head 2) '(2 3 4 6)))
    (assert-true (perm-equal (gr-head 3) '(3 4 6)))
    (loop for i from 4 to 8
	  do (assert-true (equal (gr-head i) (list i)))))
  (let ((*grammar* *gr2*)
	(heads '((0 5 1 2 3 4) (5 1 2 3 4) (2) (3) (4) (5))))
    (dotimes (i 6)
      (assert-true (perm-equal (gr-head i) (nth i heads)))))

  (let ((*grammar* *gr-example*))
    (assert-true (perm-equal (gr-head 0) '(0 5 15 2 8)))
    (assert-true (perm-equal (gr-head 1) '(6 9 1 5 15 2 8)))
    (assert-true (perm-equal (gr-head 2) '(5 15 2 8)))
    (loop for i from 3 to 20
	  do (assert-true (equal (gr-head i) (list i)))))
  )



;; 
;;  2. disambiguation
;; --------------------------------


