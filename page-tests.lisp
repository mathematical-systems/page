;; ================================
;;  Tests for PaGe
;; 

;; NOTE:
;;  This file will be separated into page-core-tests.lisp and page-tests.lisp

(defpackage #:page-tests
  (:use #:common-lisp #:lisp-unit #:page))

(in-package #:page-tests)

(define-test g1-parse
  (let* ((cg (canonicalize *g1*))
	 (parser (deremer-lalr1-parser cg))
	 (ast1 '("$accept"
		  ("E" ("E" ("T" ("F" ("id")))) ("+") ("T" ("F" ("id"))))
		  ("$eof")))
	 (ast2 '("$accept"
		 ("E" ("E" ("T" ("F" ("id")))) ("+")
		  ("T" ("T" ("F" ("id"))) ("*") ("F" ("id"))))
		 ("$eof")))
	 (ast3 '("$accept"
		 ("E"
		  ("T"
		   ("T"
		    ("F" ("(") ("E" ("E" ("T" ("F" ("id")))) ("+") ("T" ("F" ("id"))))
		     (")")))
		   ("*") ("F" ("id"))))
		 ("$eof")))
	 (ast4 '("$accept"
		 ("E" ("T" ("F" ("id"))))
		 ("$eof"))))
    (assert-equal ast1 (parse parser '("id" "+" "id")))
    (assert-equal ast2 (parse parser '("id" "+" "id" "*" "id")))
    (assert-equal ast3 (parse parser '("(" "id" "+" "id" ")" "*" "id")))
    (assert-equal ast4 (parse parser '("id")))
    (assert-true (stringp (parse parser '())))))


  
