;; ================================
;;  Tests for PaGe
;; 

;; NOTE:
;;  This file will be separated into page-core-tests.lisp and page-tests.lisp

(defpackage #:page-tests
  (:use #:common-lisp #:lisp-unit #:page))

(in-package #:page-tests)
;; (import 'page::cg-parse)

(define-test g1-parse
  (let* ((cg (canonicalize *g1*))
	 (parser (deremer-lalr1-parser))
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
    (setf (symbol-function 'reader-of) #'(lambda (l) (page::simple-reader l :eof "$eof")))
    (assert-equal ast1 (cg-parse parser (reader-of '("id" "+" "id"))))
    (assert-equal ast2 (cg-parse parser (reader-of '("id" "+" "id" "*" "id"))))
    (assert-equal ast3 (cg-parse parser (reader-of '("(" "id" "+" "id" ")" "*" "id"))))
    (assert-equal ast4 (cg-parse parser (reader-of '("id"))))
    (assert-true (stringp (cg-parse parser (reader-of '()))))))


  
