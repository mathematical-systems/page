;; 
;;  PaGe-test: test scripts for page.lisp by lisp-unit
;; =================================================================


(defpackage #:page-test
  (:use #:common-lisp #:lisp-unit #:page)
  (:shadowing-import-from #:page
			  #:defparser
			  #:make-page-grammar
			  )
  )

(in-package #:page-test)

;; 
;;  0. Grammars for Testing
;; --------------------------------

;; page-grammars

;; (defparameter *gr1*
;;   (%make-page-grammar
;;    "E"
;;    '(("E" "E" "+" "T")
;;      ("E" "T")
;;      ("T" "T" "*" "F")
;;      ("T" "F")
;;      ("F" "(" "E" ")")
;;      ("F" "id"))))

;; (defparameter *gr-ex*
;;   (let ((l '(
;; 	     "Stat -> empty" 
;; 	     "Stat -> CondStat" 
;; 	     "CondStat -> if BoolExp then Stat"
;; 	     "CondStat -> if BoolExp then Stat else Stat"
;; 	     "BoolExp -> BoolExp || BoolExpAnd"
;; 	     "BoolExp -> BoolExpAnd"
;; 	     "BoolExpAnd -> BoolExpAnd && BoolExpUn"
;; 	     "BoolExpAnd -> BoolExpUn"
;; 	     "BoolExpUn -> ~~ BoolExpUn"
;; 	     "BoolExpUn -> BoolExpCond"
;; 	     "BoolExpCond -> ( BoolExp )"
;; 	     "BoolExpCond -> Exp == Exp"
;; 	     "BoolExpCond -> Exp < Exp"
;; 	     "BoolExpCond -> Exp > Exp"
;; 	     "Exp -> Exp + ExpMul"
;; 	     "Exp -> Exp - ExpMul"
;; 	     "Exp -> ExpMul"
;; 	     "ExpMul -> ExpMul * ExpUn"
;; 	     "ExpMul -> ExpMul / ExpUn"
;; 	     "ExpMul -> ExpUn"
;; 	     "ExpUn -> ( Exp )"
;; 	     "ExpUn -> id"
;; 	     )))
;;     (make-page-grammar
;;      "Stat"
;;      (mapcar
;;       #'(lambda (l) (remove "->" l :test #'string=))
;;       (mapcar
;;        #'(lambda (str) (pagen::split #'(lambda (x) (eq x #\Space)) str))
;;        l)))))

;; (defparameter *gr-ex-base*
;;   '(
;;     "Stat -> CondStat" 
;;     "CondStat -> if BoolExp then Stat"
;;     "CondStat -> if BoolExp then Stat else Stat"
;;     "BoolExp -> BoolExp || BoolExpAnd"
;;     "BoolExp -> BoolExpAnd"
;;     "BoolExpAnd -> BoolExpAnd && BoolExpUn"
;;     "BoolExpAnd -> BoolExpUn"
;;     "BoolExpUn -> ~~ BoolExpUn"
;;     "BoolExpUn -> BoolExpCond"
;;     "BoolExpCond -> BoolExp"
;;     "BoolExpCond -> Exp == Exp"
;;     "BoolExpCond -> Exp < Exp"
;;     "BoolExpCond -> Exp > Exp"
;;     "Exp -> Exp + ExpMul"
;;     "Exp -> Exp - ExpMul"
;;     "ExpMul -> ExpMul * ExpUn"
;;     "ExpMul -> ExpMul / ExpUn"
;;     "ExpUn -> ( Exp )"
;;     "ExpUn -> id"
;;     ))
   

;; (defparameter *gr-example*
;;   (make-page-grammar
;;    "E"
;;    '(("E" "E" "+" "E")
;;      ("E" "E" "*" "E")
;;      ("E" "E" "-" "E")
;;      ("E" "E" "/" "E")
;;      ("E" "(" "E" ")")
;;      ("E" "id")
;;      ("B" "E" "=" "E")
;;      ("B" "E" "<" "E")
;;      ("B" "E" ">" "E")
;;      ("B" "B" "and" "B")
;;      ("B" "B" "or" "B")
;;      ("B" "not" "B")
;;      ("B" "(" "B" ")")
;;      ("B" "bool")
;;      ("E" "if" "B" "then" "E")
;;      ("E" "if" "B" "then" "E" "else" "E")
;;      )))


;; 
;;  disambiguation
;; 

;; (let* ((pagen::*grammar* pagen-test::*gr-example*)
;; 	     (pa (pagen::disambiguator
;; 		  '((:left 19 18 17 16 11 10)
;; 		    (:shift 3)
;; 		    (:ord< 16 17 18 19)
;; 		    (:ord< 10 11)))))
;; 	(ex-parser "if id = id then if id > id then id else id" :dump t :rpa (car pa) :spa (cdr pa)))


(defvar input-example-list
  '(
    ((defparser test-pp
	(:grammar "E"
		  "E -> E + E"
		  "E -> E * E"
		  "E -> ( E )"
		  "E -> id")
	(:disambiguation
	 (:left "+" "*")
	 (:ord< "+" "*")))
     (test-pp "id + id" :dump t)
     (test-pp "id + id + id" :dump t))
    ((defparser test-pp
	(:grammar "S"
		  "S -> V <- E"
		  "S -> S ; S"
		  "S -> if E then S"
		  "S -> if E then S else S"
		  "E -> E + E"
		  "E -> E * E"
		  "E -> ( E )"
		  "E -> id")
	(:disambiguation
	 (:left "+" "*" ";")
	 (:ord< "+" "*")
	 (:shift "else")
	 ))
     (test-pp "id + id * id" :dump t)	; fail
     (test-pp "V <- id + id * id" :dump t)
     (test-pp "V <- id + id * id ; if id + id then V <- id else V <- id" :dump t))
    ((defparser test-pp
	(:grammar "S"
		  "S -> V <- E"
		  "S -> S ; S"
		  "S -> if E then S"
		  "S -> if E then S else S"
		  "E -> E + E"
		  "E -> E * E"
		  "E -> ( E )"
		  "E -> id")
	(:disambiguation
	 (:left "+" "*" ";")
	 (:ord< "+" "*")
	 ))				; no :shift
     (test-pp "V <- id + id * id ; if id + id then V <- id else V <- id" :dump t))
    ((defparser test-pp
	(:grammar "S"
		  "S -> { S }"
		  "S -> S ; S"
		  "S -> V <- E"
		  "S -> if B then S"
		  "S -> if B then S else S"
		  "E -> E + E"
		  "E -> E * E"
		  "E -> ( E )"
		  "E -> id"
		  "B -> V == V"
		  "B -> V < V"
		  "B -> V > V"
		  "B -> B || B"
		  "B -> B && B"
		  "B -> ~ B"
		  "B -> ( B )"
		  )
	(:disambiguation
	 (:left "+" "*" "||" "&&")
	 (:left ";")
	 (:ord< "+" "*")
	 (:ord< "||" "&&")
	 (:shift "else")
	 ))
     (test-pp
       (concatenate
	'string
	
	"V <- id + id * id ; "
	"if ~ V == V || V < V || V > V "
	  "then { V <- id ; "
                 "V <- ( id * id * id ) } "
  	  "else V <- id"
	)
       :dump t))
    ((defparser cbnf-parser 
	(:grammar-var cbnf-skeleton))
     (cbnf-parser
	 (concatenate
	  'string

	  "int * ( id ) const ; "
	  "{ int ( id ) ; "
	    "float ( id ) ; "
	    "if ( id == id ) "
	      "if ( id /= id ) { } "
	    "else id ; "
	  "}"
	  )
	 :dump t))
    ))

(defvar cbnf-skeleton
  (cons
   "translation_unit"
   (list
    "translation_unit -> external_decl"
    "translation_unit -> translation_unit external_decl"
    "external_decl -> function_definition" "external_decl -> decl"
    "function_definition -> decl_specs declarator decl_list compound_stat"
    "function_definition -> declarator decl_list compound_stat"
    "function_definition -> decl_specs declarator compound_stat"
    "function_definition -> declarator compound_stat"
    "decl -> decl_specs init_declarator_list ;"
    "decl -> decl_specs ;"
    "decl_list -> decl"
    "decl_list -> decl_list decl"
    "decl_specs -> storage_class_spec decl_specs"
    "decl_specs -> storage_class_spec"
    "decl_specs -> type_spec decl_specs"
    "decl_specs -> type_spec"
    "decl_specs -> type_qualifier decl_specs"
    "decl_specs -> type_qualifier"
    "storage_class_spec -> auto"
    "storage_class_spec -> register"
    "storage_class_spec -> static"
    "storage_class_spec -> extern"
    "storage_class_spec -> typedef"
    "type_spec -> void"
    "type_spec -> char"
    "type_spec -> short"
    "type_spec -> int"
    "type_spec -> long"
    "type_spec -> float"
    "type_spec -> double"
    "type_spec -> signed"
    "type_spec -> unsigned"
    "type_spec -> struct_or_union_spec"
    "type_spec -> enum_spec"
    "type_spec -> typedef_name"
    "type_qualifier -> const"
    "type_qualifier -> volatile"
    "struct_or_union_spec -> struct_or_union id { struct_decl_list }"
    "struct_or_union_spec -> struct_or_union { struct_decl_list }"
    "struct_or_union_spec -> struct_or_union id"
    "struct_or_union -> struct"
    "struct_or_union -> union"
    "struct_decl_list -> struct_decl"
    "struct_decl_list -> struct_decl_list struct_decl"
    "init_declarator_list -> init_declarator"
    "init_declarator_list -> init_declarator_list , init_declarator"
    "init_declarator -> declarator"
    "init_declarator -> declarator = initializer"
    "struct_decl -> spec_qualifier_list struct_declarator_list ;"
    "spec_qualifier_list -> type_spec spec_qualifier_list"
    "spec_qualifier_list -> type_spec"
    "spec_qualifier_list -> type_qualifier spec_qualifier_list"
    "spec_qualifier_list -> type_qualifier"
    "struct_declarator_list -> struct_declarator"
    "struct_declarator_list -> struct_declarator_list , struct_declarator"
    "struct_declarator -> declarator"
    "struct_declarator -> declarator : const_exp"
    "struct_declarator -> : const_exp"
    "enum_spec -> enum id { enumerator_list }"
    "enum_spec -> enum { enumerator_list }"
    "enum_spec -> enum id"
    "enumerator_list -> enumerator"
    "enumerator_list -> enumerator_list , enumerator"
    "enumerator -> id"
    "enumerator -> id = const_exp"
    "declarator -> pointer direct_declarator"
    "declarator -> direct_declarator"
    "direct_declarator -> id"
    "direct_declarator -> ( declarator )"
    "direct_declarator -> direct_declarator [ const_exp ]"
    "direct_declarator -> direct_declarator [ ]"
    "direct_declarator -> direct_declarator ( param_type_list )"
    "direct_declarator -> direct_declarator ( id_list )"
    "direct_declarator -> direct_declarator ( )"
    "pointer -> * type_qualifier_list"
    "pointer -> *"
    "pointer -> * type_qualifier_list pointer"
    "pointer -> * pointer"
    "type_qualifier_list -> type_qualifier"
    "type_qualifier_list -> type_qualifier_list type_qualifier"
    "param_type_list -> param_list"
    "param_type_list -> param_list , ..."
    "param_list -> param_decl"
    "param_list -> param_list , param_decl"
    "param_decl -> decl_specs declarator"
    "param_decl -> decl_specs abstract_declarator"
    "param_decl -> decl_specs"
    "id_list -> id"
    "id_list -> id_list , id"
    "initializer -> assignment_exp"
    "initializer -> { initializer_list }"
    "initializer -> { initializer_list , }"
    "initializer_list -> initializer"
    "initializer_list -> initializer_list , initializer"
    "type_name -> spec_qualifier_list abstract_declarator"
    "type_name -> spec_qualifier_list"
    "abstract_declarator -> pointer"
    "abstract_declarator -> pointer direct_abstract_declarator"
    "abstract_declarator -> direct_abstract_declarator"
    "direct_abstract_declarator -> ( abstract_declarator )"
    "direct_abstract_declarator -> direct_abstract_declarator [ const_exp ]"
    "direct_abstract_declarator -> [ const_exp ]"
    "direct_abstract_declarator -> direct_abstract_declarator [ ]"
    "direct_abstract_declarator -> [ ]"
    "direct_abstract_declarator -> direct_abstract_declarator ( param_type_list )"
    "direct_abstract_declarator -> ( param_type_list )"
    "direct_abstract_declarator -> direct_abstract_declarator ( )"
    "direct_abstract_declarator -> ( )"
    "typedef_name -> id"
    "stat -> labeled_stat"
    "stat -> exp_stat"
    "stat -> compound_stat"
    "stat -> selection_stat"
    "stat -> iteration_stat"
    "stat -> jump_stat"
    "labeled_stat -> id : stat"
    "labeled_stat -> case const_exp : stat"
    "labeled_stat -> default : stat"
    "exp_stat -> exp ;"
    "exp_stat -> ;"
    "compound_stat -> { decl_list stat_list }"
    "compound_stat -> { stat_list }"
    "compound_stat -> { decl_list }"
    "compound_stat -> { }"
    "stat_list -> stat"
    "stat_list -> stat_list stat"
    "selection_stat -> if ( exp ) stat"
    "selection_stat -> if ( exp ) stat else stat"
    "selection_stat -> switch ( exp ) stat"
    "iteration_stat -> while ( exp ) stat"
    "iteration_stat -> do stat while ( exp ) ;"
    "iteration_stat -> for ( exp ; exp ; exp ) stat"
    "iteration_stat -> for ( exp ; exp ; ) stat"
    "iteration_stat -> for ( exp ; ; exp ) stat"
    "iteration_stat -> for ( exp ; ; ) stat"
    "iteration_stat -> for ( ; exp ; exp ) stat"
    "iteration_stat -> for ( ; exp ; ) stat"
    "iteration_stat -> for ( ; ; exp ) stat"
    "iteration_stat -> for ( ; ; ) stat"
    "jump_stat -> goto id ;"
    "jump_stat -> continue ;"
    "jump_stat -> break ;"
    "jump_stat -> return exp ;"
    "jump_stat -> return ;"
    "exp -> assignment_exp"
    "exp -> exp , assignment_exp"
    "assignment_exp -> conditional_exp"
    "assignment_exp -> unary_exp assignment_operator assignment_exp"
    "assignment_operator -> ="
    "assignment_operator -> *="
    "assignment_operator -> /="
    "assignment_operator -> %="
    "assignment_operator -> +="
    "assignment_operator -> -="
    "assignment_operator -> <<="
    "assignment_operator -> >>="
    "assignment_operator -> &="
    "assignment_operator -> ^="
    "assignment_operator -> |="
    "conditional_exp -> logical_or_exp"
    "conditional_exp -> logical_or_exp ? exp : conditional_exp"
    "const_exp -> conditional_exp"
    "logical_or_exp -> logical_and_exp"
    "logical_or_exp -> logical_or_exp || logical_and_exp"
    "logical_and_exp -> inclusive_or_exp"
    "logical_and_exp -> logical_and_exp && inclusive_or_exp"
    "inclusive_or_exp -> exclusive_or_exp"
    "inclusive_or_exp -> inclusive_or_exp | exclusive_or_exp"
    "exclusive_or_exp -> and_exp"
    "exclusive_or_exp -> exclusive_or_exp ^ and_exp"
    "and_exp -> equality_exp"
    "and_exp -> and_exp & equality_exp"
    "equality_exp -> relational_exp"
    "equality_exp -> equality_exp == relational_exp"
    "equality_exp -> equality_exp != relational_exp"
    "relational_exp -> shift_expression"
    "relational_exp -> relational_exp < shift_expression"
    "relational_exp -> relational_exp > shift_expression"
    "relational_exp -> relational_exp <= shift_expression"
    "relational_exp -> relational_exp >= shift_expression"
    "shift_expression -> additive_exp"
    "shift_expression -> shift_expression << additive_exp"
    "shift_expression -> shift_expression >> additive_exp"
    "additive_exp -> mult_exp"
    "additive_exp -> additive_exp + mult_exp"
    "additive_exp -> additive_exp - mult_exp"
    "mult_exp -> cast_exp"
    "mult_exp -> mult_exp * cast_exp"
    "mult_exp -> mult_exp / cast_exp"
    "mult_exp -> mult_exp % cast_exp"
    "cast_exp -> unary_exp"
    "cast_exp -> ( type_name ) cast_exp"
    "unary_exp -> postfix_exp"
    "unary_exp -> ++ unary_exp"
    "unary_exp -> -- unary_exp"
    "unary_exp -> unary_operator cast_exp"
    "unary_exp -> sizeof unary_exp"
    "unary_exp -> sizeof ( type_name )"
    "unary_operator -> &"
    "unary_operator -> *"
    "unary_operator -> +"
    "unary_operator -> -"
    "unary_operator -> ~"
    "unary_operator -> !"
    "postfix_exp -> primary_exp"
    "postfix_exp -> postfix_exp [ exp ]"
    "postfix_exp -> postfix_exp ( argument_exp_list )"
    "postfix_exp -> postfix_exp ( )"
    "postfix_exp -> postfix_exp . id"
    "postfix_exp -> postfix_exp -> id"
    "postfix_exp -> postfix_exp ++"
    "postfix_exp -> postfix_exp --"
    "primary_exp -> id"
    "primary_exp -> constant"
    "primary_exp -> string"
    "primary_exp -> ( exp )"
    "argument_exp_list -> assignment_exp"
    "argument_exp_list -> argument_exp_list , assignment_exp"
    "constant -> int_const"
    "constant -> char_const"
    "constant -> float_const"
    "constant -> enumeration_const")))
