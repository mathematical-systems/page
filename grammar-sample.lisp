(in-package #:page)
(export '(*g1-f* *g-arith* *g-reg* *g-reg-tiger* *g-reg-tiger-inv* *input-g-reg-tiger-1* *input-g-reg-tiger-2* *g-arith* *g-if* *g-minus* *g1* *g2* *g3* *g4* *g5* *g6* *g7* *g8* *G9* *G10* *g11* *cbnf*))

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
		  ((S if b then S else S) ,#'(lambda (x y z w u v) (list 'S (list x) (list y) (list z) w (list u) v)) (20 . 15)
		  ((S statement) ,#'(lambda (x y) (list 'S (list 'statement))))))))

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

