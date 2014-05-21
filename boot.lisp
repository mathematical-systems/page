(progn
  (compile-file "page-core.lisp")
  (load "page-core.fasl")
  (compile-file "page.lisp")
  (load "page.fasl")
  (compile-file "grammar-sample.lisp")
  (load "grammar-sample.fasl")
  (compile-file "lisp-unit.lisp")
  (load "lisp-unit.fasl")
  (compile-file "page-tests.lisp")
  (load "page-tests.fasl"))
  


