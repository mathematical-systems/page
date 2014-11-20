(progn
  (compile-file "pagen.lisp")
  (load "pagen.fasl")
  (compile-file "page.lisp")
  (load "page.fasl")
  (compile-file "lisp-unit.lisp")
  (load "lisp-unit.fasl")
  (compile-file "pagen-test.lisp")
  (load "pagen-test.fasl")
  (compile-file "page-test.lisp")
  (load "page-test.fasl")
  )
  


