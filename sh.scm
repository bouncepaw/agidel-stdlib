(module
 agidel-plugin.sh
 (shebang! str set for-each-cli-arg _bracket)
 (import (prefix scheme -)
         (prefix chicken.base -)
         (prefix srfi-13 -)
         format)

 (-define (str lst)
          (-if (-list? lst)
               (-map (-lambda (x) (-if (-string? x)
                                       (-string-append "\"" x "\"")
                                       (-symbol->string x)))
                     lst)
               (-car (str (-list lst)))))

 (-define-syntax
  shebang!
  (syntax-rules ()
    ((_) "#!/bin/sh\n")
    ((_ path) (format "#!~A\n" path))))

 (-define-syntax
  set
  (syntax-rules (+ -)
    ((_ var val) (format "~A=~A\n" (str var) (str val)))
    ((_ var + val) (format "~A+=~A\n" (str var) (str val)))
    ((_ var - val) (format "~A-=~A\n" (str var) (str val)))))

 (-define-syntax
  for-each-cli-arg
  (syntax-rules ()
    ((_ var expr) (format "for ~A in \"$@\"; do\n~A\ndone\n"
                             var
                             expr))
    ((_ var exprs ...) (format "for ~A in \"$@\"; do\n~A\ndone\n"
                             var
                             (-apply -string-append (-list exprs ...))))))
 (-define-syntax
  _bracket
  (syntax-rules ()
    ((_ exprs ...) (format "~A\n" (-string-join (str (-list exprs ...)) " "))))))
