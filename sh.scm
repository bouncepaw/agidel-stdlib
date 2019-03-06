(module
 agidel-plugin.sh
 (shebang! str set for-each-cli-arg _bracket)
 (import (rename (prefix scheme -) (-define define)
                 (-define-syntax define-syntax))
         (prefix chicken.base -)
         (prefix srfi-13 -)
         matchable
         format)

 (define (str lst)
   (-if (-list? lst)
        (-map (-lambda (x) (-if (-string? x)
                                (-string-append "\"" x "\"")
                                (-symbol->string x)))
              lst)
        (-car (str (-list lst)))))

 (define shebang!
   (match-lambda*
    (() "#!/bin/sh\n")
    ((path (format "#!~A\n" path)))))

 (define set
   (match-lambda*
    ((var val) (format "~A=~A\n" (str var) (str val)))
    ((var oper val) (format "~A~A=~A\n" (str var) (str oper) (str val)))))

 (define (for-each-cli-arg var . exprs)
   (format "for ~A in \"$@\"; do\n~A\ndone\n"
           var
           (-apply -string-append exprs)))

 (define (_bracket . exprs)
   (format "~A\n" (-string-join (str exprs)))))
