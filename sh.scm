(module
 agidel-plugin.sh
 (set shift for-each-arg call echo)
 (import (prefix scheme -)
         (prefix chicken.base -)
         (prefix srfi-13 -)
         format)

 (-define (set var val)
   (format "~A=~A\n" var val))

 (-define (shift) "shift")

 (-define (for-each-arg varname . exprs)
          (format "for ~A in \"$@\"; do\n~A\ndone\n" (-string-join exprs "\n")))

 (-define (call . exprs)
          (-string-join exprs " "))

 (-define (echo . exprs)
          (apply call "echo" exprs)))
