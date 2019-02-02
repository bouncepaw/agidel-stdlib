#| This one is crazy. All things from other modules are prefixed with -.
|#
(module
 agidel-plugin.c
 (_agidel-arities
  + - * / % ; arithmetical operators
  inc inc* dec dec* ; -- ++
  )
 ;;(+ - / * % or and not bitor bitand compl left-shift right-shift
 ;;  set defvar defconst defun
 ;; _bracket _brace)
 (import (rename (prefix scheme -) (-define define) (-lambda lambda))
         (prefix (chicken base) -)
         (prefix (clojurian syntax) -)
         format)

 ;; By default, all the arguments get ævaled. You can override it here. Write q
 ;; to quote an arg, and e to æval it. Point represents rest-arg.
 (define _agidel-arities
   '((defvar . q) (defun q q . e) (struct e . q) (enum e . q) (union e . q)))

 (define (as-is x) x)
 
 (define (prefix->infix-operator operands operator default-value one-operandλ)
   (-cond
    [(-null? operands) default-value]
    [(= (-length operands) 1) (-apply one-operandλ operands)]
    [else (format "(~A)" (-string-join operands operator 'infix))]))

 ;;; Arithmetical operators
 (define (+ . os) (prefix->infix-operator os " + " "0" as-is))
 (define (- . os)
   (prefix->infix-operator os " - " "0"
                           (lambda (o) (format "-(~A)" o))))
 (define (* . os)
   (prefix->infix-operator os " * " "1"
                           (lambda (o) (format "*~A" o))))
 (define (/ . os) (prefix->infix-operator os " / " "1" as-is))
 (define (% . os) (prefix->infix-operator os " % " "1" as-is))

 ;;; Incrementors and decrementors
 (define (inc o) (format "++(~A)" o))
 (define (inc* o) (format "(~A)++" o))
 (define (dec o) (format "--(~A)" o))
 (define (dec* o) (format "(~A)--"))

 ;;; Logic operators
 (define (or . os) (prefix->infix-operator os " || " "true" as-is))
 (define (and . os) (prefix->infix-operator os " && " "false" as-is))
 (define (not o) (format "!(~A)" o))
 )
