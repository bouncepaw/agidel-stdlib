#| This one is crazy. All things from other modules are prefixed with -.
|#
(module
 agidel-plugin.c
 (_agidel-arity
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

 ;; Special alist with arities of functions exported. If function accepts any
 ;; number of arguments, do not list it here.
 (define _agidel-arity
   '((inc . 1) (inc* . 1) (dec . 1) (dec* . 1)
     (left-shift . 2) (right-shift . 2)
     (not . 1) (compl . 1)
     (if . 3) (unless . 3) (if* . 3) (unless . 3)
     (get . 2) (ptr . 1) (ref . 1) 
     (break . 0) (continue . 0) (return . 1) (goto . 1)
     (size-of . 1) (align-of . 1) (type-case . 2)))

 (define (as-is x) x)
 
 (define (prefix->infix-operator operands operator default-value one-operandλ)
   (-cond
    [(-null? operands) default-value]
    [(= (-length operands) 1) (-apply one-operandλ operands)]
    [else (format "(~A)" (-string-join operands operator 'infix))]))

 ;;; Arithmetical operators
 (define (+ . operands)
   (prefix->infix-operator operands " + " "0" as-is))
 (define (- . operands)
   (prefix->infix-operator operands " - " "0"
                           (lambda (o) (format "-(~A)" o))))
 (define (* . operands)
   (prefix->infix-operator operands " * " "1"
                           (lambda (o) (format "*~A" o))))
 (define (/ . operands)
   (prefix->infix-operator operands " / " "1" as-is))
 (define (% . operands)
   (prefix->infix-operator operands " % " "1" as-is))

 ;;; n++ n-- --n ++n
 (define (inc o) (format "++(~A)" o))
 (define (inc* o) (format "(~A)++" o))
 (define (dec o) (format "--(~A)" o))
 (define (dec* o) (format "(~A)--"))
 )
