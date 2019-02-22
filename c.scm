(module
 agidel-plugin.c
 *
 (import (prefix scheme -)
         (prefix chicken.base -)
         (prefix chicken.string -)
         (prefix srfi-13 -)
         format)

 (-define (str lst)
          (-if (-list? lst)
               (-map (-lambda (x) (-if (-string? x)
                                       (-string-append "\"" x "\"")
                                       (-->string x)))
                     lst)
               (-car (str (-list lst)))))

 (-define (prefix->infix operator . operands)
          (format "(~A)" (-string-join operands operator (-quote infix))))

 (-define-syntax
  +
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to +"))
    ((_ o) (format "+~A" o))
    ((_ o ...) (-apply prefix->infix " + " (str (list o ...))))))

 (-define-syntax
  -
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to -"))
    ((_ o) (format "-(~A)" o))
    ((_ o ...) (-apply prefix->infix " - " (str (list o ...))))))

 (-define-syntax
  /
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to /"))
    ((_ o) (format "(1/~A)" o))
    ((_ o ...) (-apply prefix->infix " / " (str (list o ...))))))

 (-define-syntax
  *
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to *"))
    ((_ o) (format "*~A" o))
    ((_ o ...) (-apply prefix->infix " * " (str (list o ...))))))

 (-define-syntax
  %
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to %"))
    ((_ o) (-number->string (-exact->inexact (-/ o 100))))
    ((_ o ...) (-apply prefix->infix " % " (str (list o ...))))))

 (-define-syntax
  bitor
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to bitor"))
    ((_ o) (str o))
    ((_ o ...) (-apply prefix->infix " | " (str (list o ...))))))

 (-define-syntax
  bitand
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to bitand"))
    ((_ o) (str o))
    ((_ o ...) (-apply prefix->infix " & " (str (list o ...))))))

 (-define-syntax
  xor
  (syntax-rules ()
    ((_) (-error "Agidel: given no args to xor"))
    ((_ o) (str o))
    ((_ o ...) (-apply prefix->infix " ^ " (str (list o ...))))))
 )
